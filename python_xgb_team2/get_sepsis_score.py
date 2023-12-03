#!/usr/bin/env python

import os, glob, sys
import numpy as np
import pandas as pd
import xgboost as xgb

def get_sepsis_score(CINCdat, model):
    thresh = 0.032

    ## Things to incorporate into get_sepsis_score fuction:
    # Convert to dataframe
    CINCdat = pd.DataFrame(CINCdat, columns= ['ICULOS', 'HR', 'O2Sat', 'Temp', 'SBP', 'MAP', 'DBP', 'Resp',
                                               'FiO2', 'pH', 'BUN', 'Creatinine', 'Glucose', 'Magnesium',
                                                'Potassium', 'Hct', 'Hgb', 'WBC', 'Age', 'Sex'])
    ## Set ranges for feasible vals - set vals as NaN if out of range
    CINCdat.loc[~CINCdat["Resp"].between(5, 60), "Resp"] = None
    CINCdat.loc[~CINCdat["HR"].between(10, 300), "HR"] = None
    CINCdat.loc[~CINCdat["MAP"].between(0, 300), "MAP"] = None
    CINCdat.loc[~CINCdat["SBP"].between(40, 280), "SBP"] = None
    CINCdat.loc[~CINCdat["DBP"].between(20, 130), "DBP"] = None
    CINCdat.loc[~CINCdat["O2Sat"].between(60, 100), "O2Sat"] = None
    CINCdat.loc[~CINCdat["Temp"].between(32, 42.2), "Temp"] = None
    # Back-fill missing values (aka. beginning of patient's stay)
    CINCdat.bfill()

    ## calculate shock-index
    CINCdat["shock"] = CINCdat["HR"]/CINCdat["MAP"]

    ## remove other unnecessary columns (as per Julia's guidance)
    CINCdat = CINCdat.drop(columns = [ 'MAP', 'DBP'])
    CINCdat = CINCdat.drop(columns = ['Hct', 'Hgb', 'Magnesium', 'Potassium', 'Glucose'])
    # Calculates the first derivative/slope
    if len(CINCdat)>1:
        CINCdat["WBC_grad"] = pd.Series(np.gradient(CINCdat["WBC"], CINCdat.index))
        CINCdat["shock_grad"] = pd.Series(np.gradient(CINCdat["shock"], CINCdat.index))
        CINCdat["Temp_grad"] = pd.Series(np.gradient(CINCdat["Temp"], CINCdat.index))
        CINCdat["Resp_grad"] = pd.Series(np.gradient(CINCdat["Resp"], CINCdat.index))
    else:
        CINCdat["WBC_grad"] = 0
        CINCdat["shock_grad"] = 0
        CINCdat["Temp_grad"] = 0
        CINCdat["Resp_grad"] = 0

    ## Deviation from baseline (first observation after ICU admission)
    CINCdat["Baseline_WBC"] = CINCdat.groupby(level = 0)["WBC"].first()
    CINCdat['WBC_dev'] = CINCdat['WBC'] - CINCdat['Baseline_WBC']
    CINCdat["Baseline_Resp"] = CINCdat.groupby(level = 0)["Resp"].first()
    CINCdat['Resp_dev'] = CINCdat['Resp'] - CINCdat['Baseline_Resp']
    CINCdat["Baseline_shock"] = CINCdat.groupby(level = 0)["shock"].first()
    CINCdat['shock_dev'] = CINCdat['shock'] - CINCdat['Baseline_shock']
    CINCdat["Baseline_Temp"] = CINCdat.groupby(level = 0)["Temp"].first()
    CINCdat['Temp_dev'] = CINCdat['Temp'] - CINCdat['Baseline_Temp']

    # Drop baseline columns
    CINCdat = CINCdat.drop(columns = ['Baseline_WBC', 'Baseline_Resp', 'Baseline_shock', 'Baseline_Temp'])

    ## Implement early detection scoring
    # EWSP is the early warning score point
    CINCdat = get_early_warning_score(CINCdat)

    x_mean = np.array(
    [ 23.77,  85.34,  97.14,  36.86, 122.41,  18.86,   0.52,   7.39,  23.49,   1.55,
    11.2,   62.72,   1.08, 0.58,   0.12,   0.,  0.03,   0.06,  -0.06,   1.65, -0.93
    , -0.5,    0.96] )
    x_std = np.array(
    [20.97, 16.83,  2.88,  0.72, 23.02,  5.02,  0.22,  0.07, 18.62,  1.85,  8.53, 15.98, 0.49, 
     0.29,  0.44,  0.62,  0.16,  1.39,  9.47,  5.41, 1.44,  0.86,  0.35] )

    # print(CINCdat.columns.to_list())
    ## Obtain the z-scores for all the variables
    # CINCdat.drop(columns=['Sex'])
    CINCdat_zScores = CINCdat
    CINCdat_zScores = (CINCdat-x_mean)/x_std
    # for c in cols:
    #     CINCdat_zScores[c] = (CINCdat_zScores[c]-x_mean[c])/x_std[c]

    ## Replace values still missing with the mean
    CINCdat_zScores = CINCdat_zScores.fillna(0)

    score=model.predict(xgb.DMatrix(CINCdat_zScores.iloc[:,0:23]))
    score=min(max(score),1)
    label = score >= thresh

    return score, label

## Function that outputs the EWSP for a given data set
import math
import numpy as np
import pandas as pd

def get_early_warning_score(data):
   Resp = data['Resp']
   O2Sat = data['O2Sat']
   HR = data['HR']
   SBP = data['SBP']
   Temp = data['Temp']

   conditions_score_3 = [
      (Resp<8) | (O2Sat<=94) | (HR <= 40) | (SBP<=90) | (Temp<=35)
      , (Resp>=25) | (HR >= 130) | (SBP>=220)
   ]
   choices_score_3 = [
      3
      , 3
   ]

   conditions_score_2 = [
      (O2Sat>=92) | (O2Sat<94) | (SBP>90) | (SBP<101)
      , (Resp>=21) | (Resp<25) | (HR >= 110) | (HR < 130) & (Temp>=39)
   ]
   choices_score_2 = [
      2
      , 2
   ]

   conditions_score_1 = [
      (Resp>=8) | (Resp<12) | (O2Sat>=94) | (O2Sat<96) | (HR > 40) | (HR <= 50) | (SBP>=101) | (SBP<111) | (Temp>35) | (Temp<=36)
      , (HR>=91) | (HR<=110) | (Temp>38) | (Temp<39)
   ]
   choices_score_1 = [
      1
      , 1
   ]

   conditions_score_0 = [
      (Resp>=12) | (Resp<21) | (O2Sat>=96) | (HR >= 51) | (HR <= 90) | (SBP>=111) | (SBP<220) | (Temp>36.0) | (Temp<=38)
   ]
   choices_score_0 = [
      0
   ]
   
   data['point_3'] = np.select(conditions_score_3, choices_score_3, default=0)
   data['point_2'] = np.select(conditions_score_2, choices_score_2, default=0)
   data['point_1'] = np.select(conditions_score_1, choices_score_1, default=0)
   data['point_0'] = np.select(conditions_score_0, choices_score_0, default=0)
      
   data["EWSP"] = data[['point_3', 'point_2', 'point_1', 'point_0']].mean(axis=1)

   data = data.drop(columns = ['point_3', 'point_2', 'point_1', 'point_0'])
   
   return data
    
