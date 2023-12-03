#!/usr/bin/env python

import numpy as np
import pandas as pd
import xgboost as xgb

def get_sepsis_score(data, model):
    x_mean = np.array(
        [ 23.77,  97.13,  36.85, 122.19,  18.86,   0.49,  23.25,   1.55, 10.37, 11.31,
         62.72,   1.08, 0] )
    x_std = np.array(
        [20.97 , 2.91,  0.71, 23.03,  5.03,  0.21, 18.91,  1.94,  1.97,  9.24,
         15.98,  0.29, 1] )

    # Preparing a pd dataframe of only the last row for data parsing
    x_prep = pd.DataFrame(data, columns=['ICULOS', 'HR', 'O2Sat', 'Temp', 'SBP', 'MAP', 'DBP', 'Resp', 'FiO2',
       'pH', 'BUN', 'Creatinine', 'Glucose', 'Magnesium', 'Potassium', 'Hct',
       'Hgb', 'WBC', 'Age', 'Sex'])
    x = pd.DataFrame(x_prep.iloc[-1]).T
    
    # Set ranges for feasible vals - set vals as NaN if out of range
    x.loc[~x["Resp"].between(5, 60), "Resp"] = None
    x.loc[~x["HR"].between(10, 300), "HR"] = None
    x.loc[~x["MAP"].between(0, 300), "MAP"] = None
    x.loc[~x["SBP"].between(40, 280), "SBP"] = None
    x.loc[~x["DBP"].between(20, 130), "DBP"] = None
    x.loc[~x["O2Sat"].between(60, 100), "O2Sat"] = None
    x.loc[~x["Temp"].between(32, 42.2), "Temp"] = None
    
    # Selecting which features to use
    x["shock"] = x["HR"]/x["MAP"]
    x = x.drop(columns = ['Hct', 'Magnesium', 'Potassium', 'Glucose','pH','HR', 'MAP', 'DBP'])
               
    # Moving 'Sex' to the back
    x['Sex'] = x.pop('Sex')
               
    # Creating the normalized dataframe
    cols = 13
    x = abs((x-x_mean)/x_std)
    x = x.fillna(0)
    x_norm_dm = xgb.DMatrix(x.iloc[:,0:cols])

    x = x.assign(probSepsisXGB=model.predict(x_norm_dm))
    
    
    score = x.iloc[-1, -1]
    label = score >= 0.0312
    return score, label

def load_sepsis_model():    
    model = xgb.Booster({'nthread': 2})
    model.load_model("xgboost.model")
    
    return model






