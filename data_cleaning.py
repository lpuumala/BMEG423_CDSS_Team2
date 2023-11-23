# %%
import os, glob, sys
import numpy as np
import pandas as pd
## Load all the data so we can quickly combine it and explore it. 
pfile = 'CinC.pickle'
pfile_test = 'CinC_test.pickle'
if os.path.isfile(pfile):
  CINCdat = pd.read_pickle(pfile)
else:
  os.chdir("training_2023-11-05")
  extension = 'csv'
  all_filenames = [i for i in glob.glob('*.{}'.format(extension))]
  CINCdat = pd.concat([pd.read_csv(f).assign(patient=os.path.basename(f).split('.')[0]) for f in all_filenames ])
  os.chdir(os.path.dirname(__file__))
  CINCdat.to_pickle(pfile)
print(len(CINCdat)) # should be n=196585

if os.path.isfile(pfile_test):
  CINCdat_test = pd.read_pickle(pfile_test)
else:
  os.chdir("../testing_2023-11-05")
  extension = 'csv'
  all_filenames = [i for i in glob.glob('*.{}'.format(extension))]
  CINCdat_test = pd.concat([pd.read_csv(f).assign(patient=os.path.basename(f).split('.')[0]) for f in all_filenames ])
  os.chdir(os.path.dirname(__file__))
  CINCdat_test.to_pickle(pfile_test)
print(len(CINCdat_test)) # should be n=40566

## Set ranges for feasible vals - set vals as NaN if out of range
CINCdat.loc[~CINCdat["Resp"].between(5, 60), "Resp"] = None
CINCdat_test.loc[~CINCdat_test["Resp"].between(5, 60), "Resp"] = None
CINCdat.loc[~CINCdat["HR"].between(10, 300), "HR"] = None
CINCdat_test.loc[~CINCdat_test["HR"].between(10, 300), "HR"] = None
CINCdat.loc[~CINCdat["MAP"].between(0, 300), "MAP"] = None
CINCdat_test.loc[~CINCdat_test["MAP"].between(0, 300), "MAP"] = None
CINCdat.loc[~CINCdat["SBP"].between(40, 280), "SBP"] = None
CINCdat_test.loc[~CINCdat_test["SBP"].between(40, 280), "SBP"] = None
CINCdat.loc[~CINCdat["DBP"].between(20, 130), "DBP"] = None
CINCdat_test.loc[~CINCdat_test["DBP"].between(20, 130), "DBP"] = None
CINCdat.loc[~CINCdat["O2Sat"].between(60, 100), "O2Sat"] = None
CINCdat_test.loc[~CINCdat_test["O2Sat"].between(60, 100), "O2Sat"] = None
CINCdat.loc[~CINCdat["Temp"].between(32, 42.2), "Temp"] = None
CINCdat_test.loc[~CINCdat_test["Temp"].between(32, 42.2), "Temp"] = None
## Forward-fill missing values
CINCdat.update(CINCdat.groupby('patient').ffill())
CINCdat_test.update(CINCdat_test.groupby('patient').ffill())

## Back-fill missing values (aka. beginning of patient's stay)
CINCdat.update(CINCdat.groupby('patient').bfill())
CINCdat_test.update(CINCdat_test.groupby('patient').bfill())

## calculate shock-index
CINCdat_test["shock"] = CINCdat_test["HR"]/CINCdat_test["MAP"]
CINCdat["shock"] = CINCdat["HR"]/CINCdat["MAP"]
## remove correlated columns: MAP, HR, DBP, SBP (according to research)
CINCdat_test = CINCdat_test.drop(columns = ['HR', 'MAP', 'DBP', 'SBP'])
CINCdat = CINCdat.drop(columns = ['HR', 'MAP', 'DBP', 'SBP'])
## remove other unnecessary columns (as per Julia's guidance)
CINCdat_test = CINCdat_test.drop(columns = ['Hct', 'Hgb', 'Magnesium', 'Potassium', 'Glucose', 'Creatinine'])
CINCdat = CINCdat.drop(columns = ['Hct', 'Hgb', 'Magnesium', 'Potassium', 'Glucose', 'Creatinine'])
## calculate differencess of some columns
CINCdat_test["WBC_grad"] = CINCdat_test.groupby('patient')['WBC'].diff()
CINCdat["WBC_grad"] = CINCdat.groupby('patient')['WBC'].diff()
CINCdat_test["shock_grad"] = CINCdat_test.groupby('patient')['shock'].diff()
CINCdat["shock_grad"] = CINCdat.groupby('patient')['shock'].diff()
CINCdat_test["Temp_grad"] = CINCdat_test.groupby('patient')['Temp'].diff()
CINCdat["Temp_grad"] = CINCdat.groupby('patient')['Temp'].diff()
CINCdat_test["Temp_resp"] = CINCdat_test.groupby('patient')['Resp'].diff()
CINCdat["Temp_resp"] = CINCdat.groupby('patient')['Resp'].diff()
## calculate deviations of some columns -  may not work properly
CINCdat_NOsepsis = CINCdat[~CINCdat.patient.isin(np.unique(CINCdat.patient[CINCdat.SepsisLabel==1]))]
CINCdat_NOsepsis = CINCdat_NOsepsis[CINCdat_NOsepsis.ICULOS>1]
meanWBC = round(CINCdat_NOsepsis['WBC'].mean(),2)
CINCdat_test['WBC_dev'] = CINCdat_test['WBC'] - meanWBC
CINCdat['WBC_dev'] = CINCdat['WBC'] - meanWBC
meanResp = round(CINCdat_NOsepsis['Resp'].mean(),2)
CINCdat_test['Resp_dev'] = CINCdat_test['Resp'] - meanResp
CINCdat['Resp_dev'] = CINCdat['Resp'] - meanResp
meanShock = round(CINCdat_NOsepsis['shock'].mean(),2)
CINCdat_test['shock_dev'] = CINCdat_test['WBC'] - meanShock
CINCdat['shock_dev'] = CINCdat['shock'] - meanShock
meanTemp = round(CINCdat_NOsepsis['Temp'].mean(),2)
CINCdat_test['Temp_dev'] = CINCdat_test['Temp'] - meanTemp
CINCdat['Temp_dev'] = CINCdat['Temp'] - meanTemp

print(CINCdat_test.head(5))
print(CINCdat_test.iloc[10004])

#%%
## Get reference ranges for variables using only non-sepsis patients as 'normal'
CINCdat_NOsepsis = CINCdat[~CINCdat.patient.isin(np.unique(CINCdat.patient[CINCdat.SepsisLabel==1]))]
CINCdat_NOsepsis = CINCdat_NOsepsis[CINCdat_NOsepsis.ICULOS>1]
CINCdat_NOsepsis.drop(['patient','SepsisLabel','Sex'],axis=1,inplace=True)
meanCINCdat = round(CINCdat_NOsepsis.mean(axis=0),2)
sdCINCdat = round(CINCdat_NOsepsis.std(axis=0),2)
np.set_printoptions(suppress=True)
print('x_mean = np.array(')
print(np.array(meanCINCdat),')')
print('x_std = np.array(')
print(np.array(sdCINCdat),')')

## Obtain the z-scores for all the variables
CINCdat_zScores = CINCdat
CINCdat_test_zScores = CINCdat_test
cols = CINCdat_zScores.columns.drop(['patient','SepsisLabel','Sex'])
for c in cols:
  CINCdat_zScores[c] = (CINCdat_zScores[c]-meanCINCdat[c])/sdCINCdat[c]
  CINCdat_test_zScores[c] = (CINCdat_test_zScores[c]-meanCINCdat[c])/sdCINCdat[c]

## Replace values still missing with the mean
CINCdat_zScores = CINCdat_zScores.fillna(0)
CINCdat_test_zScores = CINCdat_test_zScores.fillna(0)
