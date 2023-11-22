#!/usr/bin/env python
import os, glob, sys
import numpy as np
import pandas as pd

## Load all the data so we can quickly combine it and explore it. 
pfile = 'CinC.pickle'
pfile_test = 'CinC_test.pickle'
retval = os.getcwd()
print("Current working directory %s" % retval)

if os.path.isfile(pfile):
  CINCdat = pd.read_pickle(pfile)
else:
  os.chdir("../training_2023-11-05")
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

## Forward-fill missing values
CINCdat.update(CINCdat.groupby('patient').ffill())
CINCdat_test.update(CINCdat_test.groupby('patient').ffill())

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

# #### OPTION 1: LOGISTIC REGRESSION ####
# ## Build a logistic regression using all the training data
# from sklearn.linear_model import LogisticRegression
# lreg = LogisticRegression(random_state=0)
# lreg.fit(CINCdat_zScores.iloc[:,0:20],CINCdat_zScores.SepsisLabel)
# print('const=',np.round(lreg.intercept_,4))
# print('coeffs = np.array(')
# print(np.round(lreg.coef_,4),')')

# ## Add the predictions
# CINCdat_zScores = CINCdat_zScores.assign(probSepsisLR=lreg.predict_proba(CINCdat_zScores.iloc[:,0:20])[::,1])
# CINCdat_test_zScores = CINCdat_test_zScores.assign(probSepsisLR=lreg.predict_proba(CINCdat_test_zScores.iloc[:,0:20])[::,1])
# print(CINCdat_zScores)

# ## Quick but not necessarily great way to find a threshold. Also calculate the AUC
# from sklearn.metrics import roc_curve, roc_auc_score
# fpr, tpr, thresholds = roc_curve(CINCdat_zScores.SepsisLabel,CINCdat_zScores.probSepsisLR)
# print('AUC:',round(roc_auc_score(CINCdat_zScores.SepsisLabel,CINCdat_zScores.probSepsisLR),2))
# print('AUC_test:',round(roc_auc_score(CINCdat_test_zScores.SepsisLabel,CINCdat_test_zScores.probSepsisLR),2))
# thresh=round(thresholds[np.argmax(tpr - fpr)],4)
# print('Threshold:',thresh)

# # Quick calculation of utility score
# CINCdat = CINCdat.assign(SepsisLabelLR = (CINCdat_zScores.probSepsisLR>thresh).astype(int))
# CINCdat_test= CINCdat_test.assign(SepsisLabelLR = (CINCdat_test_zScores.probSepsisLR>thresh).astype(int))

# import evaluate_sepsis_score as ev
# util = ev.evaluate_utility(CINCdat.patient,np.array(CINCdat_zScores.SepsisLabel),np.array(CINCdat.SepsisLabelLR))
# print(util)
# util_test = ev.evaluate_utility(CINCdat_test.patient,np.array(CINCdat_test_zScores.SepsisLabel),np.array(CINCdat_test.SepsisLabelLR))
# print(util_test)

# #### OPTION 2: BOOSTED TREE #####
# ## Build a LightGBM model using all the training data
# import lightgbm as lgb
# train_data = lgb.Dataset(data=CINCdat_zScores.iloc[:,0:20], label=CINCdat_zScores.SepsisLabel)
# param = {'objective': 'binary'}
# bst = lgb.train(param, train_data, 10)

# ## Add the predictions
# CINCdat_zScores = CINCdat_zScores.assign(probSepsisGBM=bst.predict(data=CINCdat_zScores.iloc[:,0:20]))
# CINCdat_test_zScores = CINCdat_test_zScores.assign(probSepsisGBM=bst.predict(data=CINCdat_test_zScores.iloc[:,0:20]))

# ## Quick but not necessarily great way to find a threshold. Also calculate the AUC
# from sklearn.metrics import roc_curve, roc_auc_score
# fpr, tpr, thresholds = roc_curve(CINCdat_zScores.SepsisLabel,CINCdat_zScores.probSepsisGBM)
# print('AUC:',round(roc_auc_score(CINCdat_zScores.SepsisLabel,CINCdat_zScores.probSepsisGBM),2))
# print('AUC_test:',round(roc_auc_score(CINCdat_test_zScores.SepsisLabel,CINCdat_test_zScores.probSepsisGBM),2))

# # Save the model and get the threshold for use as a model. This is Youden's J statistic
# bst.save_model('lightgbm.model')
# thresh=round(thresholds[np.argmax(tpr - fpr)],4)
# print('Threshold:',thresh)

# # Quick calculation of utility score
# CINCdat = CINCdat.assign(SepsisLabelGBM = (CINCdat_zScores.probSepsisGBM>thresh).astype(int))
# CINCdat_test= CINCdat_test.assign(SepsisLabelGBM = (CINCdat_test_zScores.probSepsisGBM>thresh).astype(int))

# import evaluate_sepsis_score as ev
# util = ev.evaluate_utility(CINCdat.patient,np.array(CINCdat_zScores.SepsisLabel),np.array(CINCdat.SepsisLabelGBM))
# print('Training Utility:', util)
# util_test = ev.evaluate_utility(CINCdat_test.patient,np.array(CINCdat_test_zScores.SepsisLabel),np.array(CINCdat_test.SepsisLabelGBM))
# print('Testing Utility:', util_test)


# ## OPTION 3: KNN Classifier ##
# from sklearn.neighbors import KNeighborsClassifier
# model = KNeighborsClassifier(n_neighbors=5)
# model.fit(CINCdat_zScores.iloc[:,0:20],CINCdat_zScores.SepsisLabel)

# ## Add the predictions
# CINCdat_zScores = CINCdat_zScores.assign(probSepsisKNN=model.predict(CINCdat_zScores.iloc[:,0:20]))
# CINCdat_test_zScores = CINCdat_test_zScores.assign(probSepsisKNN=model.predict(CINCdat_test_zScores.iloc[:,0:20]))

# ## Quick but not necessarily great way to find a threshold. Also calculate the AUC
# from sklearn.metrics import roc_curve, roc_auc_score
# fpr, tpr, thresholds = roc_curve(CINCdat_zScores.SepsisLabel,CINCdat_zScores.probSepsisKNN)
# print('AUC:',round(roc_auc_score(CINCdat_zScores.SepsisLabel,CINCdat_zScores.probSepsisKNN),2))
# print('AUC_test:',round(roc_auc_score(CINCdat_test_zScores.SepsisLabel,CINCdat_test_zScores.probSepsisKNN),2))
# thresh=round(thresholds[np.argmax(tpr - fpr)],4)
# print('Threshold:',thresh)

# # Quick calculation of utility score
# CINCdat = CINCdat.assign(SepsisLabelKNN = (CINCdat_zScores.probSepsisKNN>thresh).astype(int))
# CINCdat_test= CINCdat_test.assign(SepsisLabelKNN = (CINCdat_test_zScores.probSepsisKNN>thresh).astype(int))

# import evaluate_sepsis_score as ev
# util = ev.evaluate_utility(CINCdat.patient,np.array(CINCdat_zScores.SepsisLabel),np.array(CINCdat.SepsisLabelKNN))
# print(util)
# util_test = ev.evaluate_utility(CINCdat_test.patient,np.array(CINCdat_test_zScores.SepsisLabel),np.array(CINCdat_test.SepsisLabelKNN))
# print(util_test)

# exit()

## OPTION 4: Support Vector Machine ##

from sklearn import svm
clf = svm.LinearSVC(max_iter=3000)
clf.fit(CINCdat_zScores.iloc[:,0:20],CINCdat_zScores.SepsisLabel)

## Add the predictions
CINCdat_zScores = CINCdat_zScores.assign(probSepsisSVM=clf.predict(CINCdat_zScores.iloc[:,0:20]))
CINCdat_test_zScores = CINCdat_test_zScores.assign(probSepsisSVM=clf.predict(CINCdat_test_zScores.iloc[:,0:20]))

## Quick but not necessarily great way to find a threshold. Also calculate the AUC
from sklearn.metrics import roc_curve, roc_auc_score
fpr, tpr, thresholds = roc_curve(CINCdat_zScores.SepsisLabel,CINCdat_zScores.probSepsisSVM)
print('AUC:',round(roc_auc_score(CINCdat_zScores.SepsisLabel,CINCdat_zScores.probSepsisSVM),2))
print('AUC_test:',round(roc_auc_score(CINCdat_test_zScores.SepsisLabel,CINCdat_test_zScores.probSepsisSVM),2))
thresh=round(thresholds[np.argmax(tpr - fpr)],4)
print('Threshold:',thresh)

# Quick calculation of utility score
CINCdat = CINCdat.assign(SepsisLabelSVM = (CINCdat_zScores.probSepsisSVM>thresh).astype(int))
CINCdat_test= CINCdat_test.assign(SepsisLabelSVM = (CINCdat_test_zScores.probSepsisSVM>thresh).astype(int))

import evaluate_sepsis_score as ev
util = ev.evaluate_utility(CINCdat.patient,np.array(CINCdat_zScores.SepsisLabel),np.array(CINCdat.SepsisLabelSVM))
print(util)
util_test = ev.evaluate_utility(CINCdat_test.patient,np.array(CINCdat_test_zScores.SepsisLabel),np.array(CINCdat_test.SepsisLabelSVM))
print(util_test)
exit()

#### OPTION 5: XGBoost Model ####
from sklearn.model_selection import train_test_split
import xgboost as xgb
dtrain = xgb.DMatrix(data = CINCdat_zScores.iloc[:,0:20], label=CINCdat_zScores.SepsisLabel)
param = {'objective':'binary:logistic' }
xgb_model = xgb.train(param,dtrain, 10)
dtest= xgb.DMatrix(CINCdat_zScores.iloc[:,0:20])
ypred = xgb_model.predict(dtest)

#Measuring accuracy on Testing Data
from sklearn import metrics
#Add the predictions
CINCdat_zScores = CINCdat_zScores.assign(probSepsisXGB=xgb_model.predict(xgb.DMatrix(CINCdat_zScores.iloc[:,0:20])))
print(CINCdat_zScores)
CINCdat_test_zScores = CINCdat_test_zScores.assign(probSepsisXGB=xgb_model.predict(xgb.DMatrix(CINCdat_test_zScores.iloc[:,0:20])))
print(CINCdat_zScores)

## Quick but not necessarily great way to find a threshold. Also calculate the AUC. Old thresholding algorithm
from sklearn.metrics import roc_curve, roc_auc_score
fpr, tpr, thresholds = roc_curve(CINCdat_zScores.SepsisLabel,CINCdat_zScores.probSepsisXGB)
print('AUC:',round(roc_auc_score(CINCdat_zScores.SepsisLabel,CINCdat_zScores.probSepsisXGB),2))
print('AUC_test:',round(roc_auc_score(CINCdat_test_zScores.SepsisLabel,CINCdat_test_zScores.probSepsisXGB),2))

# Save the model and get the threshold for use as a model
xgb_model.save_model('xgboost.model')
thresh=round(thresholds[np.argmax(tpr - fpr)],4)
print('Threshold:',thresh)

# Quick calculation of utility score
CINCdat = CINCdat.assign(SepsisLabelXGB = (CINCdat_zScores.probSepsisXGB>thresh).astype(int))
CINCdat_test= CINCdat_test.assign(SepsisLabelXGB = (CINCdat_test_zScores.probSepsisXGB>thresh).astype(int))

import evaluate_sepsis_score as ev
util = ev.evaluate_utility(CINCdat.patient,np.array(CINCdat_zScores.SepsisLabel),np.array(CINCdat.SepsisLabelXGB))
print(util)
util_test = ev.evaluate_utility(CINCdat_test.patient,np.array(CINCdat_test_zScores.SepsisLabel),np.array(CINCdat_test.SepsisLabelXGB))
print(util_test)

#### OPTION 6: Random Forest ####
from sklearn.ensemble import RandomForestClassifier
model_RF = RandomForestClassifier(n_estimators= 100)
model_RF.fit(CINCdat_zScores.iloc[:,0:20],CINCdat_zScores.SepsisLabel)

## Add the predictions
CINCdat_zScores = CINCdat_zScores.assign(probSepsisRF=model_RF.predict(CINCdat_zScores.iloc[:,0:20]))
print(CINCdat_zScores)
CINCdat_test_zScores = CINCdat_test_zScores.assign(probSepsisRF=model_RF.predict(CINCdat_test_zScores.iloc[:,0:20]))
print(CINCdat_test_zScores)

## Quick but not necessarily great way to find a threshold. Also calculate the AUC
from sklearn.metrics import roc_curve, roc_auc_score
fpr, tpr, thresholds = roc_curve(CINCdat_zScores.SepsisLabel,CINCdat_zScores.probSepsisRF)
print('AUC:',round(roc_auc_score(CINCdat_zScores.SepsisLabel,CINCdat_zScores.probSepsisRF),2))
print('AUC_test:',round(roc_auc_score(CINCdat_test_zScores.SepsisLabel,CINCdat_test_zScores.probSepsisRF),2))
thresh=round(thresholds[np.argmax(tpr - fpr)],4)
print('Threshold:',thresh)

# Quick calculation of utility score
CINCdat = CINCdat.assign(SepsisLabelRF = (CINCdat_zScores.probSepsisRF>thresh).astype(int))
CINCdat_test= CINCdat_test.assign(SepsisLabelRF = (CINCdat_test_zScores.probSepsisRF>thresh).astype(int))

import evaluate_sepsis_score as ev
util = ev.evaluate_utility(CINCdat.patient,np.array(CINCdat_zScores.SepsisLabel),np.array(CINCdat.SepsisLabelRF))
print(util)
util_test = ev.evaluate_utility(CINCdat_test.patient,np.array(CINCdat_test_zScores.SepsisLabel),np.array(CINCdat_test.SepsisLabelRF))
print(util_test)