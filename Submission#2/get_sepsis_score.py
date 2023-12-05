import os, glob, sys
import numpy as np
import pandas as pd
import xgboost as xgb

def get_sepsis_score(data, model):
    thresh = 0.0219
    
    # Mean values found from the training set
    x_mean = np.array([23.77, 85.27, 97.13, 36.86, 122.19, 81.84, 63.05, 18.88, 0.48, 7.39, 23.47, 1.55, 131.97, 2.05, 4.16, 31.01, 10.33, 11.43, 62.72, 0, 1.08, -0.46, 0.02, 0.08, 0.36, -4.82, 2.34, -0.02, -0.32])
    # Standard deviations found from the training set
    x_std = np.array([20.97, 16.85, 2.9, 0.71, 22.98, 16.25, 13.67, 5.02, 0.19, 0.06, 19.2, 1.92, 47.29, 0.38, 0.59, 5.65, 1.95, 9.7, 15.98, 1, 0.29, 1.63, 0.08, 0.19, 1.74, 11.61, 5.8, 0.33, 0.85])
    
    # Creating a mask for the array values
    iculos, hr, o2sat, temp, sbp, mp, dbp, resp, fio2, ph, bun, creatinine, glucose, magnesium, potassium, hct, hgb, wbc, age, sex, shock, wbc_grad, shock_grad, temp_grad, resp_grad, wbc_dev, resp_dev, shock_dev, temp_dev, ewsp = range(30)
    
    # Set ranges for feasible values
    data[:, resp] = np.where((data[:, resp] >= 5) & (data[:, resp] <= 60), data[:, resp], np.nan)
    data[:, hr] = np.where((data[:, hr] >= 10) & (data[:, hr] <= 300), data[:, hr], np.nan)
    data[:, mp] = np.where((data[:, mp] >= 0) & (data[:, mp] <= 300), data[:, mp], np.nan)
    data[:, sbp] = np.where((data[:, sbp] >= 40) & (data[:, sbp] <= 280), data[:, sbp], np.nan)
    data[:, dbp] = np.where((data[:, dbp] >= 20) & (data[:, dbp] <= 130), data[:, dbp], np.nan)
    data[:, o2sat] = np.where((data[:, o2sat] >= 60) & (data[:, o2sat] <= 100), data[:, o2sat], np.nan)
    data[:, temp] = np.where((data[:, temp] >= 32) & (data[:, temp] <= 42.2), data[:, temp], np.nan)
    
    # Forward filling the data 
    for i in range(1, len(data)):
        mask = np.isnan(data[i])
        data[i][mask] = data[i - 1][mask]
        
    # Calculate 'shock' for entire columns of data
    shockCalc = data[:, hr] / data[:, mp]
    data = np.column_stack((data, shockCalc))
    
    # Calculate the gradients for WBC, Shock, Temp, and Resp if the data has more than one row
    if len(data) > 1:
        WBC_grad = np.gradient(data[:, wbc])
        Shock_grad = np.gradient(data[:, shock])
        Temp_grad = np.gradient(data[:, temp])
        Resp_grad = np.gradient(data[:, resp])
    else:
        WBC_grad = np.zeros(len(data))
        Shock_grad = np.zeros(len(data))
        Temp_grad = np.zeros(len(data))
        Resp_grad = np.zeros(len(data))
    
    # Add the gradient columns to data
    data = np.column_stack((data, WBC_grad, Shock_grad, Temp_grad, Resp_grad))
   
    # Calculate the deviation from the baseline. Baseline is the first measurement upon entry
    # Find the baseline first, then the deviation second
    baseline_WBC = data[0, wbc]
    baseline_Resp = data[0, resp]
    baseline_shock = data[0, shock]
    baseline_Temp = data[0, temp]

    WBC_dev = data[:, wbc] - baseline_WBC
    Resp_dev = data[:, resp] - baseline_Resp
    Shock_dev = data[:, shock] - baseline_shock
    Temp_dev = data[:, temp] - baseline_Temp

    # Adding the deviations to the array
    data = np.column_stack((data, WBC_dev, Resp_dev, Shock_dev, Temp_dev))
    
    # Order the columns from data in order for the model to read properly
    x = data[:, [iculos, hr, o2sat, temp, sbp, mp, dbp, resp, fio2, ph, bun, creatinine, glucose, magnesium, potassium, hct, hgb, wbc, age, sex, shock, wbc_grad, shock_grad, temp_grad, resp_grad, wbc_dev, resp_dev, shock_dev, temp_dev]]
    x = np.nan_to_num((x - x_mean) / x_std, nan=0)
    

    # Create a DMatrix for XGBoost
    x_norm_dm = xgb.DMatrix(np.reshape(x[-1,:],(1,-1)))

    # Predict using the model. Do not validate features, causes bugs with unlabeled DMatrix
    scores = model.predict(x_norm_dm, validate_features=False)
    
    # Find the biggest score that is not larger than 1
    score=min(max(scores),1)
    label = score >= thresh
    
    # return the score and its correspond label
    return score, label


def load_sepsis_model():    
    model = xgb.Booster({'nthread': 2})
    model.load_model("xgboost.json")
    
    return model