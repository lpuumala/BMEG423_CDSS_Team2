#!/usr/bin/env python

import numpy as np
import pandas as pd
import xgboost as xgb

# This one just uses arrays 
def get_sepsis_score(data, model):
    if len(data.shape) == 1:
        data = np.reshape(data, (1, -1))
        
    x_mean = np.array([1.08, 23.77, 97.13, 36.85, 122.19, 18.86, 0.49, 23.25, 1.55, 10.37, 11.31, 62.72,  0])
    x_std = np.array([0.29, 20.97, 2.91, 0.71, 23.03, 5.03, 0.21, 18.91, 1.94, 1.97, 9.24, 15.98,  1])

    # Fill NaN values with zeros
    for i in range(1, len(data)):
        mask = np.isnan(data[i])
        data[i][mask] = data[i - 1][mask]

    # Set ranges for feasible values
    data[:, 7] = np.where((data[:, 7] >= 5) & (data[:, 7] <= 60), data[:, 7], np.nan)
    data[:, 1] = np.where((data[:, 1] >= 10) & (data[:, 1] <= 300), data[:, 1], np.nan)
    data[:, 5] = np.where((data[:, 5] >= 0) & (data[:, 5] <= 300), data[:, 5], np.nan)
    data[:, 4] = np.where((data[:, 4] >= 40) & (data[:, 4] <= 280), data[:, 4], np.nan)
    data[:, 6] = np.where((data[:, 6] >= 20) & (data[:, 6] <= 130), data[:, 6], np.nan)
    data[:, 2] = np.where((data[:, 2] >= 60) & (data[:, 2] <= 100), data[:, 2], np.nan)
    data[:, 3] = np.where((data[:, 3] >= 32) & (data[:, 3] <= 42.2), data[:, 3], np.nan)
                    
    # Calculate 'shock' for entire columns of data
    shock = data[:, 1] / data[:, 5]
    x = np.concatenate((shock.reshape(-1, 1), data[:, [0, 2, 3, 4, 7, 8, 10, 11, 16, 17, 18, 19]]), axis=1)
    x = np.nan_to_num(np.abs((x - x_mean) / x_std), nan=0)
    
    # Create a DMatrix for XGBoost
    x_norm_dm = xgb.DMatrix(np.reshape(x[-1,[1,2,3,4,5,6,7,8,9,10,11,0,12]],(1,-1)))

    # Predict using the model
    scores = model.predict(x_norm_dm)
    score = scores[-1]
    label = score >= 0.0255
    return score, label

def load_sepsis_model():    
    model = xgb.Booster({'nthread': 2})
    model.load_model("xgboost.model")
    
    return model






