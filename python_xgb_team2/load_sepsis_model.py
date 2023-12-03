import xgboost as xgb

def load_sepsis_model():

    return xgb.Booster(model_file='xgboost.model')