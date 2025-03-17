#%%
import importlib.util
import subprocess
import sys

def install_and_import(package):
    if importlib.util.find_spec(package) is None:
        subprocess.check_call([sys.executable, "-m", "pip", "install", package])
    globals()[package] = importlib.import_module(package)

install_and_import('numpy')
install_and_import('pandas')
install_and_import('lightgbm')
install_and_import('xgboost')
install_and_import('tensorflow')
install_and_import('darts')
install_and_import('sklearn')
install_and_import('prophet')

# %%
import os
from pathlib import Path

# Ottiene il percorso completo del file
path_file = Path(__file__).resolve()
# Risale indietro di due cartelle
path_project = path_file.parents[2]
path_data_metrics= path_project/"app"/"data"/"output"/"metrics"/"daily"
print(f"path_data_metrics: {path_data_metrics}")

# %%
import numpy as np
import pandas as pd

import lightgbm as lgb
import xgboost as xgb
import tensorflow as tf
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import LSTM, Dense, Dropout
from darts import TimeSeries
from darts.models import TransformerModel, LightGBMModel, XGBModel
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import MinMaxScaler
from sklearn.ensemble import StackingRegressor
from sklearn.metrics import mean_absolute_error
from prophet import Prophet
print("PACKAGES UPLOADED!!!")

# %%
# Ottiene l'elenco dei file presenti nella cartella
metrics_files = [f for f in os.listdir(path_data_metrics) 
                    if f.endswith('.feather') 
                        and os.path.isfile(os.path.join(path_data_metrics, f))]

print(f"metrics_files: {metrics_files}")



# %%



# %%



# %%



# %%



# %%



# %%



# %%
