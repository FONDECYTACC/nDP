# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

import gc
gc.collect()
import sys
sys.modules[__name__].__dict__.clear()

#os.system('"C:/Users/CISS Fondecyt/anaconda32/python.exe" -m conda install spyder-kernels=2.5')

# 0. paquetes y setting ---------------------------------------------------

import os


# si da problemas, debes instalar de afuera todo
#os.system('"C:/Users/CISS Fondecyt/anaconda32/python.exe" -m pip install lifelines')
#os.system('"C:/Users/CISS Fondecyt/anaconda32/python.exe" -m pip install --upgrade pip')

!pip install auton-survival
!pip install lifelines

!pip install pandas matplotlib numpy scikit-learn scikit-survival

!pip install pyjanitor
!pip install pandas_flavor
!pip install pyarrow

#os.system('"C:/Users/CISS Fondecyt/anaconda32/python.exe" -m pip install pandas matplotlib numpy scikit-learn scikit-survival')
#os.system('"C:/Users/CISS Fondecyt/anaconda3/python.exe" -m pip install pyjanitor')
#os.system('"C:/Users/CISS Fondecyt/anaconda3/python.exe" -m pip install pyarrow')

import pyarrow.parquet as pq
from janitor import clean_names, remove_empty

import lifelines
import re
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

%matplotlib inline

from sklearn import set_config
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import OrdinalEncoder

from sksurv.datasets import load_gbsg2
from sksurv.preprocessing import OneHotEncoder
from sksurv.ensemble import RandomSurvivalForest


# 1. BD ---------------------------------------------------

prueba = pq.read_table("data_mine_miss_proc2_20240417.gz.parquet").to_pandas()
#función de glimpse
def glimpse(df):
    print(f"Rows: {df.shape[0]}")
    print(f"Columns: {df.shape[1]}")
    for col in df.columns:
        print(f"$ {col} <{df[col].dtype}> {df[col].head().values}")

glimpse(prueba)


print(
pd.concat([
    pd.crosstab(prueba.cohab_dum_alone, columns="count"),
    pd.crosstab(prueba.cohab_dum_alone, columns="count", normalize='all')
], axis=1)
)

# 2. Modelo ---------------------------------------------------

from sksurv.datasets import get_x_y
from sksurv.linear_model import CoxPHSurvivalAnalysis
from sksurv.preprocessing import OneHotEncoder

from sksurv.ensemble import RandomSurvivalForest
from sksurv.util import Surv


df = pd.DataFrame(prueba, columns=["id", "lag_time", "time", "surv_time", "event",
"lag_dias_treat_imp_sin_na", "lag_comp_bpsc_y2_moderate",
"lag_comp_bpsc_y3_severe", "lag_policonsumo2", 
"edad_al_ing_1", "ano_nac_corr",
"esc_dum_rec_3prim", "esc_dum_rec_2high", 
"susprindum_oh", "susprindum_coc", 
"susprindum_pbc", "susprindum_mar", 
"freq_cons_dum_5day", "freq_cons_dum_44to6wk", 
"freq_cons_dum_32to3wk", "freq_cons_dum_21wkmore", 
"cond_oc_dum_3unemp", "cond_oc_dum_2inact", 
"viv_dum_illegal", "viv_dum_own", 
"viv_dum_rent", "viv_dum_temp", 
"macro_dum_south", "macro_dum_north", 
"psycom_dum_with", "psycom_dum_study", 
"rurality_rural", "rurality_mix", 
"susinidum_oh", "susinidum_coc", 
"susinidum_pbc", "susinidum_mar", 
"cohab_dum_alone", "cohab_dum_fam_or", 
"cohab_dum_cpl_child", "porc_pobr", 
"tipo_de_plan_2"])

#fill Nas with 0
df['lag_dias_treat_imp_sin_na'] = df['lag_dias_treat_imp_sin_na'].fillna(0)


#haciendo el formato supervivencia
structured_data = Surv.from_dataframe("event", "surv_time", df)
#sacando las variables que no son predictoras
X = df.drop(columns=["id", "lag_time", "time", "surv_time", "event", "tipo_de_plan_2"]) 


# Initialize and fit Random Survival Forest
rsf = RandomSurvivalForest(n_estimators=100, min_samples_split=10, random_state=42)
rsf.fit(X, structured_data)
#   Cell In[40], line 2
#     rsf.fit(X, structured_data)

#   File ~\AppData\Local\Programs\Python\Python311\Lib\site-packages\IPython\core\displayhook.py:269 in __call__
#     self.update_user_ns(result)

#   File ~\AppData\Local\Programs\Python\Python311\Lib\site-packages\IPython\core\displayhook.py:207 in update_user_ns
#     if self.cache_size and result is not self.shell.user_ns['_oh']:

# KeyError: '_oh'
print("Me apareció un error")
# Print model details
print("Feature importances:", rsf.feature_importances_)

# 3. Opc 2.  ---------------------------------------------------


from lifelines import CoxPHFitter

# Sample setup - assuming a 'cluster' column represents the frailty groups
cph = CoxPHFitter(penalizer=0.01)  # Small penalization to avoid excessive exclusion 
results = cph.fit(df, duration_col='surv_time', event_col='event', cluster_col='id', strata='tipo_de_plan_2')

# Access selected variables
results.summary.index 








from sklearn.model_selection import KFold 
import pandas as pd
from lifelines import CoxPHFitter  # Or your preferred library

# ... (Your data loading, variable selection, etc.)

def evaluate_model(model, X_test, duration_col, event_col): 
    # ... Implementation to calculate the C-index based on model predictions

cv = KFold(n_splits=10, shuffle=True, random_state=42)  # Set a random_state for reproducibility
performance_scores = []

for train_index, test_index in cv.split(data):
    X_train, X_test = data.iloc[train_index], data.iloc[test_index]
    model = CoxPHFitter().fit(X_train, duration_col, event_col, cluster_col='id', strata='tipo_de_plan_2') 

    score = evaluate_model(model, X_test, duration_col, event_col)
    performance_scores.append(score)

overall_cv_performance = sum(performance_scores) / len(performance_scores)
print("Average Cross-Validated Performance:", overall_cv_performance)
