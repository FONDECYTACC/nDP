# -*- coding: utf-8 -*-
"""
Created on Wed May  1 14:42:05 2024

@author: andre
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

from lifelines import CoxPHFitter
from lifelines.datasets import load_rossi
from lifelines.utils import to_long_format, add_covariate_to_timeline
from lifelines import CoxTimeVaryingFitter


# 1. BD ---------------------------------------------------

prueba = pq.read_table("data_mine_miss_restr_proc2_20240427.gz.parquet").to_pandas()
#función de glimpse
def glimpse(df):
    print(f"Rows: {df.shape[0]}")
    print(f"Columns: {df.shape[1]}")
    for col in df.columns:
        print(f"$ {col} <{df[col].dtype}> {df[col].head().values}")

glimpse(prueba)


print(
pd.concat([
    pd.crosstab(prueba.tipo_de_plan_2_mod, columns="count"),
    pd.crosstab(prueba.tipo_de_plan_2_mod, columns="count", normalize='all')
], axis=1)
)

# 3. Opc 2.  ---------------------------------------------------


# Create the covariate timeline
time_varying_df = add_covariate_to_timeline(df_long, df_long[['age_time']], duration_col='stop', event_col='arrest')

# Fit the Cox model
cph = CoxPHFitter()
cph.fit(prueba, duration_col='time', event_col='event', cluster_col='id', 
        formula="lag_tr_outcome + log_lag_dias_treat_imp_sin_na + lag_comp_bpsc_y3_severe + lag_less_90d_tr1_rec2 + lag_policonsumo2 + edad_al_ing_1 + ano_nac_corr + susinidum_oh + susinidum_coc + susinidum_pbc + susinidum_mar + psycom_dum_study + freq_cons_dum_5day + cond_oc_dum_2inact + cond_oc_dum_3unemp + susprindum_oh + susprindum_coc + susprindum_pbc + susprindum_mar")
print(cph.summary)

ctv = CoxTimeVaryingFitter()
ctv.fit(prueba, id_col='id', start_col='lag_time', stop_col='time', event_col='event', strata="tipo_de_plan_2_mod",
        formula="lag_tr_outcome + log_lag_dias_treat_imp_sin_na + lag_comp_bpsc_y3_severe + lag_less_90d_tr1_rec2 + lag_policonsumo2 + edad_al_ing_1 + ano_nac_corr + susinidum_oh + susinidum_coc + susinidum_pbc + susinidum_mar + psycom_dum_study + freq_cons_dum_5day + cond_oc_dum_2inact + cond_oc_dum_3unemp + susprindum_oh + susprindum_coc + susprindum_pbc + susprindum_mar",
show_progress=True, robust=True)
print(ctv.summary)


prueba = pd.get_dummies(prueba, columns=['tipo_de_plan_2_mod'], prefix=['tp2mod_']) 

MIN_STRATUM_SIZE = 100
unique_strata = prueba["tipo_de_plan_2_mod"].unique()
models = {}
for stratum in unique_strata:
    subset = prueba[prueba["tipo_de_plan_2_mod"] == stratum]
    if subset.shape[0] < MIN_STRATUM_SIZE:
        print(f"Skipping stratum '{stratum}' due to small size.")
        continue # Move on to the next stratum
    # Correct indentation below
    ctv = CoxTimeVaryingFitter()
    ctv.fit(subset, id_col="id", start_col="lag_time", stop_col="time", event_col="event", formula="lag_tr_outcome + log_lag_dias_treat_imp_sin_na + lag_comp_bpsc_y3_severe + lag_less_90d_tr1 + lag_policonsumo2 + edad_al_ing_1 + ano_nac_corr + susinidum_oh + susinidum_coc + susinidum_pbc + susinidum_mar + psycom_dum_study + freq_cons_dum_5day + cond_oc_dum_2inact + cond_oc_dum_3unemp + susprindum_oh + susprindum_coc + susprindum_pbc + susprindum_mar", show_progress=True)
    models[stratum] = ctv
    print(f"Stratum: {stratum}")
    print(ctv.summary)