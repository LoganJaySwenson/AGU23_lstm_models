# -*- coding: utf-8 -*-
"""
Created on Sun Jan 14 12:52:01 2024

@author: Logan
"""
# 05. Evaluate model performance for stream gauges in the High Plains Aquifer
import os
import pickle
from pathlib import Path

import torch
import pandas as pd
from neuralhydrology.nh_run import eval_run


# Set working dir
os.chdir("C:/Users/Logan/Desktop/AGU_lstm_models")

# Set path to model run
run_id = "AGU_development_run_0612_194636" 
run_dir = Path("runs")/run_id

# Evaluate model performance on the holdout dataset
eval_run(run_dir = run_dir, period="test", gpu=-1, epoch = 18) # train on GPU & test on CPU

with open(run_dir / "test" / "model_epoch018" / "test_results.p", "rb") as fp:
    results = pickle.load(fp)
    
results.keys()
print(results)
print(results.keys())


# Get simulated flow at each stream gauge 
result_dfs = []
for key, value in results.items():
    ds = results[key]['1D']['xr']
    df = ds.to_dataframe()
    df = df.reset_index()[['date', 'flow_obs', 'flow_sim']]
    df['site_no'] = key
    result_dfs.append(df)

result_df = pd.concat(result_dfs, ignore_index=True)

# Save simulated results
result_df.to_csv("data/Gages_flow_sim.csv", index=False)