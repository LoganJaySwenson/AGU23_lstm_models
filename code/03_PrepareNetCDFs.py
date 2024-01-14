# -*- coding: utf-8 -*-
"""
Created on Tue Nov 28 14:23:26 2023

@author: Logan
"""
# 03. Prepare inputs for neuralhydrology models
import os
import pandas as pd
import xarray as xr

# Set dir
os.chdir("C:/Users/Logan/Desktop/AGU_lstm_models")

dir_data = "C:/Users/Logan/Desktop/AGU_lstm_models/data_dir/data"
dir_save = "C:/Users/Logan/Desktop/AGU_lstm_models/data_dir/time_series"

# Read all sites and save as netCDF 
for i in os.listdir(dir_data):
    file_path = os.path.join(dir_data, i)
    df = pd.read_csv(file_path)

    df["date"] = pd.to_datetime(df["date"])
    

    df.drop(columns=["site_no"], inplace=True)

    site_no = os.path.splitext(i)[0]

    dataset = xr.Dataset()

    for column in df.columns:
        if column == "date":
            dataset.coords["date"] = df[column].values
        else:
            dataset[column] = xr.Variable("date", df[column].values)

    # Save the dataset to a netCDF file
    netcdf_file = os.path.join(dir_save, site_no + ".nc")
    dataset.to_netcdf(netcdf_file)
    
    print(f"Saved {netcdf_file}")