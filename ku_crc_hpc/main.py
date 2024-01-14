# Train LSTM to simulate streamflow at stream gauges in the High Plains Aquifer

# Import libraries
import pickle
from pathlib import Path

import torch
from neuralhydrology.nh_run import start_run

# Train LSTM model on KU CRC HPC
if torch.cuda.is_available():
    start_run(config_file=Path("config.yml"))

# fall back to CPU-only mode
else:
    start_run(config_file=Path("config.yml"), gpu=-1)