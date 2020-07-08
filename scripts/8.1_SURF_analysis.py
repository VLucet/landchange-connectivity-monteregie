#!/usr/bin/python3
# 8.1 Analysis via SURF algorithm

# Import local SURF module
import sys
import os

import multiprocessing as mp
import pandas as pd

sys.path.append(".")
from surf import utils

# import matplotlib.pyplot as plt

base_path = 'outputs/current_density_sum/'
base_files = os.listdir(base_path)
list_of_files = [base_path + file for file in base_files]
print(len(list_of_files))
# print(list_of_files)

workers = mp.Pool(processes=12)
kp_lengths = workers.map(utils.get_kp_lengths, list_of_files)
workers.close()

# Split at ".", get first element, then split at "_"
splitted = list(map(lambda x: x.split("_"), [i[0] for i in list(map(lambda x: x.split("."), base_files))]))
# print(splitted)
results = pd.DataFrame(list(zip([i[1] for i in splitted],
                                [i[3] for i in splitted],
                                [i[5] for i in splitted],
                                [i[6] for i in splitted],
                                kp_lengths)),
                       columns=["scenario",
                                "timestep",
                                "iter",
                                "species",
                                "kp_nb"])
# print(results.head())
results.to_csv("surf/surf_output.csv", index=False)
