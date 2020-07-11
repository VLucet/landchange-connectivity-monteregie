#!/usr/bin/python3
# 8.1 Analysis via SURF algorithm

# Import local SURF module
import sys
import os
import cv2

import multiprocessing as mp
import pandas as pd
from functools import partial

sys.path.append(".")
from surf import utils

# import matplotlib.pyplot as plt

base_path = 'outputs/current_density_sum/'
base_files = os.listdir(base_path)
list_of_files = [base_path + file for file in base_files]
print(len(list_of_files))
# print(list_of_files)

the_mask = cv2.imread("data/stsim/aggregated/primary_stratum_mont_or_not_or_PA.tif", -1).astype("uint8")
the_mask[the_mask == 2] = 1

workers = mp.Pool(processes=8)
#kp_lengths = workers.map(partial(utils.get_kp_lengths, mask=the_mask), list_of_files)
kp_lengths = workers.map(utils.get_kp_lengths, list_of_files)
workers.close()

# ------------ Create CSV ------------ 

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
