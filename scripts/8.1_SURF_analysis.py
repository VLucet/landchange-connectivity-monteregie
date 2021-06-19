#!/usr/bin/python3
# 8.1 Analysis via SURF algorithm

#-------------------------------------------------------------------------------
## 8.2 Feature dectection, guessed from Pelletier et al.
## 2020
## Inputs: Outputs from Circuitscape runs
## Outputs: Detected features
#-------------------------------------------------------------------------------

# Import local SURF module
import sys
import os
import cv2

import multiprocessing as mp
import pandas as pd
import numpy as np
from functools import partial

sys.path.append(".")
from surf import utils
from matplotlib import pyplot as plt
# import matplotlib.pyplot as plt


def Filter_all(string, substr):
    return [str for str in string if
            all(sub in str for sub in substr)]


def Filter_any(string, substr):
    return [str for str in string if
            any(sub in str for sub in substr)]


base_path = 'outputs/current_density_sum/'
base_files = os.listdir(base_path)
base_files = [str for str in base_files if 'sce' in str]
list_of_files = [base_path + file for file in base_files]


print(len(list_of_files))
# print(list_of_files)

the_mask = cv2.imread("data/stsim/aggregated/primary_stratum_mont_or_not_or_PA.tif", -1).astype("uint8")
the_mask[the_mask == 2] = 1

# ------------------------------------

# workers = mp.Pool(processes=8)
# kp_lengths = workers.map(partial(utils.get_kp_lengths, mask=the_mask, h_threshold=7000, oct_nb=1, oct_layers=2), list_of_files)
# # kp_lengths = workers.map(utils.get_kp_lengths, list_of_files)
# workers.close()
# 
# # i = 0
# # kp_lengths = [0] * len(list_of_files)
# # for file in list_of_files:
# #     kp_lengths[i] = utils.get_kp_lengths(file, mask=the_mask, h_threshold=7000, oct_nb=1, oct_layers=2)
# #     i += i
# 
# # ------------ Create surf CSV ------------
# 
# # Split at ".", get first element, then split at "_"
# splitted = list(map(lambda x: x.split("_"), [i[0] for i in list(map(lambda x: x.split("."), base_files))]))
# # print(splitted)
# results = pd.DataFrame(list(zip([i[1] for i in splitted],
#                                 [i[3] for i in splitted],
#                                 [i[5] for i in splitted],
#                                 [i[6] for i in splitted],
#                                 kp_lengths)),
#                        columns=["scenario",
#                                 "timestep",
#                                 "iter",
#                                 "species",
#                                 "kp_nb"])
# # print(results.head())
# results.to_csv("surf/surf_output.csv", index=False)


# ------------ Create hist CSV ------------


# scenarios
temp = []
final = pd.DataFrame()
for sce in ["sce_" + str(nb) for nb in range(38, 53)]:
    for spe in ['BLBR', 'PLCI', 'MAAM', 'URAM', 'RASY']:
        for iter in [5]: # list(range(1,11))
            for ts in [2, 11]:
                subs = [sce, spe, 'it_'+str(iter)+'_', 'ts_'+str(ts)+'_']
                filtered = Filter_all(list_of_files, subs)
                print(filtered)
                if len(filtered) is 1:
                    data = utils.read_img(filtered[0]).transform().img[the_mask == 1].flatten()
                    n, bins = np.histogram(data, 1000)
                    temp = {'sce': sce, 'species': spe, 'iter': iter, 'ts': ts, 'n': n, 'bins': bins[range(0, len(bins)-1)]}
                    temp_df = pd.DataFrame(temp)
                    temp_df.head()
                    final = final.append(temp_df, ignore_index=True)
                else:
                    print("length error, passing")
                    next

final.to_csv("outputs/final/final_values_output.csv", index=False)

temp = []
final = pd.DataFrame()
for sce in ["sce_" + str(nb) for nb in [37]]:
    for spe in ['BLBR', 'PLCI', 'MAAM', 'URAM', 'RASY']:
        for iter in [5]: # list(range(1,11))
            for ts in [0, 2]:
                subs = [sce, spe, 'it_'+str(iter)+'_', 'ts_'+str(ts)+'_']
                filtered = Filter_all(list_of_files, subs)
                print(filtered)
                if len(filtered) is 1:
                    data = utils.read_img(filtered[0]).transform().img[the_mask == 1].flatten()
                    n, bins = np.histogram(data, 1000)
                    temp = {'sce': sce, 'species': spe, 'iter': iter, 'ts': ts, 'n': n, 'bins': bins[range(0, len(bins)-1)]}
                    temp_df = pd.DataFrame(temp)
                    temp_df.head()
                    final = final.append(temp_df, ignore_index=True)
                else:
                    print("length error, passing")
                    next

final.to_csv("outputs/final/final_values_output_original.csv", index=False)

# True land use


base_path = 'outputs/current_density_sum/'
base_files = os.listdir(base_path)
base_files = [str for str in base_files if 'TRUE' in str]
list_of_files = [base_path + file for file in base_files]


temp = []
final = pd.DataFrame()
for spe in ['BLBR', 'PLCI', 'MAAM', 'URAM', 'RASY']:
    for ts in [0, 2]:
        subs = [spe, 'TRUE_'+str(ts)+'_']
        filtered = Filter_all(list_of_files, subs)
        print(filtered)
        if len(filtered) is 1:
            data = utils.read_img(filtered[0]).transform().img[the_mask == 1].flatten()
            n, bins = np.histogram(data, 1000)
            temp = {'species': spe, 'ts': ts, 'n': n, 'bins': bins[range(0, len(bins)-1)]}
            temp_df = pd.DataFrame(temp)
            temp_df.head()
            final = final.append(temp_df, ignore_index=True)
        else:
            print("length error, passing")
            next

final.to_csv("outputs/final/final_values_output_TRUE.csv", index=False)

# ------- Slew of figures -------


for sce in ["sce_" + str(nb) for nb in range(38, 53)]:
    for spe in ['BLBR', 'PLCI', 'MAAM', 'URAM', 'RASY']:
        res_1 = Filter_all(list_of_files, [spe, sce, 'it_1_', 'ts_2_'])
        res_2 = Filter_all(list_of_files, [spe, sce, 'it_1_', 'ts_11_'])
        print(res_1, res_2)
        if len(res_1) is 1 & len(res_2) is 1:
            start = utils.read_img(res_1[0]).transform().img[the_mask == 1].flatten()
            end = utils.read_img(res_2[0]).transform().img[the_mask == 1].flatten()
            fig = plt.figure();
            plt.hist(start, 1000, alpha=0.5);
            plt.hist(end, 1000, alpha=0.5);
            plt.savefig("outputs/figures/" + sce + '_' + spe + "_hists.png");
        else:
            print("length error, passing")
            next



subset = Filter_any(Filter_all(list_of_files, ['it_1_']),  ['ts_2_', 'ts_11_'])
for file in subset:
    fig = plt.figure();
    utils.read_img(file).transform().plot()
    plt.savefig("outputs/figures/" + os.path.basename(file) + "_simple_plot.png");

for file in subset:
    raster = utils.read_img(file)
    img = utils.process_flow(file, mask=the_mask)
    fig = plt.figure()
    plt.imshow(img.img)
    plt.savefig("outputs/figures/" + os.path.basename(file) + "_annotated.png")
