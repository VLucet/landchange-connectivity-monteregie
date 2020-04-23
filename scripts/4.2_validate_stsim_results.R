#-------------------------------------------------------------------------------
## 4.2 Validate ST-SIM results
## 2020
## Inputs: stsim results
## Outputs: plots and etc..
#-------------------------------------------------------------------------------

rm(list=ls())
set.seed(77)

ST_SIM_DIR <- "../../syncrosim_2_10/"
#ST_SIM_DIR <- "/home/vlucet/Documents/Apex/syncrosim_2_10"

#-------------------------------------------------------------------------------

# Load important libraries
suppressPackageStartupMessages({
  library(raster)
  library(tidyverse)
  library(ggforce)
  library(rsyncrosim)
})

# Set options (according to BR)
options("scipen"=100, "digits"=4)

# Directory paths
projectDir <- paste0(getwd(), "/")
dataDir <- paste0(getwd(), "/data/")
libraryDir <- paste0(getwd(), "/libraries/stsim/")

# STSim library
mySession <- session(ST_SIM_DIR)
print(mySession)
#addPackage("stsim", mySession)
myLibraryName <- paste0(libraryDir, "monteregie-conncons-scripted.ssim")
myLibrary <- ssimLibrary(myLibraryName, session=mySession) 

#-------------------------------------------------------------------------------

## DIAGNOSTICS 
results <- read_rds("data/temp/stsim_run_results.rds")

trans_results <- datasheet(myLibrary, scenario = results$scenarioId, 
                           "stsim_OutputStratumTransition")
# mun_set <- sample(trans_results$SecondaryStratumID, size = 20)

print(head(trans_results))

targets <- read_csv("config/stsim/TransitionTarget_10y.csv") %>% 
  #filter(SecondaryStratumID %in% mun_set) %>% 
  group_by(Timestep, SecondaryStratumID, TransitionGroupID) %>%
  summarise(Amount_mean = mean(Amount)) %>% ungroup

trans_results_mod <- trans_results  %>% 
  #filter(SecondaryStratumID %in% mun_set) %>% 
  group_by(ScenarioID, Timestep, SecondaryStratumID, TransitionGroupID) %>%
  summarise(Amount_mean = mean(Amount)) %>% ungroup
trans_results_mod_only_3 <- trans_results_mod %>% 
  filter(Timestep >= 3) %>% 
  group_by(SecondaryStratumID, TransitionGroupID) %>%
  summarise(Amount_mean = mean(Amount_mean)) %>% ungroup %>% 
  mutate(Timestep=3)
trans_results_toplot <- bind_rows(subset(trans_results_mod, Timestep < 3), 
                                  trans_results_mod_only_3)

# Plots 
trans_results_toplot %>% 
  filter(TransitionGroupID != "Urbanisation") %>% 
  ggplot(aes(x=Timestep, y=Amount_mean)) +
  geom_line(show.legend = F) + 
  facet_grid_paginate(TransitionGroupID~SecondaryStratumID, nrow=3, ncol=5, page = 1, scales = "free") +
  geom_line(data=targets, inherit.aes = T, linetype=2)
ggsave("outputs/figures/one_to_one_mun.png") 

## JOIN for general diagnoctic plot
gen_plot_df <- left_join(trans_results_toplot, targets, 
                         by= c("Timestep", "SecondaryStratumID", "TransitionGroupID")) %>% 
  filter(TransitionGroupID != "Urbanisation") %>% 
  rename(observed = Amount_mean.x, targets = Amount_mean.y) %>% 
  arrange(Timestep)

# general plot
gen_plot_df %>% 
  #filter(ScenarioID == 8) %>% 
  ggplot(aes(x=targets, y=observed, color=as.factor(Timestep))) +
  geom_point(show.legend = F) +
  geom_abline(slope = 1, intercept = 0, linetype=2) +
  facet_wrap("ScenarioID")
ggsave("outputs/figures/one_to_one_targets.png")

summary(lm(observed~targets, gen_plot_df))
