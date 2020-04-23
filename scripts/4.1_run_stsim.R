#-------------------------------------------------------------------------------
## 4.1 Run ST-SIM
## 2020
## Inputs: stratums spatial multipliers, datasheets
## Outputs: librairy and model results
#-------------------------------------------------------------------------------

rm(list=ls())
set.seed(77)

#-------------------------------------------------------------------------------
# Set parameters 
OMP_NUM_THREADS <- as.numeric(Sys.getenv("OMP_NUM_THREADS"))
aggregation <- list(ag = as.logical(Sys.getenv("R_AGGR")), 
                    factor = as.numeric(Sys.getenv("R_AGGR_FACT")))
STSIM_ITER <- as.numeric(Sys.getenv("STSIM_ITER"))
STSIM_TS_START <- as.numeric(Sys.getenv("STSIM_TS_START"))
STSIM_TS_END <- as.numeric(Sys.getenv("STSIM_TS_END"))

STSIM_STEP_SAVE <- as.numeric(Sys.getenv("STSIM_STEP_SAVE"))
STSIM_STEP_COMPUTE <- as.numeric(Sys.getenv("STSIM_STEP_COMPUTE"))
ST_SIM_DIR <- "../../syncrosim/"

STSIM_RUN  <- as.logical(Sys.getenv("STSIM_RUN"))

R_METHOD_STSIM <- Sys.getenv("R_METHOD_STSIM")
R_SAMPLING_METHOD <- Sys.getenv("R_SAMPLING_METHOD")

if (is.na(OMP_NUM_THREADS)) { 
  OMP_NUM_THREADS <- 2 ; print("Running on 2 cores only")
  ST_SIM_DIR <- "/home/vlucet/Documents/Apex/syncrosim_2_10"
}

# !!! To be commented out
ST_SIM_DIR <- "/home/vlucet/Documents/Apex/syncrosim_2_10"
# !!! To be commented out

#-------------------------------------------------------------------------------

# Sources functions 
source("scripts/functions/rsyncrosim_helpr.R")

# Working directory
# setwd(wd_win)
print("Preparing library");Sys.time()

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
myLibrary <- ssimLibrary(myLibraryName, session=mySession, overwrite = TRUE) 
Definitions <- project(myLibrary, project="Definitions")

#-------------------------------------------------------------------------------
## stsim SETTINGS
#-------------------------------------------------------------------------------

# Model basic settings ----------------------------------------------------     

definition_settings <- 
  c("Terminology", "Stratum", "SecondaryStratum", "TertiaryStratum", "StateLabelX", "StateLabelY", # TODO add it here
    "StateClass", "TransitionType", "TransitionGroup", "TransitionTypeGroup", 
    "StateAttributeType")

for (sheetname in definition_settings){
  loadSheet(sheetname,NULL, Definitions, path = "config/stsim/")  
}

## stsim SECONDARY SETTINGS - PARTITION IN SCENARIOS

# Run Parameters - DEFAULT
run_default <- scenario(Definitions, scenario = "Run Control: Default")

loadSheet("RunControl", NULL, run_default, params = list(MinimumIteration = 1,
                                                         MaximumIteration = STSIM_ITER, 
                                                         MinimumTimestep = STSIM_TS_START,
                                                         MaximumTimestep = STSIM_TS_END, 
                                                         IsSpatial = TRUE))

# Transition Pathways - DEFAULT
loadSheet("DeterministicTransition", NULL, run_default, path = "config/stsim/")
loadSheet("Transition", NULL, run_default, path = "config/stsim/")

# Initial conditions
if (aggregation$ag){
  loadSheet("InitialConditionsSpatial", NULL,  run_default, 
            params = list(StratumFileName = paste0(getwd(),"/data/stsim/aggregated/primary_stratum.tif"), 
                          StateClassFileName = paste0(getwd(),"/data/land_use/aggregated/aggregated_lu_1990.tif"),
                          SecondaryStratumFileName = paste0(getwd(),"/data/stsim/aggregated/secondary_stratum.tif"), 
                          TertiaryStratumFileName = paste0(getwd(),"/data/stsim/aggregated/tertiary_stratum.tif")))
} else {
  loadSheet("InitialConditionsSpatial", NULL, run_default, 
            params = list(StratumFileName = paste0(getwd(),"/data/stsim/primary_stratum_mont_or_not_30by30.tif"), 
                          StateClassFileName = 
                            paste0(getwd(),"/data/land_use/LandUse_mont_aafc_buffered_30by30_1990.tif"),
                          SecondaryStratumFileName = paste0(getwd(),"/data/stsim/secondary_stratun_mun_30by30.tif"), 
                          TertiaryStratumFileName = paste0(getwd(),"/data/stsim/tertiary_stratum_PA_30by30.tif")))
}

# Output options
loadSheet("OutputOptions", NULL, run_default, 
          params = list(SummaryOutputSC = TRUE, SummaryOutputSCTimesteps = STSIM_STEP_SAVE, 
                        SummaryOutputTR = TRUE, SummaryOutputTRTimesteps = STSIM_STEP_SAVE, 
                        RasterOutputSC = TRUE, RasterOutputSCTimesteps = STSIM_STEP_SAVE,
                        RasterOutputTR = TRUE, RasterOutputTRTimesteps = STSIM_STEP_SAVE))

#----------------------------------------------------------------------------

# Transition targets
targets_default <- scenario(Definitions, scenario = "Transition Targets: Default")
loadSheet("TransitionTarget", filename=paste0("TransitionTarget_", STSIM_STEP_COMPUTE, "y"), 
          targets_default, path = "config/stsim/")

# Transition multipliers (non-Spatial) 
# TODO follow up here
transmul_default <- scenario(Definitions, scenario = "Transition Multipliers: Default")
loadSheet("TransitionMultiplierValue", NULL, transmul_default, path = "config/stsim/")

# Spatial multipliers
spatial_multiplier_default <-
  scenario(Definitions,scenario = "Spatial Multiplier: Default")

loadSheet("TransitionSpatialMultiplier", NULL, spatial_multiplier_default,
          params = list(TransitionGroupID = c("Deforestation [Type]",
                                              "Agricultural Loss [Type]",
                                              "Agricultural Expansion [Type]"), 
                        MultiplierFileName = c(paste0(getwd(), "/data/stsim/spatial_multipliers/",
                                                      R_METHOD_STSIM, "_", R_SAMPLING_METHOD, "_urb_spa_mul.tif"), 
                                               paste0(getwd(), "/data/stsim/spatial_multipliers/",
                                                      R_METHOD_STSIM, "_", R_SAMPLING_METHOD, "_urb_spa_mul.tif"),
                                               paste0(getwd(), "/data/stsim/spatial_multipliers/",
                                                      R_METHOD_STSIM,"_", R_SAMPLING_METHOD, "_agex_spa_mul.tif"))))

# Transition Adjacency
#transition_adjacency <- scenario(Definitions, scenario = "Transition Adjacency: Default")
#loadSheet("StateAttributeValue", NULL, transition_adjacency, path = "config/stsim/")
#loadSheet("TransitionAdjacencySetting", NULL, transition_adjacency, path = "config/stsim/")
#loadSheet("TransitionAdjacencyMultiplier", NULL, transition_adjacency, path = "config/stsim/")

####################
## Full Scenarios ##
####################

scenario_1_test <- scenario(Definitions, scenario = "Test-scenario1")
#dependency(scenario_1_test, c(run_default, transition_adjacency, transmul_default, 
#                              spatial_multiplier_default, targets_default))# Removed adajcency
dependency(scenario_1_test, c(run_default, transmul_default,
                              spatial_multiplier_default, targets_default))

print("Running StSIM");Sys.time()

#########
## RUN ##
#########

if (STSIM_RUN){
  results <- run(scenario_1_test, summary = TRUE, jobs = OMP_NUM_THREADS)
  print(results)
  
  ## DIAGNOSTICS 
  
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
    group_by(Timestep, SecondaryStratumID, TransitionGroupID) %>%
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
    arrange(Timestep)
  
  # general plot
  gen_plot_df %>% 
    ggplot(aes(x=Amount_mean.x, Amount_mean.y, color=as.factor(Timestep))) +
    geom_point(show.legend = F) +
    #geom_abline() +
    geom_abline(slope = 1, intercept = 0, linetype=2)
  ggsave("outputs/figures/one_to_one_targets.png")
  
  summary(lm(Amount_mean.y~Amount_mean.x, gen_plot_df))
}