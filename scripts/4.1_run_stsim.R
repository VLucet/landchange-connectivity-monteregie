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
OMP_NUM_THREADS <- as.numeric(Sys.getenv("OMP_NUM_THREADS", unset = 4))
aggregation <- list(ag = as.logical(Sys.getenv("R_AGGR", unset = TRUE)), 
                    factor = as.numeric(Sys.getenv("R_AGGR_FACT", unset = 3)))
STSIM_ITER <- as.numeric(Sys.getenv("STSIM_ITER", unset = 2))
STSIM_TS_START <- as.numeric(Sys.getenv("STSIM_TS_START", unset = 0))
STSIM_TS_END <- as.numeric(Sys.getenv("STSIM_TS_END", unset = 6))


STSIM_STEP_SAVE <- as.numeric(Sys.getenv("STSIM_STEP_SAVE", unset = 1))
STSIM_STEP_COMPUTE <- as.numeric(Sys.getenv("STSIM_STEP_COMPUTE", unset = 10))
ST_SIM_DIR <- "../syncrosim_2_10/"

STSIM_RUN  <- as.logical(Sys.getenv("STSIM_RUN", unset = TRUE))

R_METHOD_STSIM <- Sys.getenv("R_METHOD_STSIM", unset = "rf")
# R_SAMPLING_METHOD <- Sys.getenv("R_SAMPLING_METHOD")
R_RATIO <- as.numeric(Sys.getenv("R_RATIO", unset = 2))

# !!! To be commented out
#ST_SIM_DIR <- "/home/vlucet/Documents/Apex/syncrosim_2_10"
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
addPackageFile(filename = "../syncrosim_packages/stsim-3-2-17.ssimpkg", session = mySession)
myLibraryName <- paste0(libraryDir, "monteregie-conncons-scripted.ssim")
myLibrary <- ssimLibrary(myLibraryName, session=mySession, overwrite = TRUE) 
Definitions <- project(myLibrary, project="Definitions")

#-------------------------------------------------------------------------------
## stsim SETTINGS
#-------------------------------------------------------------------------------

# Model basic settings ----------------------------------------------------     

definition_settings <- 
  c("Terminology", 
    "Stratum", "SecondaryStratum", "TertiaryStratum", 
    "StateLabelX", "StateLabelY", "StateClass", 
    "TransitionType", "TransitionGroup", "TransitionTypeGroup")

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

run_historic <- scenario(Definitions, scenario = "Run Control: Historic")
loadSheet("RunControl", NULL, run_historic, params = list(MinimumIteration = 1,
                                                          MaximumIteration = STSIM_ITER, 
                                                          MinimumTimestep = STSIM_TS_START,
                                                          MaximumTimestep = 2, 
                                                          IsSpatial = TRUE))

run_forecast <- scenario(Definitions, scenario = "Run Control: Forecast")
loadSheet("RunControl", NULL, run_forecast, params = list(MinimumIteration = 1,
                                                          MaximumIteration = STSIM_ITER, 
                                                          MinimumTimestep = 2,
                                                          MaximumTimestep = STSIM_TS_END, 
                                                          IsSpatial = TRUE))

# Transition Pathways
deter_trans_default <- scenario(Definitions, scenario = "Deterministic Transition : Default")
loadSheet("DeterministicTransition", NULL, deter_trans_default, path = "config/stsim/")
loadSheet("Transition", NULL, deter_trans_default, path = "config/stsim/")

# Initial conditions
if (aggregation$ag){
  init_ag_1990 <- scenario(Definitions, scenario = "Initial conditions : Aggregated 1990")
  loadSheet("InitialConditionsSpatial", NULL,  init_ag_1990, 
            params = list(StateClassFileName = paste0(getwd(),"/data/land_use/aggregated/aggregated_lu_buffered_1990_patched.tif"),
                          StratumFileName = paste0(getwd(),"/data/stsim/aggregated/primary_stratum_mont_or_not_or_PA.tif"), 
                          SecondaryStratumFileName = paste0(getwd(),"/data/stsim/aggregated/secondary_stratun_mun.tif"), 
                          TertiaryStratumFileName = paste0(getwd(),"/data/stsim/aggregated/tertiary_stratum_land_types.tif")))
  init_ag_2010 <- scenario(Definitions, scenario = "Initial conditions : Aggregated 2010")
  loadSheet("InitialConditionsSpatial", NULL,  init_ag_2010, 
            params = list(StateClassFileName = paste0(getwd(),"/data/land_use/aggregated/aggregated_lu_buffered_2010_patched.tif"),
                          StratumFileName = paste0(getwd(),"/data/stsim/aggregated/primary_stratum_mont_or_not_or_PA.tif"), 
                          SecondaryStratumFileName = paste0(getwd(),"/data/stsim/aggregated/secondary_stratun_mun.tif"), 
                          TertiaryStratumFileName = paste0(getwd(),"/data/stsim/aggregated/tertiary_stratum_land_types.tif")))
  
} else {
  # loadSheet("InitialConditionsSpatial", NULL, run_default, 
  #           params = list(StratumFileName = paste0(getwd(),"/data/stsim/primary_stratum_mont_or_not_30by30.tif"), 
  #                         StateClassFileName = 
  #                           paste0(getwd(),"/data/land_use/LandUse_mont_aafc_buffered_30by30_1990.tif"),
  #                         SecondaryStratumFileName = paste0(getwd(),"/data/stsim/secondary_stratun_mun_30by30.tif"), 
  #                         TertiaryStratumFileName = paste0(getwd(),"/data/stsim/tertiary_stratum_PA_30by30.tif")))
}

# Output options
output_default <- scenario(Definitions, scenario = "OutputOptions : Default")
loadSheet("OutputOptions", NULL, output_default, 
          params = list(SummaryOutputSC = TRUE, SummaryOutputSCTimesteps = STSIM_STEP_SAVE, 
                        SummaryOutputTR = TRUE, SummaryOutputTRTimesteps = STSIM_STEP_SAVE, 
                        RasterOutputSC = TRUE, RasterOutputSCTimesteps = STSIM_STEP_SAVE,
                        RasterOutputTR = FALSE, RasterOutputTRTimesteps = STSIM_STEP_SAVE))

#----------------------------------------------------------------------------
# Transition targets
# targets_default <- scenario(Definitions, scenario = "Transition Targets: Default")
# loadSheet("TransitionTarget", filename=paste0("TransitionTarget_", STSIM_STEP_COMPUTE, "y"), 
#           targets_default, path = "config/stsim/")

targets_default_no_ref_hist <- scenario(Definitions, scenario = "Transition Targets: No Reforestation + historic")
loadSheet("TransitionTarget", filename=paste0("TransitionTarget_", STSIM_STEP_COMPUTE, "y", "_ref_0_historic"), 
          targets_default_no_ref_hist, path = "config/stsim/")

targets_default_no_ref <- scenario(Definitions, scenario = "Transition Targets: No Reforestation")
loadSheet("TransitionTarget", filename=paste0("TransitionTarget_", STSIM_STEP_COMPUTE, "y", "_ref_0"), 
          targets_default_no_ref, path = "config/stsim/")

targets_default_ref <- scenario(Definitions, scenario = "Transition Targets: Reforestation")
loadSheet("TransitionTarget", filename=paste0("TransitionTarget_", STSIM_STEP_COMPUTE, "y", "_ref"), 
          targets_default_ref, path = "config/stsim/")

#----------------------------------------------------------------------------

# Transition multipliers (non-Spatial) 
transmul_historic <- scenario(Definitions, scenario = "Transition Multipliers: Historic")
loadSheet("TransitionMultiplierValue", filename = "TransitionMultiplierValue_historic", 
          proj_or_sce=transmul_historic, path = "config/stsim/")

transmul_baseline <- scenario(Definitions, scenario = "Transition Multipliers: Baseline")
loadSheet("TransitionMultiplierValue", filename = "TransitionMultiplierValue_baseline", 
          proj_or_sce=transmul_baseline, path = "config/stsim/")

transmul_4.5 <- scenario(Definitions, scenario = "Transition Multipliers: RCP 4.5")
loadSheet("TransitionMultiplierValue", filename = "TransitionMultiplierValue_4.5", 
          proj_or_sce=transmul_4.5, path = "config/stsim/")

transmul_8.5 <- scenario(Definitions, scenario = "Transition Multipliers: RCP 8.5")
loadSheet("TransitionMultiplierValue", filename = "TransitionMultiplierValue_8.5", 
          proj_or_sce=transmul_8.5, path = "config/stsim/")

#----------------------------------------------------------------------------

# Spatial multipliers
spatial_multiplier_default <-
  scenario(Definitions,scenario = "Spatial Multiplier: Default")
loadSheet("TransitionSpatialMultiplier", NULL, spatial_multiplier_default,
          params = list(TransitionGroupID = c("Urbanisation",
                                              "Agricultural Expansion Gr"), 
                        MultiplierFileName = c(paste0(getwd(), "/data/stsim/spatial_multipliers/",
                                                      R_METHOD_STSIM, "_ratio_", R_RATIO, "_urb_f_spamul.tif"),
                                               paste0(getwd(), "/data/stsim/spatial_multipliers/",
                                                      R_METHOD_STSIM, "_ratio_", R_RATIO, "_agex_f_spamul.tif"))))

# Limited to corrs
# first need to do the multiplication 
# ~~~~~~
corrs_and_areas <- raster("data/stsim/spatial_multipliers/corrs_and_areas.tif")

urb <- raster(paste0("data/stsim/spatial_multipliers/",
                     R_METHOD_STSIM, "_ratio_", R_RATIO, "_urb_f_spamul.tif"))
agex <- raster(paste0("data/stsim/spatial_multipliers/",
                      R_METHOD_STSIM, "_ratio_", R_RATIO, "_agex_f_spamul.tif"))

urb_times_corrs_and_areas <- urb*corrs_and_areas
agex_times_corrs_and_areas <- agex*corrs_and_areas

writeRaster(urb_times_corrs_and_areas, 
            "data/stsim/spatial_multipliers/urb_f_corrs_and_areas_spa_mul.tif", 
            overwrite = TRUE)
writeRaster(agex_times_corrs_and_areas, 
            "data/stsim/spatial_multipliers/agex_f_corrs_and_areas_spa_mul.tif", 
            overwrite = TRUE)
# ~~~~~~

# Now scenario
spatial_multiplier_corrs <-
  scenario(Definitions,scenario = "Spatial Multiplier: Corrs/Areas")
loadSheet("TransitionSpatialMultiplier", NULL, spatial_multiplier_corrs,
          params = list(TransitionGroupID = c("Urbanisation",
                                              "Agricultural Expansion Gr",
                                              "Urbanisation",
                                              "Agricultural Expansion Gr"), 
                        Timestep = c(1,1,3,3), # ASSUMES 10 YEARS TIMESTEP
                        MultiplierFileName = c(paste0(getwd(), "/data/stsim/spatial_multipliers/",
                                                      R_METHOD_STSIM, "_ratio_", R_RATIO, "_urb_f_spamul.tif"),
                                               paste0(getwd(), "/data/stsim/spatial_multipliers/",
                                                      R_METHOD_STSIM, "_ratio_", R_RATIO, "_agex_f_spamul.tif"),
                                               paste0(getwd(), 
                                                      "/data/stsim/spatial_multipliers/urb_f_corrs_and_areas_spa_mul.tif"),
                                               paste0(getwd(),
                                                      "/data/stsim/spatial_multipliers/agex_f_corrs_and_areas_spa_mul.tif")
                        )))

spatial_multiplier_corrs_reforestation <-
  scenario(Definitions,scenario = "Spatial Multiplier: Corrs/Areas + Reforestation")
loadSheet("TransitionSpatialMultiplier", NULL, spatial_multiplier_corrs_reforestation,
          params = list(TransitionGroupID = c("Urbanisation",
                                              "Agricultural Expansion Gr",
                                              "Urbanisation",
                                              "Agricultural Expansion Gr", 
                                              "Reforestation Gr"), 
                        Timestep = c(1,1,3,3,3),
                        MultiplierFileName = c(paste0(getwd(), "/data/stsim/spatial_multipliers/",
                                                      R_METHOD_STSIM, "_ratio_", R_RATIO, "_urb_f_spamul.tif"),
                                               paste0(getwd(), "/data/stsim/spatial_multipliers/",
                                                      R_METHOD_STSIM, "_ratio_", R_RATIO, "_agex_f_spamul.tif"),
                                               paste0(getwd(), 
                                                      "/data/stsim/spatial_multipliers/urb_f_corrs_and_areas_spa_mul.tif"),
                                               paste0(getwd(),
                                                      "/data/stsim/spatial_multipliers/agex_f_corrs_and_areas_spa_mul.tif"),
                                               paste0(getwd(), "/data/stsim/spatial_multipliers/corrs_and_areas_allone.tif")
                        )))

# Transition size distribution
trans_size_distribution <- scenario(Definitions, scenario = "Transition Size Distribution: Default") 
loadSheet("TransitionSizeDistribution", NULL, trans_size_distribution, path = "config/stsim/")
loadSheet("TransitionSizePrioritization", NULL, trans_size_distribution, path = "config/stsim/")

# Transition Adjacency
transition_adjacency <- scenario(Definitions, scenario = "Transition Adjacency: Default")
loadSheet("StateAttributeType", NULL, transition_adjacency, path = "config/stsim/")
loadSheet("StateAttributeValue", NULL, transition_adjacency, path = "config/stsim/")
loadSheet("TransitionAdjacencySetting", NULL, transition_adjacency, path = "config/stsim/")
loadSheet("TransitionAdjacencyMultiplier", NULL, transition_adjacency, path = "config/stsim/")

# TST
TST_default <- scenario(Definitions, scenario = "TST: Default")
loadSheet("TimeSinceTransitionGroup", NULL, TST_default, path = "config/stsim/")
loadSheet("TimeSinceTransitionRandomize", NULL, TST_default, path = "config/stsim/")

# TPA
# TPA_default <- scenario(Definitions, scenario = "TPA: default")
# loadSheet("TransitionPathwayAutoCorrelation", NULL, TPA_default, path = "config/stsim/")

####################
## Full Scenarios ##
####################

# Want to compare the forest everywhere with the forest only in corridors

unchanging_sub_scenarios <- 
  c(deter_trans_default,
    output_default,
    transition_adjacency,
    trans_size_distribution,
    TST_default)

# Scenario BAU 1990-2010
historic_run <- scenario(Definitions, scenario = "historic run")
dependency(historic_run, c(run_historic,
                           init_ag_1990,
                           transmul_historic,
                           spatial_multiplier_default,
                           targets_default_no_ref_hist, 
                           unchanging_sub_scenarios))

# BAU 2010-2100
BAU_run_historic <- scenario(Definitions, scenario = "BAU run | historic")
dependency(BAU_run_historic, c(run_forecast,
                               init_ag_2010,
                               transmul_historic,
                               spatial_multiplier_default,
                               targets_default_no_ref,
                               unchanging_sub_scenarios))

BAU_run_baseline <- scenario(Definitions, scenario = "BAU run | baseline")
dependency(BAU_run_baseline, c(run_forecast,
                               init_ag_2010,
                               transmul_baseline,
                               spatial_multiplier_default,
                               targets_default_no_ref, 
                               unchanging_sub_scenarios))

BAU_run_8.5 <- scenario(Definitions, scenario = "BAU run | RCP 8.5")
dependency(BAU_run_8.5, c(run_forecast,
                          init_ag_2010,
                          transmul_8.5,
                          spatial_multiplier_default,
                          targets_default_no_ref, 
                          unchanging_sub_scenarios))

# BAU + ref
BAU_run_ref_historic <- scenario(Definitions, scenario = "BAU run + ref | historic")
dependency(BAU_run_ref_historic, c(run_forecast,
                                   init_ag_2010,
                                   transmul_historic,
                                   spatial_multiplier_default,
                                   targets_default_ref, 
                                   unchanging_sub_scenarios))

BAU_run_ref_baseline <- scenario(Definitions, scenario = "BAU run + ref | baseline")
dependency(BAU_run_ref_baseline, c(run_forecast,
                                   init_ag_2010,
                                   transmul_baseline,
                                   spatial_multiplier_default,
                                   targets_default_ref, 
                                   unchanging_sub_scenarios))

BAU_run_ref_8.5 <- scenario(Definitions, scenario = "BAU run + ref | RCP 8.5")
dependency(BAU_run_ref_8.5, c(run_forecast,
                              init_ag_2010,
                              transmul_8.5,
                              spatial_multiplier_default,
                              targets_default_ref, 
                              unchanging_sub_scenarios))

# BAU + protec
BAU_corr_protected_historic <- scenario(Definitions, scenario = "BAU run + corrs protection | historic")
dependency(BAU_corr_protected_historic, c(run_forecast,
                                          init_ag_2010,
                                          transmul_historic,
                                          spatial_multiplier_corrs,
                                          targets_default_no_ref, 
                                          unchanging_sub_scenarios))

BAU_corr_protected_baseline <- scenario(Definitions, scenario = "BAU run + corrs protection | baseline")
dependency(BAU_corr_protected_baseline, c(run_forecast,
                                          init_ag_2010,
                                          transmul_baseline,
                                          spatial_multiplier_corrs,
                                          targets_default_no_ref, 
                                          unchanging_sub_scenarios))

BAU_corr_protected_8.5 <- scenario(Definitions, scenario = "BAU run + corrs protection | RCP 8.5")
dependency(BAU_corr_protected_8.5, c(run_forecast,
                                     init_ag_2010,
                                     transmul_8.5,
                                     spatial_multiplier_corrs,
                                     targets_default_no_ref, 
                                     unchanging_sub_scenarios))
# BAU + protec + random ref 
REF_corr_protected_ref_historic <- scenario(Definitions, scenario = "BAU run + corrs protection + ref | historic")
dependency(REF_corr_protected_ref_historic, c(run_forecast,
                                              init_ag_2010,
                                              transmul_historic,
                                              spatial_multiplier_corrs,
                                              targets_default_ref, 
                                              unchanging_sub_scenarios))

REF_corr_protected_ref_baseline <- scenario(Definitions, scenario = "BAU run + corrs protection + ref | baseline")
dependency(REF_corr_protected_ref_baseline, c(run_forecast,
                                              init_ag_2010,
                                              transmul_baseline,
                                              spatial_multiplier_corrs,
                                              targets_default_ref, 
                                              unchanging_sub_scenarios))

REF_corr_protected_ref_8.5 <- scenario(Definitions, scenario = "BAU run + corrs protection + ref | RCP 8.5")
dependency(REF_corr_protected_ref_8.5, c(run_forecast,
                                         init_ag_2010,
                                         transmul_8.5,
                                         spatial_multiplier_corrs,
                                         targets_default_ref,
                                         unchanging_sub_scenarios))

# BAU + protec + targeted ref

REF_corr_protected_ref_targeted_historic <- scenario(Definitions, scenario = "BAU run + corrs protection + ref TARGETED | historic")
dependency(REF_corr_protected_ref_targeted_historic, c(run_forecast,
                                                       init_ag_2010,
                                                       transmul_historic,
                                                       spatial_multiplier_corrs_reforestation,
                                                       targets_default_ref, 
                                                       unchanging_sub_scenarios))

REF_corr_protected_ref_targeted_baseline <- scenario(Definitions, scenario = "BAU run + corrs protection + ref TARGETED | baseline")
dependency(REF_corr_protected_ref_targeted_baseline, c(run_forecast,
                                                       init_ag_2010,
                                                       transmul_baseline,
                                                       spatial_multiplier_corrs_reforestation,
                                                       targets_default_ref, 
                                                       unchanging_sub_scenarios))

REF_corr_protected_ref_targeted_8.5 <- scenario(Definitions, scenario = "BAU run + corrs protection + ref TARGETED | RCP 8.5")
dependency(REF_corr_protected_ref_targeted_8.5, c(run_forecast,
                                                  init_ag_2010,
                                                  transmul_8.5,
                                                  spatial_multiplier_corrs_reforestation,
                                                  targets_default_ref, 
                                                  unchanging_sub_scenarios))

#########
## RUN ##
#########

if (STSIM_RUN){
  print("Running StSIM");Sys.time()
  
  results <- data.frame()
  
  all_sce <- 
    list(
      historic_run, #38
      
      BAU_run_historic, #39 
      BAU_run_baseline, #40
      BAU_run_8.5, #41
      
      BAU_run_ref_historic, #42 
      BAU_run_ref_baseline, #43
      BAU_run_ref_8.5, #44
      
      BAU_corr_protected_historic, #45 
      BAU_corr_protected_baseline, #46
      BAU_corr_protected_8.5, #47
      
      REF_corr_protected_ref_historic, #48 
      REF_corr_protected_ref_baseline, #49
      REF_corr_protected_ref_8.5, #50
      
      REF_corr_protected_ref_targeted_historic, #51 
      REF_corr_protected_ref_targeted_baseline, #52
      REF_corr_protected_ref_targeted_8.5 #53
    )
  
  for(sce in all_sce){
    
    temp_res <- run(sce,
        summary = TRUE, jobs = OMP_NUM_THREADS)
    results <- bind_rows(results, temp_res)
    
  }
  
  print(results)
  saveRDS(results, "data/temp/stsim_run_results.RDS")
  write_csv(results, "outputs/final/stsim_run_results.csv")
}
