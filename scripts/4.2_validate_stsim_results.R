#-------------------------------------------------------------------------------
## 4.2 Validate ST-SIM results
## 2020
## Inputs: stsim results
## Outputs: plots and etc..
#-------------------------------------------------------------------------------

rm(list=ls())
set.seed(77)

#ST_SIM_DIR <- "/home/vlucet/Documents/Apex/syncrosim_2_10"
ST_SIM_DIR <- "../syncrosim_2_10/"

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

sce_dir_vec <- list.files("libraries/stsim/monteregie-conncons-scripted.ssim.output", 
                          full.names = T)
sce_nb_vec <- as.numeric(unlist(lapply(str_split(sce_dir_vec, "-"), FUN = last)))

## DIAGNOSTICS 
results <- read_rds("data/temp/stsim_run_results.RDS")

if (length(results$scenarioId) > 1){
  
  for (sce in results$scenarioId[2:length(results$scenarioId)]){
    trans_results <- datasheet(myLibrary, scenario = sce, 
                               "stsim_OutputStratumTransition") %>% 
      filter(Timestep >= 3) %>% 
      group_by(Iteration, Timestep, StratumID, SecondaryStratumID, TransitionGroupID) %>% 
      summarise(Amount = sum(Amount)) %>% ungroup %>% 
      group_by(StratumID, SecondaryStratumID, TransitionGroupID) %>% 
      summarise(Amount_observed = mean(Amount)) %>% ungroup %>% 
      mutate(Timestep=3)
    
    # trans_results_ref <- datasheet(myLibrary, scenario = sce, 
    #                                "stsim_OutputStratumTransition") %>% 
    #   filter((TransitionGroupID %in% c("Reforestation [Type]", "Reforestation Gr"))) %>% 
    #   filter(Timestep >= 3) %>% 
    #   group_by(Iteration, Timestep, StratumID, TransitionGroupID) %>% 
    #   summarise(Amount = sum(Amount)) %>% ungroup %>% 
    #   group_by(StratumID, TransitionGroupID) %>% 
    #   summarise(Amount_observed = mean(Amount)) %>% ungroup %>% 
    #   mutate(Timestep=3)
    # 
    # print(trans_results_ref)
    
    # test <-  datasheet(myLibrary, scenario = sce, "stsim_OutputStratumTransition") 
    # test %>% filter(StratumID == "PA") %>% pull(TransitionGroupID) %>% droplevels() %>%  table
    # test %>% filter(StratumID == "Not_Monteregie") %>% pull(TransitionGroupID) %>% droplevels() %>%  table
    
    targets <- datasheet(myLibrary, scenario = sce, 
                         "stsim_TransitionTarget") %>% 
      filter(Timestep >= 3) %>% 
      rename(Amount_targeted = Amount)
    
    targets_ref <- targets  %>% 
      filter((TransitionGroupID %in% c("Reforestation [Type]", "Reforestation Gr")))
    
    print(targets_ref)
    
    # OLD PLOT when runs were 1990-2100
    
    # theplot <- trans_results_toplot %>% 
    #   filter(TransitionGroupID != "Urbanisation", ScenarioID==sce) %>% 
    #   ggplot(aes(x=Timestep, y=Amount_mean)) +
    #   geom_line(show.legend = F) + 
    #   facet_grid_paginate(TransitionGroupID~SecondaryStratumID, nrow=3, ncol=5, page = 12, scales = "free") +
    #   geom_line(data=targets, inherit.aes = T, linetype=2)
    # ggsave(theplot, filename = paste0("outputs/figures/sce_",sce,"_one_to_one_mun.png"))
    
    # JOINING
    
    gen_plot_df <- left_join(trans_results, targets) %>% 
      arrange(Timestep) %>% 
      replace_na(list(Amount_targeted = 0, Amount_observed = 0))
    
    gen_plot_df <- 
      gen_plot_df[!grepl(gen_plot_df$TransitionGroupID, pattern = "_to_"),]
    
    the_plot <- gen_plot_df %>% 
      #filter(ScenarioID == 9) %>% 
      filter(!(TransitionGroupID %in% c("Reforestation [Type]", "Reforestation Gr"))) %>% 
      ggplot(aes(x=Amount_observed, y=Amount_targeted, 
                 color=as.factor(SecondaryStratumID))) +
      geom_point(show.legend = F) +
      geom_abline(slope = 1, intercept = 0, linetype=2) +
      facet_wrap(vars(TransitionGroupID)) +
      NULL
    print(the_plot)
    ggsave(paste0("outputs/figures/sce_",sce,"_one_to_one_targets.png"))
    
    summary(lm(Amount_targeted~Amount_observed, drop_na(gen_plot_df)))
  }
  # now take care of the first one
  sce <- results$scenarioId[1]
  
  trans_results <- datasheet(myLibrary, scenario = sce, 
                             "stsim_OutputStratumTransition") %>% 
    group_by(Iteration, Timestep, StratumID, SecondaryStratumID, TransitionGroupID) %>% 
    summarise(Amount = sum(Amount)) %>% ungroup %>% 
    group_by(Timestep, StratumID, SecondaryStratumID, TransitionGroupID) %>% 
    summarise(Amount_observed = mean(Amount))
  
  targets <- datasheet(myLibrary, scenario = sce, 
                       "stsim_TransitionTarget") %>% 
    rename(Amount_targeted = Amount)
  
  # JOINING
  
  gen_plot_df <- left_join(trans_results, targets) %>% 
    arrange(Timestep) %>% 
    replace_na(list(Amount_targeted = 0, Amount_observed = 0))
  
  gen_plot_df <- 
    gen_plot_df[!grepl(gen_plot_df$TransitionGroupID, pattern = "_to_"),]
  
  the_plot <- gen_plot_df %>% 
    #filter(ScenarioID == 9) %>% 
    filter(!(TransitionGroupID %in% c("Reforestation [Type]", "Reforestation Gr"))) %>% 
    ggplot(aes(x=Amount_observed, y=Amount_targeted, 
               color=as.factor(SecondaryStratumID))) +
    geom_point(show.legend = F) +
    geom_abline(slope = 1, intercept = 0, linetype=2) +
    facet_wrap(vars(TransitionGroupID)) +
    NULL
  print(the_plot)
  ggsave(paste0("outputs/figures/sce_",sce,"_one_to_one_targets.png"))
  
  summary(lm(Amount_targeted~Amount_observed, drop_na(gen_plot_df)))
  
} else {
  
  sce <- results$scenarioId[1]
  
  trans_results <- datasheet(myLibrary, scenario = sce, 
                             "stsim_OutputStratumTransition") %>% 
    group_by(Iteration, Timestep, StratumID, SecondaryStratumID, TransitionGroupID) %>% 
    summarise(Amount = sum(Amount)) %>% ungroup %>% 
    group_by(Timestep, StratumID, SecondaryStratumID, TransitionGroupID) %>% 
    summarise(Amount_observed = mean(Amount))
  
  targets <- datasheet(myLibrary, scenario = sce, 
                       "stsim_TransitionTarget") %>% 
    rename(Amount_targeted = Amount)
  
  # JOINING
  
  gen_plot_df <- left_join(trans_results, targets) %>% 
    arrange(Timestep) %>% 
    replace_na(list(Amount_targeted = 0, Amount_observed = 0))
  
  gen_plot_df <- 
    gen_plot_df[!grepl(gen_plot_df$TransitionGroupID, pattern = "_to_"),]
  
  the_plot <- gen_plot_df %>% 
    #filter(ScenarioID == 9) %>% 
    filter(!(TransitionGroupID %in% c("Reforestation [Type]", "Reforestation Gr"))) %>% 
    ggplot(aes(x=Amount_observed, y=Amount_targeted, 
               color=as.factor(SecondaryStratumID))) +
    geom_point(show.legend = F) +
    geom_abline(slope = 1, intercept = 0, linetype=2) +
    facet_wrap(vars(TransitionGroupID)) +
    NULL
  print(the_plot)
  ggsave(paste0("outputs/figures/sce_",sce,"_one_to_one_targets.png"))
  
  summary(lm(Amount_targeted~Amount_observed, drop_na(gen_plot_df)))
  
}
