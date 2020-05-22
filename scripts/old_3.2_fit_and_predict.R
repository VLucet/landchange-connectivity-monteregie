#-------------------------------------------------------------------------------
## 3.2 Fit and predict model
## 2020
## Inputs: Dataframe etc.
## Outputs: Spatial multipliers, model fit
#-------------------------------------------------------------------------------

rm(list=ls())
set.seed(77)

#-------------------------------------------------------------------------------
# Set parameters 
OMP_NUM_THREADS <- as.numeric(Sys.getenv("OMP_NUM_THREADS"))-1
options(mc.cores=OMP_NUM_THREADS)
R_METHOD <- Sys.getenv("R_METHOD")
# R_CROP <- as.logical(Sys.getenv("R_CROP")) => never got implemented 
R_N_TREES <- as.numeric(Sys.getenv("R_N_TREES"))

options(stringsAsFactors = FALSE)

if (is.na(OMP_NUM_THREADS)) { 
  OMP_NUM_THREADS <- 6 
  options(mc.cores = OMP_NUM_THREADS)
  R_N_TREES <- 1000
  print("Running on 6 cores only and using default input parameters") 
  setwd("~/Documents/Master/landuse_model_full/landuse_model_workflow/")
}
#-------------------------------------------------------------------------------

# Set Working directory
# wd <- "~/home/vlucet/scratch/oct19_landuse_model"
# setwd(wd)

## Load required packages ##
suppressPackageStartupMessages({
  library(raster)
  library(parallel)
  library(mgcv)
  library(ranger)
  library(dplyr)
  library(tidyr)
  library(ROCR)
  library(brms)
  library(keras)
  library(rlist)
})

# Reset Raster tmp files
removeTmpFiles(0)
showTmpFiles()

## Source functions
source("scripts/functions/get_change_raster.R")
source("scripts/functions/train_NN_model.R")
source("scripts/functions/custom_perf.R")
source("scripts/functions/custom_fit_and_predict.R")

#-------------------------------------------------------------------------------
# Load inputs
data <- readRDS("data/temp/data_temp.RDS")

lu.template <- raster("data/temp/template.tif")
lu.buffer.template <- raster("data/temp/template_with_buffer.tif")

#-------------------------------------------------------------------------------
# Models
if (R_METHOD == "all"){
  methodlist <- c("glm", "nn", "rf") #  "brms" "gam"
} else {
  methodlist <- c(R_METHOD)
}

responselist <- c("urb", "agex")
samplingmethodlist <- c("bal", "imbal")
measureslist <- c("auc", "rch", "fpr", "tpr", "prec")

# Emmpty objects for collection
auc.df <- data.frame()

for (method in methodlist){
  
  outdir <- file.path("outputs", method)
  
  if (method == "gam"){
    # different formula for gam
    formulalist <- 
      list(urb = "urb~s(pop_change_norm, k=6)+s(urb_norm, k=6)+s(in_norm, k=6)+s(for_norm, k=6)+s(elev_norm, k=6)+s(ag_neigh_norm, k=6)+s(for_neigh_norm, k=6)+s(urb_neigh_norm, k=6)+s(roa_neigh_norm, k=6)",
           agex = "agex~s(pop_change_norm, k=6)+s(urb_norm, k=6)+s(in_norm, k=6)+s(for_norm, k=6)+s(elev_norm, k=6)+s(ag_neigh_norm, k=6)+s(for_neigh_norm, k=6)+s(urb_neigh_norm, k=6)+s(roa_neigh_norm, k=6)")
  } else {
    # same formula for the remaining methods
    formulalist <- 
      list(urb = "urb~pop_change_norm+urb_norm+in_norm+for_norm+elev_norm+ag_neigh_norm+for_neigh_norm+urb_neigh_norm+roa_neigh_norm", 
           agex = "agex~pop_change_norm+urb_norm+in_norm+for_norm+elev_norm+ag_neigh_norm+for_neigh_norm+urb_neigh_norm+roa_neigh_norm")
  }
  
  for (response in responselist){
    
    for(samplingmethod in samplingmethodlist){
      
      message(paste("Using sampling method... ", samplingmethod)) ; Sys.time()
      
      if(samplingmethod == "imbal"){
        
        train_data <- data$imbal_train
        test_data <- data$imbal_test
        
      } else if (samplingmethod == "bal"){
        
        train_data <- data[[response]]$bal_train
        test_data <- data[[response]]$bal_test
        
      }
      
      message(paste("Train data has shape", dim(train_data), collapse = " "))
      message(paste("Test data has shape", dim(test_data), collapse = " "))
      
      fit.out <- custom_fit_and_predict(modelformula = formulalist[[response]], 
                                        method = method,
                                        samplingmethod = samplingmethod,
                                        train = train_data,
                                        test = test_data,
                                        
                                        IDs = data$frame_IDs,
                                        step_list = list(present = data$df.change, 
                                                         future = data$df.change.future),
                                        nb_cores = OMP_NUM_THREADS, 
                                        
                                        predict=T, 
                                        check_model=T, 
                                        make_plot = T, 
                                        save_rasters = T, 
                                        save_fit=T,
                                        
                                        template = lu.template, 
                                        template_buffer = lu.buffer.template,
                                        ntrees=R_N_TREES)
      
      saveRDS(fit.out, paste0(file.path("outputs", method), 
                              paste("/output", response, samplingmethod, sep="_"), 
                              ".rds"))
      
      # Calculate the performance
      # predictions
      message("Calculating performance...") ; Sys.time()
      
      # create empty list and df
      
      perf_list <- custom_perf(predictions_list = list(fit.out$prediction.train,
                                                       fit.out$prediction.test,
                                                       fit.out$raster.current, 
                                                       fit.out$raster.future),
                               labels_list = list(train_data[[response]], 
                                                  test_data[[response]],
                                                  data$df.change[[response]], 
                                                  data$df.change.future[[response]]),
                               predictions_steps = c("train", "test", "current", "future"),
                               method = method, 
                               samplingmethod = samplingmethod,
                               measures_list = measureslist)
      
      for(step in names(perf_list)){
        for (measure in measureslist){
          
          # to do plot_roc function
          # # Plots 
          # png(file.path(outdir, paste(response,samplingmethod,step,"roc.png", sep="_")))
          # plot(perf.out[[step]]$rch, col=2)
          # title(response)
          # text(0.8,0.2,paste("AUC =", round(perf.out[[step]]$auc@y.values[[1]], 2)))
          # dev.off()
          
          # as.double(perf.out[[step]]$auc@y.values[[1]]))
          
          # Build the performance table
          the_row <- data.frame(method=as.character(method), 
                                response=as.character(response),
                                samplingmethod=as.character(samplingmethod),
                                step=step,
                                measure=measure, 
                                value=as.double(perf_list[[step]][[measure]]))
          auc.df <- rbind(auc.df, the_row)
        }
      }
    }
  }
}


write.csv(auc.df, "outputs/AUC_table.csv",)

# perf.list <- readRDS("outputs/rf/rf_perf.rds")

# sum(!is.na(values(fit.list$urb$raster.current)))
# 1291174
# sum(!is.na((data$df.change$urb)))
# 1291174
# length(perf.list$urb$current$pred@predictions[[1]])
# 1291174
