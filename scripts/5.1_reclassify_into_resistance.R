#-------------------------------------------------------------------------------
## 5.1 Reclassify
## 2020
## Inputs: stsim outputs
## Outputs: reclassed rasters
#-------------------------------------------------------------------------------
rm(list=ls())
# Set parameters 
aggregation <- list(ag = as.logical(Sys.getenv("R_AGGR")), 
                    factor = as.numeric(Sys.getenv("R_AGGR_FACT")))
STSIM_ITER <- as.numeric(Sys.getenv("STSIM_ITER"))

R_AGGR_FACT <- as.numeric(Sys.getenv("R_AGGR_FACT"))
STSIM_STEP_SAVE <- as.numeric(Sys.getenv("STSIM_STEP_SAVE"))
#-------------------------------------------------------------------------------

# aggregation <- list(ag = TRUE,
#                     factor = 3)
# STSIM_ITER <- 2
# 
# R_AGGR_FACT <- 3
# STSIM_STEP_SAVE <- 1

# Load important libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(raster)
  library(gtools)
})

# Reset Raster tmp files
removeTmpFiles(0)
showTmpFiles()

#-------------------------------------------------------------------------------

## Load inputs
# Get result secenario directory 
sce_dir_vec <- list.files("libraries/stsim/monteregie-conncons-scripted.ssim.output", 
                          full.names = T)
sce_nb_vec <- as.numeric(unlist(lapply(str_split(sce_dir_vec, "-"), FUN = last)))

# Template
iter_template <- paste0("it", 1:STSIM_ITER)

## Load classification matrices
species_list <- tools::file_path_sans_ext(list.files("config/rcl_tables/species/"))
list_rcl <- list.files("config/rcl_tables/species/", full.names = T)
rcls <- lapply(list_rcl, read.csv) 
names(rcls) <- species_list

#-------------------------------------------------------------------------------

# TRUE LAND USE ONLY NEEDED ONCE
if (aggregation$ag){
  true_landuse_list <- list.files("data/land_use/aggregated/", full.names=T)
} else{
  true_landuse_list <- list.files("data/land_use/", full.names=T)
}

true_landuse <- lapply(true_landuse_list, FUN = raster)
names(true_landuse) <- c("lu_1990", "lu_2000", "lu2010")

## Reclassify and rename
print("Reclassifying") ; Sys.time()
# initial_reclassed <- lapply(rcls, reclassify, x=initial)

# Looping TRUE 
print("TRUE LAND USE")
true_landuse_reclassed <- list()
for (ts in 1:length(true_landuse)){
  true_landuse_reclassed[[names(true_landuse)[ts]]] <- list()
  for (spe in 1:length(rcls)){
    true_landuse_reclassed[[names(true_landuse)[ts]]][[names(rcls)[spe]]] <- 
      reclassify(true_landuse[[ts]], rcls[[spe]])
    
    writeRaster(true_landuse_reclassed[[ts]][[spe]],
                paste0("outputs/reclassed/TRUE_lu_",ts,"_", names(rcls)[spe]),
                format="GTiff", overwrite =T)
  }
}
print(true_landuse_reclassed)

#-------------------------------------------------------------------------------

print("MODELLED LAND USE")
# Loop all scenarios (OBS)
for (sce in 1:length(sce_dir_vec)){
  
  sce_dir <- sce_dir_vec[sce]
  sce_nb <- sce_nb_vec[sce]
  
  print(sce_dir)
  print(sce_nb)
  
  # Get file list and iteration memberships
  files_iter_list <- 
    mixedsort(list.files(paste0(sce_dir, "/stsim_OutputSpatialState"),
                         full.names = T))
  print(files_iter_list)
  
  # Determibe ts template for this iteration
  ts_vec <- unique(as.numeric(unlist( # as numeric cause the whole thing
    lapply(                           # returns a string
      str_split(                      # also get the unique values
        unlist(
          lapply(
            unlist(
              lapply(files_iter_list,str_split,"\\."), # split after "it1.ts" 
              recursive = F), # unlist, but only one level
            nth, n=5)), # get number 5 (where the ts nb is)
        "s"), nth, # split after s
      n = 2)))) # get number 2
  print(ts_vec)
  
  ts_template <- unique(paste0("ts_", seq(min(ts_vec), #from min for this sce
                                          max(ts_vec), # to max
                                          STSIM_STEP_SAVE))) # by whatever we save
  print(ts_template)
  
  # First split by iterations
  iter_membreships <- lapply(X=paste0(iter_template, "\\."), FUN=grep, x=files_iter_list)
  split_by_iter <- lapply(seq_along(iter_membreships), 
                          function(x){files_iter_list[iter_membreships[[x]]]})
  names(split_by_iter) <- iter_template
  
  # Load rasters 
  split_by_iter_rasters <- lapply(split_by_iter, FUN=stack) # Will take up memory
  
  print("-----")
  
  # Looping OBSERVATIONS
  for (it in 1:length(split_by_iter_rasters)){
    for (spe in 1:length(rcls)){
      temp <- reclassify(split_by_iter_rasters[[it]], rcls[[spe]])
      print(spe)
      # print(temp)
      for (ts in ts_vec){
        idx <- which(ts_vec==ts)
        writeRaster(temp[[idx]],
                    paste0("outputs/reclassed/", 
                           paste("sce",sce_nb,"it",it,ts_template[idx],
                                 species_list[spe],"", sep="_")), 
                    format="GTiff", overwrite =T)
      }
    }
  }
}
#-------------------------------------------------------------------------------

## Build script for adding rows 
print("Building script") ; Sys.time()
reclassed_list <- tools::file_path_sans_ext(list.files("outputs/reclassed/"))

custom_paste <- function(file){
  paste0("sh ","scripts/0.1_add_buffer_lowlands_modified.sh ",
         "-i ", "outputs/reclassed/", file,".tif -b ",30*R_AGGR_FACT,
         " -o ", "outputs/reclassed_with_buffer/", file, "with_buffer ",
         "-e n")
}
first_line <- paste0("sh ","scripts/0.1_add_buffer_lowlands_modified.sh ",
                     "-i ", "outputs/reclassed/", reclassed_list[1],".tif -b ",30*R_AGGR_FACT,
                     " -o ", "outputs/reclassed_with_buffer/", "circuitscape_buffer ",
                     "-e y")
lines <- list(start="#!/bin/sh", first_line, lapply(reclassed_list, function(x) custom_paste(x)))

fileConn<-file("scripts/0.2_add_buffer_to_all.sh")
writeLines(unlist(lines), fileConn)
close(fileConn)

saveRDS(reclassed_list,"data/temp/reclassed_list.RDS")