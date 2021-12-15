#-------------------------------------------------------------------------------
## 7.1 Process results
## 2020
## Inputs: connectivity rasters
## Outputs: data to plot
#-------------------------------------------------------------------------------

rm(list = ls())
set.seed(77)

#-------------------------------------------------------------------------------
# Set parameters
STSIM_ITER <- as.numeric(Sys.getenv("STSIM_ITER", unset=10))
aggregation <- list(ag = as.logical(Sys.getenv("R_AGGR", unset = TRUE)), 
                    factor = as.numeric(Sys.getenv("R_AGGR_FACT", unset = 3)))
STSIM_STEP_SAVE <- as.numeric(Sys.getenv("STSIM_STEP_SAVE", unset = 1))
#-------------------------------------------------------------------------------

print(c(STSIM_ITER, aggregation, STSIM_STEP_SAVE))

# Load important libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(raster)
  library(RStoolbox)
  library(SDMTools)
  library(ggplot2)
  library(assertthat)
  library(foreach)
  library(doParallel)
})

#-------------------------------------------------------------------------------

# Source functions and remove temp files
source("scripts/functions/rescale_raster_layer.R")
source("scripts/functions/get_bar_df.R")

removeTmpFiles(h = 0)
showTmpFiles()
rasterOptions(tmpdir = "data/temp/raster_tmp")

# For zonal statistics
if (aggregation$ag) {
  mun_zonal <- raster("data/stsim/aggregated/secondary_stratun_mun.tif")
} else {
  stop()
}
key <- read.csv("config/stsim/SecondaryStratum.csv")

# For removing outside NAs
# template <- raster("outputs/reclassed_with_buffer/circuitscape_buffer.tif")

#-------------------------------------------------------------------------------

# Get result secenario directory 
sce_dir_vec <- list.files("libraries/stsim/monteregie-conncons-scripted.ssim.output", 
                          full.names = T)[-c(2,5,8,11,14)]
sce_nb_vec <- paste0("sce_", as.numeric(unlist(lapply(str_split(sce_dir_vec, "-"), 
                                                      FUN = last))))
print(sce_nb_vec)

# Templates
iter_template <- paste0("it_", 1:STSIM_ITER, "_")
#-------------------------------------------------------------------------------

# Load raster inputs (maybe this will take too much ram?)
species_vec <- tools::file_path_sans_ext(list.files("config/rcl_tables/species/"))
#list_files <- list.files("outputs/current_density", full.names = T, pattern = "curmap")
list_files <- c(list.files("../current_density_2", full.names=T, pattern = "curmap"),
                list.files("outputs/current_density", full.names = T, pattern = "curmap"))
# list_files <- c(list.files("/home/ubuntu/data/val/current_density_dump", full.names=T, pattern = "curmap"),
#                 list.files("outputs/current_density", full.names = T, pattern = "curmap"))
#-------------------------------------------------------------------------------

# Make a function that determines the ts_template for the iteration

# if(detectCores() > (length(sce_dir_vec)+2)){
#   n_cores <- length(sce_dir_vec)
# } else {
#   n_cores <- detectCores()-2
# }
n_cores = 5 # very important smaller number of cores!!
clust <- makeCluster(n_cores, outfile="log.txt")
registerDoParallel(cl = clust)

#-------------------------------------------------------------------------------

get_ts_template <- function(list_of_files, step_save) {
  
  ts_vec <- gtools::mixedsort(unique(as.numeric(
    unlist(lapply(
      unlist(lapply(basename(list_of_files), str_split,"_"), # split after all _ 
             recursive = F), # unlist, but only one level
      nth, n=6))
  )))
  print(ts_vec)
  ts_template <- 
    gtools::mixedsort(unique(paste0("ts_", seq(min(ts_vec), #from min for this sce
                                               max(ts_vec), # to max
                                               step_save), "_"))) # by whatever we save
  print(ts_template)
  return(list(ts_vec, ts_template))
  
}

#-------------------------------------------------------------------------------
if (FALSE) {
final_df <- foreach(sce = sce_nb_vec, .combine = dplyr::bind_rows) %dopar% {
  
  library(tidyverse)
  library(raster)
  library(gtools)
  library(assertthat)
  
  print(sce)
  sub_list_files <- list_files[grepl(x = list_files, pattern = sce)]
  templates <- get_ts_template(sub_list_files, STSIM_STEP_SAVE)
  ts_template <- templates[[1]]
  ts_vec <- templates[[2]]
  
  spe_list <- list() 
  for (species in species_vec) {
    print(species)
    ts_list <- list()
    for (timestep in ts_vec) {
      print(timestep)
      iter_list <- list()
      for (iter in iter_template) {
        print(iter)
        # Step 1 assemble NS and EW for each spe, ts, iter, sce
        stack_files <- list_files[grepl(x = list_files, pattern = species) & 
                                    grepl(x = list_files, pattern = timestep) & 
                                    grepl(x = list_files, pattern = iter) &
                                    grepl(x = list_files, pattern = sce)]
        #print(length(stack_files))
        print(stack_files)
        assert_that(length(stack_files)==2)
        # Step 2 sum NS and EW
        iter_list[[iter]] <- sum(stack(lapply(stack_files, FUN = raster)))      
      }
      
      idx <- which(ts_vec == timestep)
      
      unit_rasters_cropped <- crop(stack(iter_list), mun_zonal)
      
      df2 <- as.data.frame(zonal(x = unit_rasters_cropped, 
                                 z = mun_zonal, 
                                 fun = "mean", na.rm = TRUE)) 
      df2$sce <- sce
      df2$zone <- as.factor(df2$zone)
      df2$species <- species
      df2$timestep <- ts_template[which(ts_vec == timestep)]
      # df2$iteration <- iter
      print(head(df2))

      # final <- df2 %>% rename(current = value)
      
      final <- df2 %>% 
        pivot_longer(cols = contains("it"), names_to = "iteration", 
                     values_to = "current")
      
      ts_list[[which(ts_vec == timestep)]] <- final
    }
    spe_list[[which(species == species_vec)]] <- bind_rows(ts_list)
  }
  removeTmpFiles(h=0)
  bind_rows(spe_list)
}
stopCluster(clust)

print("LARGE FOREACH LOOP DONE")
# Save final df
saveRDS(final_df, "outputs/final/final_df_current_density.RDS")

rows <- dim(final_df)[1]
limit <- round(rows/2)

write_csv(final_df[1:limit,], "outputs/final/final_df_current_density_part1.csv")
write_csv(final_df[limit:rows,], "outputs/final/final_df_current_density_part2.csv")
}
#-------------------------------------------------------------------------------
if (TRUE) {
n_cores = 8 # very important smaller number of cores!!
clust <- makeCluster(n_cores, outfile="log.txt")
registerDoParallel(cl = clust)

foreach(sce = sce_nb_vec) %dopar% {
  
  library(tidyverse)
  library(raster)
  library(gtools)
  library(assertthat)
  
  print(sce)
  sub_list_files <- list_files[grepl(x = list_files, pattern = sce)]
  templates <- get_ts_template(sub_list_files, STSIM_STEP_SAVE)
  ts_template <- templates[[1]]
  ts_vec <- templates[[2]]
  
  spe_list <- list() 
  for (species in species_vec) {
    print(species)
    ts_list <- list()
    for (timestep in ts_vec) {
      print(timestep)
      iter_list <- list()
      
      idx <- which(ts_vec == timestep)
      
      for (iter in iter_template) {
        print(iter)
        # Step 1 assemble NS and EW for each spe, ts, iter, sce
        stack_files <- list_files[grepl(x = list_files, pattern = species) & 
                                    grepl(x = list_files, pattern = timestep) & 
                                    grepl(x = list_files, pattern = iter) &
                                    grepl(x = list_files, pattern = sce)]
        #print(length(stack_files))
        print(stack_files)
        assert_that(length(stack_files)==2)
        # Step 2 multiply NS and EW
        calculated <- calc(crop(stack(lapply(stack_files, FUN = raster)), 
                                mun_zonal), 
                           function(x){x[[1]]+x[[2]]}) # * or + !!!
        the_name <- paste0(sce,"_", timestep, iter, species,".tif")
        print(the_name)
        writeRaster(calculated, file.path("outputs/current_density_sum/", the_name), 
                    overwrite=T)
      }
    }
  }
  removeTmpFiles(h=0)
}
}
#-------------------------------------------------------------------------------
if (TRUE) {
# Now with original data - simple loop this time
list_files_origin <- list_files[grepl(x = list_files, pattern = "TRUE")]

assembled_list_T <- list()
extracted_list_T <- list()

for (species in species_vec) {
  extracted_list_T=list()
  print(species)
  for (timestep in 0:2){
    print(timestep)
    stack_files <- list_files_origin[grepl(x = list_files_origin, pattern = species) & 
                                       grepl(x = list_files_origin, 
                                             pattern = paste0("lu_",timestep,"_"))]
    assert_that(length(stack_files)==2)
    print(stack_files)
    sum.of.rasters <- sum(stack(lapply(stack_files, FUN = raster::raster)))
    
    ## ALSO save sum here
    the_name <- paste0("TRUE_", timestep, "_", species,".tif")
    print(the_name)
    writeRaster(crop(sum.of.rasters, mun_zonal), 
                file.path("outputs/current_density_sum/", the_name), 
                overwrite=T)
    ##
    
    df <- as.data.frame(zonal(x = crop(sum.of.rasters, mun_zonal), mun_zonal), 
                        z = mun_zonal, fun = "mean", na.rm = T)
    df$zone <- as.factor(df$zone)
    df$species <- species
    df$timestep <- timestep
    
    extracted_list_T[[timestep+1]] <- df
  }
  assembled_list_T[[species]] <- extracted_list_T
}

saveRDS(assembled_list_T, "outputs/final/final_raster_means_TRUE.RDS")

final_df_origin <- bind_rows(unlist(assembled_list_T, recursive = F))

final_df_origin$timestep <- as.numeric(final_df_origin$timestep)
saveRDS(final_df_origin, "outputs/final/final_df_origin_current_density.RDS")
write_csv(final_df_origin, "outputs/final/final_df_origin_current_density.csv")

#-------------------------------------------------------------------------------
# BAR PLOT DATA 

output <- 
  get_bar_df(sce_folder="libraries/stsim/monteregie-conncons-scripted.ssim.output/")
write_csv(output, "outputs/final/final_bar_plot_data.csv")
}
#-------------------------------------------------------------------------------

# # FULL SUM FOR FINAL OUTPUTS
# full_stack=list()
# count <- 1
# for (sce in 1:length(assembled_list)) { 
#   temp <- assembled_list[[sce]]
#   print(sce)
#   for (timestep in 1:length(temp[[1]])) {
#     print(timestep)
#     
#     the_stack <- stack(lapply(temp, `[[`, timestep))
#     the_sum <- sum(the_stack)
#     
#     full_stack[[count]] <- the_sum
#     raster_name <- paste("sce", sce, "ts", timestep, sep = "_")
#     names(full_stack[[count]]) 
#     
#     writeRaster(full_stack[[count]],
#                 file.path("outputs", "current_density_sum", 
#                           paste0("full_sum_", raster_name, ".tif")), 
#                 overwrite=T)
#     
#     count <- count+1
#   }
#   saveRDS(full_stack, paste0("outputs/final/final_cur_sum_sce_",sce,"_per_ts.RDS"))
#}  