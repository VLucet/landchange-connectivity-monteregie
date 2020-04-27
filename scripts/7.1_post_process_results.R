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
STSIM_ITER <- as.numeric(Sys.getenv("STSIM_ITER"))
aggregation <- list(ag = as.logical(Sys.getenv("R_AGGR")), 
                    factor = as.numeric(Sys.getenv("R_AGGR_FACT")))
STSIM_STEP_SAVE <- as.numeric(Sys.getenv("STSIM_STEP_SAVE"))
STSIM_TS_START <- as.numeric(Sys.getenv("STSIM_TS_START"))
STSIM_TS_END <- as.numeric(Sys.getenv("STSIM_TS_END"))
#-------------------------------------------------------------------------------

##-- for shorter tests
# STSIM_STEP_SAVE=1
# STSIM_ITER=2
# STSIM_TS_START=0
# STSIM_TS_END=5
##--

print(c(STSIM_ITER, STSIM_TS_END, aggregation, STSIM_STEP_SAVE))

# Load important libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(raster)
  library(RStoolbox)
  library(SDMTools)
  library(ggplot2)
})
# suppressPackageStartupMessages(library(exactextractr))

#-------------------------------------------------------------------------------

# Source functions and remove temp files
source("scripts/functions/rescale_raster_layer.R")
removeTmpFiles(h = 0)
showTmpFiles()

# For zonal statistics
if (aggregation$ag) {
  mun_zonal <- raster("data/stsim/aggregated/secondary_stratum.tif")
} else {
  mun_zonal <- raster("data/stsim/secondary_stratun_mun_30by30.tif")
}
key <- read.csv("config/stsim/SecondaryStratum.csv")

# For removing outside NAs
template <- raster("outputs/reclassed_with_buffer/circuitscape_buffer.tif")

#-------------------------------------------------------------------------------

# Get result secenario directory 
sce_dir_vec <- list.files("libraries/stsim/monteregie-conncons-scripted.ssim.output", 
                          full.names = T)
sce_nb_vec <- as.numeric(unlist(lapply(str_split(sce_dir_vec, "-"), FUN = last)))

# Templates
iter_template <- paste0("it_", 1:STSIM_ITER, "_")
ts_template <- unique(paste0("ts_", c(STSIM_TS_START, seq(STSIM_STEP_SAVE, STSIM_TS_END, STSIM_STEP_SAVE)), "_"))
ts_steps <- as.numeric(unique(c(STSIM_TS_START, seq(STSIM_STEP_SAVE, STSIM_TS_END, STSIM_STEP_SAVE))))

#-------------------------------------------------------------------------------

# Load raster inputs (maybe this will take too much ram?)
species_vec <- tools::file_path_sans_ext(list.files("config/rcl_tables/species/"))
list_files <- list.files("outputs/current_density", full.names = T, pattern = "curmap")

# Split data repeteadly => for loop
assembled_list <- list()
extracted_list <- list()
full_list <- list()

for (sce in sce_nb_vec){
  print(sce)
  spe_list <- list() 
  for (species in species_vec) {
    print(species)
    ts_list <- list()
    for (timestep in ts_template) {
      print(timestep)
      iter_list <- list()
      for (iter in iter_template) {
        # Step 1 assemble NS and EW for each spe, ts, iter, sce
        stack_files <- list_files[grepl(x = list_files, pattern = species) & 
                                    grepl(x = list_files, pattern = timestep) & 
                                    grepl(x = list_files, pattern = iter) &
                                    grepl(x = list_files, pattern = sce)]
        # Step 2 sum NS and EW
        print(stack_files)
        iter_list[[iter]] <- sum(stack(lapply(stack_files, FUN = raster)))      
      }
      
      # Step 3 take the mean accross all iterations
      mean_raster <- mean(stack(iter_list))
      ts_list[[timestep]] <- mean_raster
      
      # Step 4 remove the NA default value
      mean_raster[template == 2] <- NA
      # mean_raster <- rescale_raster_layer(mean_raster)
      
      # Make zonal stats directly on the mean raster
      # df <- as.data.frame(zonal(x = crop(mean_raster, mun_zonal), mun_zonal), 
      #                     z = mun_zonal, fun = "mean", na.rm = T)
      mean_raster_cropped <- crop(mean_raster, mun_zonal)
      df <- as.data.frame(zonal(x = mean_raster_cropped, 
                                z = mun_zonal, 
                                fun = "mean", na.rm = TRUE))
      df$sce <- sce
      df$zone <- as.factor(df$zone)
      df$species <- species
      df$timestep <- ts_steps[which(ts_template == timestep)]
      
      raster_name <- paste(sce, species, timestep, sep = "_")
      names(mean_raster) <- raster_name
      
      print(head(df))
      
      extracted_list[[raster_name]] <- df
    }
    spe_list[[species]] <- ts_list
  }
  assembled_list[[paste0("sce_",sce)]] <- spe_list
}

# Save list
saveRDS(assembled_list, "outputs/final_raster_means.rds")
print("LARGE LOOP DONE")

# assemble last dataset
final_df <- extracted_list[[1]]
for (df in extracted_list[2:length(extracted_list)]) {
  final_df <- full_join(final_df, df, by = c("sce", "zone", "timestep", "species", "mean"))
}
final_df$timestep <- as.numeric(final_df$timestep)

# Save final df
saveRDS(final_df, "outputs/final_df_current_density.RDS")

# FULL SUM FOR FINAL OUTPUTS
full_stack=list()
count <- 1
for (sce in 1:length(assembled_list)) { 
  temp <- assembled_list[[sce]]
  print(sce)
  for (timestep in 1:length(temp)) {
    print(timestep)
    
    the_stack <- stack(lapply(temp, `[[`, timestep))
    the_sum <- sum(the_stack)
    
    full_stack[[count]] <- the_sum
    raster_name <- paste("sce", sce, "ts", timestep, sep = "_")
    names(full_stack[[count]]) 
    
    writeRaster(full_stack[[count]],
                file.path("outputs", "current_density_sum", 
                          paste0("full_sum_", file_name, ".tif")), 
                overwrite=T)
    
    count <- count+1
  }
}  

saveRDS(full_stack, "outputs/cur_sum_sce_per_ts.RDS")

#-------------------------------------------------------------------------------

# Now with original data
list_files_origin <- list_files[grepl(x = list_files, pattern = "TRUE")]

assembled_list_T <- list()
extracted_list_T <- list()

for (species in species_vec) {
  extracted_list_T=list()
  for (timestep in 1:3){
    stack_files <- list_files_origin[grepl(x = list_files_origin, pattern = species) & 
                                       grepl(x = list_files_origin, 
                                             pattern = paste0("lu_",timestep,"_"))]
    print(stack_files)
    sum.of.rasters <- sum(stack(lapply(stack_files, FUN = raster::raster)))
    
    df <- as.data.frame(zonal(x = crop(sum.of.rasters, mun_zonal), mun_zonal), 
                        z = mun_zonal, fun = "mean", na.rm = T)
    df$zone <- as.factor(df$zone)
    df$species <- species
    df$timestep <- timestep
    
    extracted_list_T[[timestep]] <- df
  }
  assembled_list_T[[species]] <- extracted_list_T
}

saveRDS(assembled_list_T, "outputs/final_raster_means_TRUE.rds")

unlisted <- unlist(assembled_list_T, recursive = FALSE)
final_df_origin <- unlisted[[1]]
for (df in unlisted[2:length(unlisted)]) {
  final_df_origin <- full_join(final_df_origin, df, by = c("zone", "timestep", "species", "mean"))
}

final_df_origin$timestep <- as.numeric(final_df_origin$timestep)
saveRDS(final_df_origin, "outputs/final_df_origin_current_density.RDS")

#-------------------------------------------------------------------------------

# DATA VIZ
png("outputs/figures/final_graph.png")
ggplot(final_df) + aes(x = timestep, y = mean, color = zone) + geom_line(show.legend = FALSE) + 
  scale_color_viridis_d(option = "plasma") + theme_minimal() + facet_wrap(vars(species))
dev.off()
