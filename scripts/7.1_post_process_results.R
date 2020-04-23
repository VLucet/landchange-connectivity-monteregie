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
#STSIM_ITER=2
#STSIM_TS_END=2
##--

print(c(STSIM_ITER, STSIM_TS_END, aggregation, STSIM_STEP_SAVE))

# Load important libraries
suppressPackageStartupMessages({
  library(raster)
  library(dplyr)
  library(RStoolbox)
  library(SDMTools)
  library(ggplot2)
})
# suppressPackageStartupMessages(library(exactextractr))

# Source functions and remove temp files
source("scripts/functions/rescale_raster_layer.R")
showTmpFiles()
removeTmpFiles(h = 0)
showTmpFiles()

# Load raster inputs (maybe this will take too much ram?)
iter_template <- paste0("it_", 1:STSIM_ITER, "_")
ts_template <- unique(paste0("ts_", c(STSIM_TS_START, seq(STSIM_STEP_SAVE, STSIM_TS_END, STSIM_STEP_SAVE)), "_"))
ts_steps <- as.numeric(unique(c(STSIM_TS_START, seq(STSIM_STEP_SAVE, STSIM_TS_END, STSIM_STEP_SAVE))))
species_list <- tools::file_path_sans_ext(list.files("config/rcl_tables/species/"))

list_files <- list.files("outputs/current_density", full.names = T, pattern = "curmap")

# For zonal statistics
if (aggregation$ag) {
  mun_zonal <- raster("data/stsim/aggregated/secondary_stratum.tif")
} else {
  mun_zonal <- raster("data/stsim/secondary_stratun_mun_30by30.tif")
}
key <- read.csv("config/stsim/SecondaryStratum.csv")

# For removing outside NAs
template <- raster("outputs/reclassed_with_buffer/circuitscape_buffer.tif")

# Split data repeteadly => for loop
assembled_list <- list()
extracted_list <- list()

for (species in species_list) {
  print(species)
  ts_list <- list()
  for (timestep in ts_template) {
    print(timestep)
    iter_list <- list()
    for (iter in iter_template) {
      # Step 1 assemble NS and EW
      stack_files <- list_files[grepl(x = list_files, pattern = species) & 
                                  grepl(x = list_files, pattern = timestep) & grepl(x = list_files, 
                                                                                    pattern = iter)]
      #print(stack_files)
      iter_list[[iter]] <- sum(stack(lapply(stack_files, FUN = raster)))      
    }
    #print(iter_list)
    
    mean.of.rasters <- mean(stack(iter_list))
    ts_list[[timestep]] <- mean.of.rasters
    
    mean.of.rasters[template == 2] <- NA
    # mean.of.rasters <- rescale_raster_layer(mean.of.rasters)
    
    # Make zonal stats directly
    df <- as.data.frame(zonal(x = crop(mean.of.rasters, mun_zonal), mun_zonal), 
                        z = mun_zonal, fun = "mean", na.rm = T)
    df$zone <- as.factor(df$zone)
    df$species <- species
    df$timestep <-ts_steps[which(ts_template == timestep)]
    extracted_list[[paste(species, timestep, sep = "_")]] <- df
    
  }
  assembled_list[[species]] <- ts_list
}

# assemble last dataset
final_df <- extracted_list[[1]]
for (df in extracted_list[2:length(extracted_list)]) {
  final_df <- full_join(final_df, df, by = c("zone", "timestep", "species", "mean"))
}

final_df$timestep <- as.numeric(final_df$timestep)
saveRDS(final_df, "outputs/final_df_current_density.RDS")

saveRDS(assembled_list, "outputs/final_raster_means.rds")

# FULL SUM FOR FINAL OUTPUTS
full_stack=list()
for (x in 1:length(assembled_list[[1]])) { 
  the_stack <-  (stack(lapply(assembled_list, `[[`, x)))
  the_sum <- sum(the_stack)
  full_stack[[x]] <- the_sum
}  
names(full_stack) <- names(assembled_list[[1]])

for(raster in names(full_stack)){
  writeRaster(full_stack[[raster]],
              file.path("outputs", "current_density_sum", 
                        paste0("full_sum_", raster, ".tif")), 
              overwrite=T)
}

# Now with original data
list_files_origin <- list_files[grepl(x = list_files, pattern = "TRUE")]

assembled_list_T <- list()
extracted_list_T <- list()

for (species in species_list) {
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

unlisted <- unlist(assembled_list_T, recursive = F)
final_df_origin <- unlisted[[1]]
for (df in unlisted[2:length(unlisted)]) {
  final_df_origin <- full_join(final_df_origin, df, by = c("zone", "timestep", "species", "mean"))
}

final_df_origin$timestep <- as.numeric(final_df_origin$timestep)
saveRDS(final_df_origin, "outputs/final_df_origin_current_density.RDS")

# DATA VIZ
png("outputs/figures/final_graph.png")
ggplot(final_df) + aes(x = timestep, y = mean, color = zone) + geom_line(show.legend = FALSE) + 
  scale_color_viridis_d(option = "plasma") + theme_minimal() + facet_wrap(vars(species))
dev.off()
