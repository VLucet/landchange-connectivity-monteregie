#-------------------------------------------------------------------------------
## 2.4 Spatial multipliers from workshop
## 2018-2019
## Inputs: Physisical data [stat model output?]
## Outputs: Transition Spatial multipliers
#-------------------------------------------------------------------------------
# Remove all in environment
rm(list = ls())

# reviewed 2020 
#-------------------------------------------------------------------------------
R_AGGR <- list(ag = as.logical(Sys.getenv("R_AGGR", unset = TRUE)), 
               factor = as.numeric(Sys.getenv("R_AGGR_FACT", unset = 3)))
# R_AGGR <- list(ag = as.logical(TRUE), 
#                factor = as.numeric(3))

CORR = 0
AREAS = 0
#-------------------------------------------------------------------------------

print(R_AGGR)

## Load required packages ##
suppressPackageStartupMessages({
  library(raster)
  library(sf)
  library(tidyverse)
  library(fasterize)
  library(RStoolbox)
})

# Reset Raster tmp files
removeTmpFiles(0)
showTmpFiles()

#-------------------------------------------------------------------------------

source("scripts/functions/aggregation_helpr.R")

## Import data
corridors <- st_read("data/raw/workshop/corridors/Key_links_smoothed_RegTables.shp", 
                     quiet = TRUE)
areas <- st_read("data/raw/workshop/ensembles/Key_ensembles_smoothed_RegTables.shp", 
                 quiet = TRUE)

# Spatial Mul raster
lu <- raster("data/land_use/LandUse_mont_aafc_buffered_30by30_1990.tif")  

# Only forest
lu_forest <- (lu == 3)
lu_forest[lu_forest == 0] <- NA
lu_forest[lu_forest == 1] <- CORR

#lu_forest_allone <- (lu == 3)
# lu_forest_vec <- st_as_sf(lu_forest)

# Repoject
corridors_rpj <- st_transform(corridors, crs = crs(lu))
areas_rpj <- st_transform(areas, crs = crs(lu))

# Creates 1km buffer around corridors
corridors_rpj_buffer <- st_buffer(corridors_rpj, 1000)

# Rasterize
corridors_rpj_buffer_rast <- fasterize(corridors_rpj_buffer, raster = lu)
areas_rpj_buffer_rast <- fasterize(areas_rpj, raster = lu)
#plot(corridors_rpj_buffer_rast)
#plot(areas_rpj_buffer_rast)

# Mask
corridors_rast_masked <- mask(lu_forest, corridors_rpj_buffer_rast)
areas_rast_masked <- mask(lu_forest, areas_rpj_buffer_rast)

# corridors_rast_masked_allone <- mask(lu_forest_allone, corridors_rpj_buffer_rast)
# areas_rast_masked_allone <- mask(lu_forest_allone, areas_rpj_buffer_rast)

# make areas less susceptible to deforestion
areas_rast_masked[!is.na(areas_rast_masked)] <- AREAS

# Make mosaic
lu_1 <- lu
lu_1[!is.na(lu_1)] <- 1
corrs_and_areas <- merge(areas_rast_masked, 
                         corridors_rast_masked, 
                         lu_1,
                         tolerance = 0)

lu_2 <- lu
lu_2[!is.na(lu_2)] <- 0
corrs_and_areas_allone <- merge(corridors_rpj_buffer_rast, 
                                areas_rpj_buffer_rast, 
                                lu_2,
                                tolerance = 0)

# Aggregate or not 
if(R_AGGR$ag){
  corrs_and_areas <- aggregate(corrs_and_areas, fun=modal_custom_first, 
                               fact=R_AGGR$factor)
  corrs_and_areas_allone <- aggregate(corrs_and_areas_allone, fun=modal_custom_first, 
                                      fact=R_AGGR$factor)
}

# Write out 
writeRaster(corrs_and_areas, 
            "data/stsim/spatial_multipliers/corrs_and_areas.tif", 
            overwrite =TRUE)
writeRaster(corrs_and_areas_allone, 
            "data/stsim/spatial_multipliers/corrs_and_areas_allone.tif", 
            overwrite =TRUE)

