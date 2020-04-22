#-------------------------------------------------------------------------------
## 2.4 Spatial multipliers from workshop
## 2018-2019
## Inputs: Physisical data [stat model output?]
## Outputs: Transition Spatial multipliers
#-------------------------------------------------------------------------------

# reviewed 2020 

# Remove all in environment
rm(list = ls())

## Load required packages ##
library(raster)
library(sf)
library(tidyverse)
library(fasterize)
library(RStoolbox)

# Reset Raster tmp files
removeTmpFiles(0)
showTmpFiles()

#-------------------------------------------------------------------------------

## Import data
corridors <- st_read("data_raw/workshop/corridors/Key_links_smoothed_RegTables.shp")
areas <- st_read("data_raw/workshop/ensembles/Key_ensembles_smoothed_RegTables.shp")

# Spatial Mul raster
lu <- raster("data/land_use/LandUse_mont_aafc_30by30_1990.tif")  

# Only forest
lu_forest <- (lu == 3)
lu_forest[lu_forest == 0] <- NA
lu_forest[lu_forest == 1] <- 0

# lu_forest_vec <- st_as_sf(lu_forest)

# Repoject
corridors_rpj <- st_transform(corridors, crs = crs(lu))
areas_rpj <- st_transform(areas, crs = crs(lu))

# Creates 1km buffer around corridors
corridors_rpj_buffer <- st_buffer(corridors_rpj, 1000)

# Rasterize
corridors_rpj_buffer_rast <- fasterize(corridors_rpj_buffer, raster = lu)
areas_rpj_buffer_rast <- fasterize(areas_rpj, raster = lu)
plot(corridors_rpj_buffer_rast)
plot(areas_rpj_buffer_rast)

# Mask
corridors_rast_masked <- mask(lu_forest, corridors_rpj_buffer_rast)
areas_rast_masked <- mask(lu_forest, areas_rpj_buffer_rast)
