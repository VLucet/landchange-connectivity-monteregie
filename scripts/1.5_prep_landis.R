#-------------------------------------------------------------------------------
## 1.x Prep Landis
## 2020
## Inputs: Land use data
## Outputs: Reclassified land use data
#-------------------------------------------------------------------------------

# created 2020 

# Remove all in environment
rm(list = ls())

# Load required packages
suppressPackageStartupMessages({
  library(raster)
  library(sf)
  library(tidyverse)
  library(fasterize)
})

# Reset Raster tmp files
removeTmpFiles(0)
showTmpFiles()

#-------------------------------------------------------------------------------
## Not needed:
# init_com <- raster("data/landis/spatial/initial-communities_v2_QL.tif")
#-------------------------------------------------------------------------------

# Import landuse
mun <- st_read("data/mun/munic_SHP_clean.shp", quiet = TRUE)
lu.buf <- raster("data/land_use/aggregated/aggregated_lu_buffered_2010.tif")

# Inport and crop spatial files
age <- raster("data/landis/spatial/age_maximum_0.tif") # 0 to 200
baseline <- raster("data/landis/spatial/baseline_even_more_harvest_1_cover_types_0.tif") # 0 to 4
# 1: 2: 3: 4:
landtypes <- raster("data/landis/spatial/lowLands_landTypes_v2_QL.tif")

# reproj and crop
landis_stack <- stack(age, baseline, landtypes)
landis_stack_reproj <- projectRaster(landis_stack, lu.buf, method = "ngb")
landis_stack_cropped <- mask(crop(landis_stack_reproj, lu.buf),lu.buf)

## Replacing values
mont_ones <- fasterize(mun, lu.buf)
landis_stack_cropped_masked <- landis_stack_cropped

writeRaster(landis_stack_cropped_masked$age_maximum_0, 
            "data/landis/spatial/buf_mont_age.tif", 
            format="GTiff", overwrite=T)
writeRaster(landis_stack_cropped_masked$baseline_even_more_harvest_1_cover_types_0, 
            "data/landis/spatial/buf_mont_baseline.tif", 
            format="GTiff", overwrite=T)
writeRaster(landis_stack_cropped_masked$lowLands_landTypes_v2_QL, 
            "data/landis/spatial/buf_mont_land_types", 
            format="GTiff", overwrite=T)

landis_stack_cropped_masked[is.na(mont_ones)] <- 0
landis_stack_cropped_masked[is.na(lu.buf)] <- NA
names(landis_stack_cropped_masked) <- names(landis_stack_cropped)

## landtype 99 will be outside mont 
landis_stack_cropped_masked$lowLands_landTypes_v2_QL[is.na(mont_ones) & !is.na(lu.buf)] <- 99

# # Test
# test <- landis_stack_cropped_masked$lowLands_landTypes_v2_QL
# test_bin <- mask(crop((test != 0), mun), mun)
# lu.buf_forest <- mask(crop((lu.buf == 3), mun), mun)
# equ <- test_bin - lu.buf_forest

## Write out 
writeRaster(landis_stack_cropped_masked$age_maximum_0, 
            "data/landis/spatial/mont_age.tif", 
            format="GTiff", overwrite=T)
writeRaster(landis_stack_cropped_masked$baseline_even_more_harvest_1_cover_types_0, 
            "data/landis/spatial/mont_baseline.tif", 
            format="GTiff", overwrite=T)
writeRaster(landis_stack_cropped_masked$lowLands_landTypes_v2_QL, 
            "data/landis/spatial/mont_land_types", 
            format="GTiff", overwrite=T)
