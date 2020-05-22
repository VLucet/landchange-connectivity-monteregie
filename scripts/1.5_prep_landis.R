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
age <- raster("data/landis/spatial/age_avg_0.tif") # 0 to 200
baseline <- raster("data/landis/spatial/baseline_even_more_harvest_1_cover_types_0.tif") # 0 to 4
# 1: 2: 3: 4:
landtypes <- raster("data/landis/spatial/lowLands_landTypes_v2_QL.tif")

# Reclass and interpolate (from Bronwyn's script)
baseline_rcl <- reclassify(baseline, rcl = data.frame(is = c(1,2,3,4),
                                                      becomes = c(1,2,2,3)))
age_rcl <- reclassify(age, rcl = data.frame(from = c(0, 30, 60),
                                            to = c(30, 60, 200),
                                            becomes = c(1, 2, 3)))

# Combine
baseline_combined <- baseline_rcl*10+age_rcl
#projection(baseline_combined) <-  crs("+proj=lcc +lat_1=60 +lat_2=46 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0 +no_defs +a=6378137 +rf=298.257222101+towgs84=0,0,0,0,0,0,0 +to_meter=1")
writeRaster(baseline_combined, "data/landis/spatial/temp_baseline.tif", 
            overwrite=T)

# Calculate distance => in GRASS Start Grass session
initGRASS(gisBase = "/usr/lib/grass76/", gisDbase = "libraries/grass/", 
          location = "landusechangemodel", mapset = "landis", 
          override = TRUE)
# Run section below to set projection again
execGRASS("g.mapset", mapset = "PERMANENT")
execGRASS("g.proj", flags = c("c"), 
          proj4 = projection(raster("data/landis/spatial/temp_baseline.tif")))
execGRASS("g.mapset", mapset = "landis")
execGRASS("r.in.gdal", input = "data/landis/spatial/temp_baseline.tif", 
          output="baseline.cb", flags=c("overwrite", "o"))
execGRASS("g.region", raster = "baseline.cb")
execGRASS('r.null', map='baseline.cb', setnull='0')
execGRASS('r.neighbors', input='baseline.cb', 
          output='baseline.cb.smooth5', method='mode', size=5, flags=c('overwrite'))
execGRASS('r.neighbors', input='baseline.cb', 
          output='baseline.cb.smooth13', method='mode', size=13, flags=c('overwrite'))
execGRASS('r.out.gdal', input='baseline.cb.smooth5', 
          output="data/landis/spatial/temp_baseline_s5.tif", format='GTiff',
          createopt='COMPRESS=LZW', flags=c('overwrite', "f"))
execGRASS('r.out.gdal', input='baseline.cb.smooth13', 
          output="data/landis/spatial/temp_baseline_s13.tif", format='GTiff',
          createopt='COMPRESS=LZW', flags=c('overwrite', "f"))

baseline_combined_s5 <- raster("data/landis/spatial/temp_baseline_s5.tif")
baseline_combined_s13 <- raster("data/landis/spatial/temp_baseline_s13.tif")

# reproj and crop
landis_stack <- stack(baseline_combined_s13, landtypes)
names(landis_stack) <- c("baseline","landtypes")
landis_stack_reproj <- projectRaster(landis_stack, lu.buf, method = "ngb")
landis_stack_cropped <- mask(crop(landis_stack_reproj, lu.buf),lu.buf)

## Replacing values
mont_ones <- fasterize(mun, lu.buf)
landis_stack_cropped_masked <- landis_stack_cropped

writeRaster(landis_stack_cropped_masked$baseline, 
            "data/landis/spatial/buf_mont_baseline_cb.tif", 
            format="GTiff", overwrite=T)
writeRaster(landis_stack_cropped_masked$landtypes, 
            "data/landis/spatial/buf_mont_land_types.tif", 
            format="GTiff", overwrite=T)

landis_stack_cropped_masked[is.na(mont_ones)] <- 0
landis_stack_cropped_masked[is.na(lu.buf)] <- NA
names(landis_stack_cropped_masked) <- names(landis_stack_cropped)

## landtype 99 will be outside mont 
landis_stack_cropped_masked$landtypes[is.na(mont_ones) & !is.na(lu.buf)] <- 99

# # Test
# test <- landis_stack_cropped_masked$lowLands_landTypes_v2_QL
# test_bin <- mask(crop((test != 0), mun), mun)
# lu.buf_forest <- mask(crop((lu.buf == 3), mun), mun)
# equ <- test_bin - lu.buf_forest

## Write out 
writeRaster(landis_stack_cropped_masked$baseline, 
            "data/landis/spatial/mont_baseline_cb.tif", 
            format="GTiff", overwrite=T)
writeRaster(landis_stack_cropped_masked$landtypes, 
            "data/landis/spatial/mont_land_types", 
            format="GTiff", overwrite=T)
