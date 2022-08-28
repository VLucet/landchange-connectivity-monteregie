#-------------------------------------------------------------------------------
## 1.5 Prep Landis
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
  library(rgrass7)
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
lu.stack.buf.ag <- stack(raster("data/land_use/aggregated/aggregated_lu_buffered_new_roads_1990.tif"), 
                         raster("data/land_use/aggregated/aggregated_lu_buffered_new_roads_2000.tif"),
                         raster("data/land_use/aggregated/aggregated_lu_buffered_new_roads_2010.tif"))

# Inport and crop spatial files
age <- raster("data/landis/spatial/AGE-AVG-0.tif") # 0 to 200
baseline <- raster("data/landis/spatial/baseline_even_more_harvest_1_cover_types_0.tif")
landtypes <- raster("data/landis/spatial/lowLands_landTypes_v2_QL.tif")

# Reprooj landtypes
landtypes_reproj <- projectRaster(landtypes, to = lu.stack.buf.ag$aggregated_lu_buffered_new_roads_2010,
                                  method = "ngb")

# Reclass and interpolate (from Bronwyn's script)
baseline_rcl <- reclassify(baseline, rcl = data.frame(is = c(1,2,3,4),
                                                      becomes = c(1,2,2,3)))
age_rcl <- reclassify(age, rcl = data.frame(from = c(0, 30, 60),
                                            to = c(30, 60, 200),
                                            becomes = c(1, 2, 3)))

# Combine
baseline_combined <- baseline_rcl*10+age_rcl
baseline_combined_reproj <- projectRaster(baseline_combined, 
                                          to = lu.stack.buf.ag$aggregated_lu_buffered_new_roads_2010,
                                          method = "ngb")
writeRaster(baseline_combined_reproj, "data/landis/spatial/temp_baseline.tif", 
            overwrite=T)

# Calculate distance => in GRASS Start Grass session
unlink("libraries/grass/landusechangemodel/", recursive = T)
initGRASS(gisBase = "/usr/lib/grass78/", gisDbase = "libraries/grass/", 
          location = "landusechangemodel", mapset = "landis", 
          override = TRUE)
# Run section below to set projection again
execGRASS("g.mapset", mapset = "PERMANENT")
execGRASS("g.proj", flags = c("c"), 
          proj4 = projection(lu.stack.buf.ag$aggregated_lu_buffered_new_roads_2010))
execGRASS("g.mapset", mapset = "landis")
# Input
execGRASS("r.in.gdal", input = "data/landis/spatial/temp_baseline.tif", 
          output="baseline.cb", flags=c("overwrite"))
execGRASS("r.in.gdal", input = "data/land_use/aggregated/aggregated_lu_buffered_new_roads_1990.tif", 
          output="lu.1990", flags=c("overwrite"))
execGRASS("r.in.gdal", input = "data/land_use/aggregated/aggregated_lu_buffered_new_roads_2000.tif", 
          output="lu.2000", flags=c("overwrite"))
execGRASS("r.in.gdal", input = "data/land_use/aggregated/aggregated_lu_buffered_new_roads_2010.tif", 
          output="lu.2010", flags=c("overwrite"))

execGRASS("g.region", raster = "lu.2010")

execGRASS("r.mapcalc", expression='lu.2010.mask = lu.2010', flags=c("overwrite"))
execGRASS("r.mask", raster='lu.2010.mask')

execGRASS('r.null', map='baseline.cb', setnull='0')
execGRASS('r.null', map='lu.1990', setnull='3')
execGRASS('r.null', map='lu.2000', setnull='3')
execGRASS('r.null', map='lu.2010', setnull='3')

execGRASS('r.neighbors', input='baseline.cb', 
          output='baseline.cb.smooth5', method='mode', size=5, flags=c('overwrite'))
execGRASS('r.neighbors', input='baseline.cb', 
          output='baseline.cb.smooth13', method='mode', size=13, flags=c('overwrite'))
execGRASS('r.neighbors', input='baseline.cb', 
          output='baseline.cb.smooth20', method='mode', size=21, flags=c('overwrite'))
execGRASS('r.neighbors', input='baseline.cb', 
          output='baseline.cb.smooth25', method='mode', size=25, flags=c('overwrite'))
execGRASS('r.neighbors', input='baseline.cb', 
          output='baseline.cb.smooth35', method='mode', size=35, flags=c('overwrite'))

execGRASS('r.patch', input=c('lu.1990', 'baseline.cb.smooth5', 
                             'baseline.cb.smooth13', 'baseline.cb.smooth20', 
                             'baseline.cb.smooth25', 'baseline.cb.smooth35'), 
          output='lu.1990.patched', flags=c('overwrite'))
execGRASS('r.patch', input=c('lu.2000', 'baseline.cb.smooth5', 
                             'baseline.cb.smooth13', 'baseline.cb.smooth20', 
                             'baseline.cb.smooth25', "baseline.cb.smooth35"), 
          output='lu.2000.patched', flags=c('overwrite'))
execGRASS('r.patch', input=c('lu.2010', 'baseline.cb.smooth5', 
                             'baseline.cb.smooth13', 'baseline.cb.smooth20', 
                             'baseline.cb.smooth25', "baseline.cb.smooth35"), 
          output='lu.2010.patched', flags=c('overwrite'))

execGRASS('r.out.gdal', input='lu.1990.patched', 
          output='data/land_use/aggregated/aggregated_lu_buffered_1990_patched.tif', flags=c('overwrite'))
execGRASS('r.out.gdal', input='lu.2000.patched', 
          output='data/land_use/aggregated/aggregated_lu_buffered_2000_patched.tif', flags=c('overwrite'))
execGRASS('r.out.gdal', input='lu.2010.patched', 
          output='data/land_use/aggregated/aggregated_lu_buffered_2010_patched.tif', flags=c('overwrite'))
execGRASS("r.mask", flags = "r")

baseline_combined <- 
  stack(list(lu.1990 = raster('data/land_use/aggregated/aggregated_lu_buffered_1990_patched.tif'), 
             lu.2000 = raster('data/land_use/aggregated/aggregated_lu_buffered_2000_patched.tif'), 
             lu.2010 = raster('data/land_use/aggregated/aggregated_lu_buffered_2010_patched.tif')))

patched_region <- is.na(baseline_combined) & !is.na(lu.stack.buf.ag$aggregated_lu_buffered_new_roads_2010) 
patched_region[patched_region==0]  <- NA
writeRaster(patched_region$lu.1990,'data/land_use/aggregated/aggregated_lu_buffered_1990_to_patch',  
            format="GTiff", overwrite=T) 

baseline_combined[is.na(baseline_combined) & 
                    !is.na(lu.stack.buf.ag$aggregated_lu_buffered_new_roads_2010)] <- 12 # missing value replaced

# reproj and crop
landis_stack <- stack(baseline_combined, landtypes_reproj)
landis_stack_cropped <- mask(crop(landis_stack, lu.stack.buf.ag$aggregated_lu_buffered_new_roads_2010),
                             lu.stack.buf.ag$aggregated_lu_buffered_new_roads_2010)

writeRaster(landis_stack_cropped$lu.1990, 
            'data/land_use/aggregated/aggregated_lu_buffered_1990_patched.tif', 
            format="GTiff", overwrite=T)
writeRaster(landis_stack_cropped$lu.2000, 
            'data/land_use/aggregated/aggregated_lu_buffered_2000_patched.tif', 
            format="GTiff", overwrite=T)
writeRaster(landis_stack_cropped$lu.2010, 
            'data/land_use/aggregated/aggregated_lu_buffered_2010_patched.tif', 
            format="GTiff", overwrite=T)

# writeRaster(landis_stack_cropped$lowLands_landTypes_v2_QL, 
#             "data/landis/spatial/buf_mont_land_types.tif", 
#             format="GTiff", overwrite=T)

## Replacing values
mont_ones <- fasterize(mun, lu.stack.buf.ag$aggregated_lu_buffered_new_roads_2010)
landis_stack_cropped_masked <- landis_stack_cropped
names(landis_stack_cropped_masked) <- names(landis_stack_cropped)

landis_stack_cropped_masked[is.na(mont_ones)] <- 0
landis_stack_cropped_masked[is.na(lu.stack.buf.ag$aggregated_lu_buffered_new_roads_2010)] <- NA
names(landis_stack_cropped_masked) <- names(landis_stack_cropped)

## landtype 99 will be outside mont 
landis_stack_cropped_masked$lowLands_landTypes_v2_QL[is.na(mont_ones) & 
                                        !is.na(lu.stack.buf.ag$aggregated_lu_buffered_new_roads_2010)] <- 99

# # Test
# test <- landis_stack_cropped_masked$lowLands_landTypes_v2_QL
# test_bin <- mask(crop((test != 0), mun), mun)
# lu.buf_forest <- mask(crop((lu.buf == 3), mun), mun)
# equ <- test_bin - lu.buf_forest

## Write out 
writeRaster(landis_stack_cropped_masked$lowLands_landTypes_v2_QL, 
            "data/landis/spatial/mont_land_types", 
            format="GTiff", overwrite=T)
