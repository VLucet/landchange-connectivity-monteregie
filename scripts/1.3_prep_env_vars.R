#-------------------------------------------------------------------------------
## 1.3 Prepare environmental variables
## 2020
## Inputs: Raw data (dist to roads, CLI, elevation, forest patch size)
## Outputs: Processed data
#-------------------------------------------------------------------------------

# reviewed 2020 

# Remove all in environment
rm(list = ls())

# Load required packages
suppressPackageStartupMessages({
  library(raster)
  library(sp)
  library(rgrass7)
  library(rgdal)
})
# library(devtools)
# install_github('loicdtx/bfastSpatial')
# install.packages("igraph")
suppressPackageStartupMessages(library(bfastSpatial))

# For troublehsooting if raster package use too much memory
removeTmpFiles(0)
showTmpFiles()

#-------------------------------------------------------------------------------

## 1. DISTANCE TO ROADS

## Load inputs data
lu.1990.18.m.mont <- raster("data/land_use/AAFC_zone18_mont_mask_1990.tif")
lu.2000.18.m.mont <- raster("data/land_use/AAFC_zone18_mont_mask_2000.tif")
lu.2010.18.m.mont <- raster("data/land_use/AAFC_zone18_mont_mask_2010.tif")

## Transform crs.lu.final = crs(raster(paste0('~/Documents/Master/BackupTifs/',
## 'landuse_linear_fixed.tif'))) write.csv(projection(crs.lu.final), 'crs_lu.txt')

# 25 is the road land use category Extract roads and reclassify
lu.90.mont.roads <- reclassify((lu.1990.18.m.mont == 25), 
                               rcl = matrix(c(1, 0, 1, NA), c(2, 2)))
lu.00.mont.roads <- reclassify((lu.2000.18.m.mont == 25), 
                               rcl = matrix(c(1, 0, 1, NA), c(2, 2)))
lu.10.mont.roads <- reclassify((lu.2010.18.m.mont == 25), 
                               rcl = matrix(c(1, 0, 1, NA), c(2, 2)))

crs(lu.90.mont.roads) <- crs("+init=epsg:32618")
crs(lu.10.mont.roads) <- crs("+init=epsg:32618")
crs(lu.00.mont.roads) <- crs("+init=epsg:32618")

lu.90.mont.urb <- reclassify((lu.1990.18.m.mont == 21), 
                             rcl = matrix(c(1, 0, 1, NA), c(2, 2)))
lu.00.mont.urb <- reclassify((lu.2000.18.m.mont == 21), 
                             rcl = matrix(c(1, 0, 1, NA), c(2, 2)))
lu.10.mont.urb <- reclassify((lu.2010.18.m.mont == 21), 
                             rcl = matrix(c(1, 0, 1, NA), c(2, 2)))

crs(lu.90.mont.urb) <- crs("+init=epsg:32618")
crs(lu.00.mont.urb) <- crs("+init=epsg:32618")
crs(lu.10.mont.urb) <- crs("+init=epsg:32618")

## Write raster
writeRaster(lu.90.mont.roads, "data/land_use/roads/lu_90_mont_roads.tif", 
            overwrite = TRUE)
writeRaster(lu.00.mont.roads, "data/land_use/roads/lu_00_mont_roads.tif", 
            overwrite = TRUE)
writeRaster(lu.10.mont.roads, "data/land_use/roads/lu_10_mont_roads.tif", 
            overwrite = TRUE)

writeRaster(lu.90.mont.urb, "data/land_use/urban/lu_90_mont_urb.tif", 
            overwrite = TRUE)
writeRaster(lu.00.mont.urb, "data/land_use/urban/lu_00_mont_urb.tif", 
            overwrite = TRUE)
writeRaster(lu.10.mont.urb, "data/land_use/urban/lu_10_mont_urb.tif", 
            overwrite = TRUE)

# plot(lu.90.mont.roads)
# Line below is way too slow
# lu.90.mont.roads.dis <- distance(lu.90.mont.roads)

## !! -

# Calculate distance => in GRASS Start Grass session
initGRASS(gisBase = "/usr/lib/grass76/", 
          gisDbase = "libraries/grass/", location = "landusechangemodel", 
          mapset = "monteregie", 
          override = TRUE)

# Run section below to set projection again
execGRASS("g.mapset", mapset = "PERMANENT")
execGRASS("g.proj", flags = c("c"), 
          proj4 = projection(raster("data/land_use/roads/lu_00_mont_roads.tif")))
execGRASS("g.mapset", mapset = "monteregie")

# Import
execGRASS("r.in.gdal", input = "data/land_use/roads/lu_90_mont_roads.tif", 
          output = "lu.90.mont.roads", flags = c("overwrite", "o"))
execGRASS("r.in.gdal", input = "data/land_use/roads/lu_00_mont_roads.tif", 
          output = "lu.00.mont.roads", flags = c("overwrite", "o"))
execGRASS("r.in.gdal", input = "data/land_use/roads/lu_10_mont_roads.tif", 
          output = "lu.10.mont.roads", flags = c("overwrite", "o"))

execGRASS("r.in.gdal", input = "data/land_use/urban/lu_90_mont_urb.tif", 
          output = "lu.90.mont.urb", flags = c("overwrite", "o"))
execGRASS("r.in.gdal", input = "data/land_use/urban/lu_00_mont_urb.tif", 
          output = "lu.00.mont.urb", flags = c("overwrite", "o"))
execGRASS("r.in.gdal", input = "data/land_use/urban/lu_10_mont_urb.tif", 
          output = "lu.10.mont.urb", flags = c("overwrite", "o"))

# Import mask
execGRASS("r.in.gdal", input = "data/land_use/AAFC_zone18_mont_mask_1990.tif", 
          output = "road.mask", flags = c("overwrite", "o"))
execGRASS("g.region", raster = "lu.90.mont.roads")

# Start mask
execGRASS("r.mask", raster = "road.mask")

# calculate distance
execGRASS("r.grow.distance", input = "lu.90.mont.roads", 
          distance = "lu.90.mont.roads.dist", 
          flags = c("overwrite"))
execGRASS("r.grow.distance", input = "lu.00.mont.roads", 
          distance = "lu.00.mont.roads.dist", 
          flags = c("overwrite"))
execGRASS("r.grow.distance", input = "lu.10.mont.roads", 
          distance = "lu.10.mont.roads.dist", 
          flags = c("overwrite"))

execGRASS("r.grow.distance", input = "lu.90.mont.urb", 
          distance = "lu.90.mont.urb.dist", 
          flags = c("overwrite"))
execGRASS("r.grow.distance", input = "lu.00.mont.urb", 
          distance = "lu.00.mont.urb.dist", 
          flags = c("overwrite"))
execGRASS("r.grow.distance", input = "lu.10.mont.urb", 
          distance = "lu.10.mont.urb.dist", 
          flags = c("overwrite"))

# Export
execGRASS("r.out.gdal", input = "lu.90.mont.roads.dist", 
          output = "data/distances/roads/lu_90_mont_roadsDis.tif", 
          format = "GTiff", flags = c("overwrite"))
execGRASS("r.out.gdal", input = "lu.00.mont.roads.dist", 
          output = "data/distances/roads/lu_00_mont_roadsDis.tif", 
          format = "GTiff", flags = c("overwrite"))
execGRASS("r.out.gdal", input = "lu.10.mont.roads.dist", 
          output = "data/distances/roads/lu_10_mont_roadsDis.tif", 
          format = "GTiff", flags = c("overwrite"))

execGRASS("r.out.gdal", input = "lu.90.mont.urb.dist", 
          output = "data/distances/urban/lu_90_mont_urbDis.tif", 
          format = "GTiff", flags = c("overwrite"))
execGRASS("r.out.gdal", input = "lu.00.mont.urb.dist", 
          output = "data/distances/urban/lu_00_mont_urbDis.tif", 
          format = "GTiff", flags = c("overwrite"))
execGRASS("r.out.gdal", input = "lu.10.mont.urb.dist", 
          output = "data/distances/urban/lu_10_mont_urbDis.tif", 
          format = "GTiff", flags = c("overwrite"))


# Remove Mask
execGRASS("r.mask", "r")

## Quality check ==> WORKS !!

road_dist.90 <- raster("data/distances/roads/lu_90_mont_roadsDis.tif")
road_dist.00 <- raster("data/distances/roads/lu_00_mont_roadsDis.tif")
road_dist.10 <- raster("data/distances/roads/lu_10_mont_roadsDis.tif")

urb_dist.90 <- raster("data/distances/urban/lu_90_mont_urbDis.tif")
urb_dist.00 <- raster("data/distances/urban/lu_00_mont_urbDis.tif")
urb_dist.10 <- raster("data/distances/urban/lu_10_mont_urbDis.tif")

road.stack <- stack(road_dist.90, road_dist.00, road_dist.10)

# ------------------------------------------------------------------------------

## 2. CLI - SUITABILITY FOR AGRICULTURE

CLI.50 <- shapefile("data/raw/env_vars/CLI/CLI_50k_clipped")
CLI.250 <- shapefile("data/raw/env_vars/CLI/CLI_250k_clipped")

CLI.50.reproj <- spTransform(CLI.50, crs(lu.90.mont.roads))
CLI.250.reproj <- spTransform(CLI.250, crs(lu.90.mont.roads))

# Recast for input to GRASS
prob.char <- tail(CLI.50.reproj@data$CLASS_A, n = 40)[20]  # problem with this char

source("scripts/functions/reclassForGRASS.R")

CLI.50.reproj.r <- reclassForGRASS(CLI.50.reproj, prob.char)
CLI.250.reproj.r <- reclassForGRASS(CLI.250.reproj, prob.char)

shapefile(CLI.50.reproj.r, "data/env_vars/CLI/CLI_50k_reproj.shp", 
          overwrite = TRUE)
shapefile(CLI.250.reproj.r, "data/env_vars/CLI/CLI_250k_reproj.shp", 
          overwrite = TRUE)

## CALL TO GRASS Import
execGRASS("v.in.ogr", input = "data/env_vars/CLI/CLI_50k_reproj.shp", 
          output = "CLI_50", flags = c("overwrite"))
execGRASS("v.in.ogr", input = "data/env_vars/CLI/CLI_250k_reproj.shp", 
          output = "CLI_250", flags = c("overwrite"))

# Mask and rasterize
execGRASS("r.mask", raster = "road.mask")
execGRASS("v.to.rast", input = "CLI_50", 
          output = "CLI_50_rast", use = "attr", 
          attribute_column = "CLASS_A", 
          flags = c("overwrite"))
execGRASS("v.to.rast", input = "CLI_250", 
          output = "CLI_250_rast", use = "attr", 
          attribute_column = "CLASS_A", 
          flags = c("overwrite"))
# Output before mosaic
execGRASS("r.out.gdal", input = "CLI_50_rast", 
          output = "data/env_vars/CLI/CLI_50k.tif", 
          format = "GTiff", flags = c("overwrite"))
execGRASS("r.out.gdal", input = "CLI_250_rast", 
          output = "data/env_vars/CLI/CLI_250k.tif", 
          format = "GTiff", flags = c("overwrite"))

# Mosaic with rpatch
execGRASS("r.patch", input = "CLI_50_rast,CLI_250_rast", 
          output = "CLI_mosaic", 
          flags = c("overwrite"))

# Export mosaic
execGRASS("r.out.gdal", input = "CLI_mosaic", 
          output = "data/env_vars/CLI/CLI_mosaic.tif", 
          format = "GTiff", flags = c("overwrite"))

execGRASS('r.mask','r')

## Quality check
rast.50 = raster("data/env_vars/CLI/CLI_50k.tif")
rast.250 = raster("data/env_vars/CLI/CLI_250k.tif")
mosaic = raster("data/env_vars/CLI/CLI_mosaic.tif")

# Check and fix NAs
#plot(mosaic)  # NAs are black
mosaic[mosaic==0] <- NA
#plot(mosaic$layer)
## Write out to input file
writeRaster(mosaic, "data/env_vars/CLI/CLI_mosaic_no0.tif", overwrite=TRUE)

# -----------------------------------------------------------------------------

## 3. SLOPE (FROM EARTH ENGINE SRTM 30m)

## Input data

Elev.mont <- raster("data/raw/env_vars/elevation_GEE/Mont_slope.tif")

# Reproject and crop
Elev.mont.reproj <- projectRaster(Elev.mont, lu.1990.18.m.mont)
Elev.mont.reproj.cropped <- mask(Elev.mont.reproj, lu.1990.18.m.mont)

# Plot and export
plot(Elev.mont.reproj.cropped)
extent(Elev.mont.reproj.cropped)
writeRaster(Elev.mont.reproj.cropped, "data/env_vars/elevation/elev_mont.tif", 
            overwrite=TRUE)

# -----------------------------------------------------------------------------
## 4. size of forest patches

lu.1990.18.m.mont.forest <- reclassify((lu.1990.18.m.mont %in% c(41,45)), 
                                       rcl = matrix(c(1, 0, 1, NA), c(2, 2)))
lu.2000.18.m.mont.forest <- reclassify((lu.2000.18.m.mont %in% c(41,45)), 
                                       rcl = matrix(c(1, 0, 1, NA), c(2, 2)))
lu.2010.18.m.mont.forest <- reclassify((lu.2010.18.m.mont %in% c(41,45)), 
                                       rcl = matrix(c(1, 0, 1, NA), c(2, 2)))

forest.size.1990 <- clumpSize(lu.1990.18.m.mont.forest, directions=4)
forest.size.2000 <- clumpSize(lu.2000.18.m.mont.forest, directions=4)
forest.size.2010 <- clumpSize(lu.2010.18.m.mont.forest, directions=4)

writeRaster(forest.size.1990, "data/env_vars/patch_size/Forest_patch_size_1990.tif", 
            overwrite = TRUE)
writeRaster(forest.size.2000, "data/env_vars/patch_size/Forest_patch_size_2000.tif", 
            overwrite = TRUE)
writeRaster(forest.size.2010, "data/env_vars/patch_size/Forest_patch_size_2010.tif", 
            overwrite = TRUE)
