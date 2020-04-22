#-------------------------------------------------------------------------------
## 1.4 Reclassify land use
## 2020
## Inputs: Land use data
## Outputs: Reclassified land use data
#-------------------------------------------------------------------------------

# reviewed 2020: removed melcc data

## Clear object list 
rm(list=ls())

# Load packages 
library(tidyverse)
library(raster)
library(sf) 
library(rgrass7)

# Reset Raster tmp files
removeTmpFiles(0)
showTmpFiles()

#-------------------------------------------------------------------------------

## import AAFC landuse - monteregie only
lu.mont.aafc <- 
  stack(raster("data/land_use/AAFC_zone18_mont_mask_1990.tif"),
        raster("data/land_use/AAFC_zone18_mont_mask_2000.tif"),
        raster("data/land_use/AAFC_zone18_mont_mask_2010.tif"))
lu.mont.aafc.buffered <- 
  stack(raster("data/land_use/AAFC_zone18_mont_mask_buffered_1990.tif"),
        raster("data/land_use/AAFC_zone18_mont_mask_buffered_2000.tif"),
        raster("data/land_use/AAFC_zone18_mont_mask_buffered_2010.tif"))


## Import shapefile of monteregie (clean)
mont.clean.aafc <- st_read("data/mun/munic_SHP_clean.shp")

## Import reclass table
reclass_table <- read_csv("config/rcl_tables/land_use/recode_table.csv")

# make reclass matrices
rcl.matrix.aafc <- reclass_table %>% 
  dplyr::filter(source == "AAFC") %>% 
  dplyr::select(old_code, new_code) %>% 
  as.matrix()
colnames(rcl.matrix.aafc) <-  c("is", "becomes")

# Reclass

lu.mont.aafc.r <- reclassify(lu.mont.aafc, rcl = rcl.matrix.aafc)
lu.mont.aafc.buf.r <- reclassify(lu.mont.aafc.buffered, rcl = rcl.matrix.aafc)
names(lu.mont.aafc.r) <- paste0(names(lu.mont.aafc.r), "_rcl")
names(lu.mont.aafc.buf.r) <- paste0(names(lu.mont.aafc.buf.r), "_rcl")

writeRaster(lu.mont.aafc.r, "data/land_use/LandUse_mont_aafc_30by30.tif", 
            bylayer=T, suffix=c(1990,2000,2010), overwrite = T)

writeRaster(lu.mont.aafc.buf.r, "data/land_use/LandUse_mont_aafc_buffered_30by30.tif", 
            bylayer=T, suffix=c(1990,2000,2010), overwrite = T)