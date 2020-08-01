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
suppressPackageStartupMessages({
  library(tidyverse)
  library(raster)
  library(sf)
  library(rgrass7)
  library(fasterize)
})
# Reset Raster tmp files
removeTmpFiles(0)
showTmpFiles()

R_AGGR <- list(ag = as.logical(Sys.getenv("R_AGGR", unset = TRUE)), 
               factor = as.numeric(Sys.getenv("R_AGGR_FACT", unset = 3)))

source("scripts/functions/aggregation_helpr.R")

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
mont.clean.aafc <- st_read("data/mun/munic_SHP_clean.shp", quiet = TRUE)

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

# Aggregation 
# lu.mont.aafc.r.ag <- 
#   aggregate(lu.mont.aafc.r, fun=modal_custom_first, R_AGGR$factor)
# lu.mont.aafc.buf.r.ag <- 
#   aggregate(lu.mont.aafc.buf.r, fun=modal_custom_first, R_AGGR$factor)

lu.mont.aafc.r.ag.noroads <- 
  aggregate(lu.mont.aafc.r, fun=modal_custom_first_no_roads, R_AGGR$factor)
lu.mont.aafc.buf.r.ag.noroads <- 
  aggregate(lu.mont.aafc.buf.r, fun=modal_custom_first_no_roads, R_AGGR$factor)

# Injection of roads

# Import roads and mun 
roads <- st_read("data/raw/roads/AQ_ROUTES.shp")
mun <- st_read("data/mun/munic_SHP_clean.shp")

# rast_1990_no_roads <- lu.mont.aafc.buf.r.ag.noroads$AAFC_zone18_mont_mask_buffered_1990_rcl
# rast_1990_no_roads[rast_1990_no_roads == 4] <- NA

roads_reproj <- st_transform(roads, raster::crs(mun))
roads_sub <- st_crop(roads_reproj, rast)

roads_sub_major <- subset(roads_sub, ClsRte %in% c("Autoroute", "Nationale", "Régionale", 
                                                   "Collectrice municipale", "Collectrice de transit"))
roads_sub_major$new_id <- 4
#plot(roads_sub_major$geometry)

if (!file.exists("data/roads/roads_sub_major_rast.tif")){
  roads_sub_major_rast <- rasterize(roads_sub_major,
                                    y = rast_1990,
                                    field = "new_id",
                                    fun = "first",
                                    update = FALSE)
  writeRaster(roads_sub_major_rast, "data/roads/roads_sub_major_rast.tif", overwrite=T)
}

# test <- merge(roads_sub_major_rast, rast_1990, overlap=T)
# test_2 <- merge(roads_sub_major_rast, rast_1990_no_roads, overlap=T)
# writeRaster(test, "test.tif", overwrite=T)
# writeRaster(test_2, "test_2.tif", overwrite =T)

roads <- raster("data/roads/roads_sub_major_rast.tif")
roads_cropped <- crop(roads, lu.mont.aafc.r.ag.noroads$AAFC_zone18_mont_mask_1990_rcl)
extent(roads_cropped) <- extent(lu.mont.aafc.r.ag.noroads$AAFC_zone18_mont_mask_1990_rcl)

# idx_urban <-
#   unique(unlist(lapply(as.list(lu.mont.aafc.r.ag), 
#                        function(x){which(values(x)==2)})))
# roads_cropped[idx_urban] <- NA

lu.mont.aafc.r.ag.noroads.m <- vector("list", 3)
for (idx in 1:3){
  lu.mont.aafc.r.ag.noroads.m[[idx]] <- 
    mask(raster::merge(roads_cropped, lu.mont.aafc.r.ag.noroads[[idx]]),
         lu.mont.aafc.r.ag.noroads[[idx]])
}
lu.mont.aafc.r.ag.noroads.m <- stack(lu.mont.aafc.r.ag.noroads.m)
names(lu.mont.aafc.r.ag.noroads.m) <- names(lu.mont.aafc.r.ag.noroads)

lu.mont.aafc.buf.r.ag.noroads.m <- vector("list", 3)
for (idx in 1:3){
  lu.mont.aafc.buf.r.ag.noroads.m[[idx]] <- 
    mask(raster::merge(roads, lu.mont.aafc.buf.r.ag.noroads[[idx]]),
         lu.mont.aafc.buf.r.ag.noroads[[idx]])
}
lu.mont.aafc.buf.r.ag.noroads.m <- stack(lu.mont.aafc.buf.r.ag.noroads.m)
names(lu.mont.aafc.buf.r.ag.noroads.m) <- names(lu.mont.aafc.buf.r.ag.noroads)

# Write it out

writeRaster(lu.mont.aafc.r.ag.noroads, "data/land_use/aggregated/aggregated_lu.tif", 
            bylayer=T, suffix=c(1990,2000,2010), overwrite = T)

writeRaster(lu.mont.aafc.buf.r.ag.noroads, "data/land_use/aggregated/aggregated_lu_buffered.tif", 
            bylayer=T, suffix=c(1990,2000,2010), overwrite = T)

writeRaster(lu.mont.aafc.r.ag.noroads.m, "data/land_use/aggregated/aggregated_lu_new_roads.tif", 
            bylayer=T, suffix=c(1990,2000,2010), overwrite = T)

writeRaster(lu.mont.aafc.buf.r.ag.noroads.m, "data/land_use/aggregated/aggregated_lu_buffered_new_roads.tif", 
            bylayer=T, suffix=c(1990,2000,2010), overwrite = T)
