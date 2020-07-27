#-------------------------------------------------------------------------------
## 3.1 Prepare data for model 
## 2020
## Inputs: Processed spatial data
## Outputs: Dataset for model fitting
#-------------------------------------------------------------------------------
#
# reviewed 2020 
#
## MODEL
#               1.
#   \/----------------------------------|
#  URB<--------------AGR<--------------FOR
#            2.               3. 
#
# 1. Forest to urban, deforestation
# 2. Agri to urban, agricultural loss
# 3. Forest to agri, agricultural expansion

#-------------------------------------------------------------------------------
rm(list=ls())
set.seed(77)
#-------------------------------------------------------------------------------
# Set parameters 
R_CROP <- as.logical(Sys.getenv("R_CROP", unset = FALSE))
R_AGGR <- list(ag = as.logical(Sys.getenv("R_AGGR", unset = TRUE)), 
               factor = as.numeric(Sys.getenv("R_AGGR_FACT", unset = 3)))
R_PART <- as.double(Sys.getenv("R_PART", unset = 0.7))
#-------------------------------------------------------------------------------

## Load required packages ##
suppressPackageStartupMessages({
  library(tidymodels)
  library(raster)
  library(sf)
  library(fasterize)
  library(dplyr)
  library(tidyr)
  library(normalr)
  library(parallel)
  library(rlist)
})

# Reset Raster tmp files
removeTmpFiles(0)
showTmpFiles()

## Source functions
source("scripts/functions/aggregation_helpr.R")
source("scripts/functions/get_change_raster.R")
source("scripts/functions/prepare_transition_data.R")
source("scripts/functions/get_neighbors_custom.R")
# source("scripts/functions/down_sample.R")

#-------------------------------------------------------------------------------
## Load inputs data
# Land use
print("Preparing data...") ; Sys.time()

lu.stack <- stack(raster("data/land_use/LandUse_mont_aafc_30by30_1990.tif"), 
                  raster("data/land_use/LandUse_mont_aafc_30by30_2000.tif"),
                  raster("data/land_use/LandUse_mont_aafc_30by30_2010.tif"))
lu.stack.buf <- stack(raster("data/land_use/LandUse_mont_aafc_buffered_30by30_1990.tif"), 
                      raster("data/land_use/LandUse_mont_aafc_buffered_30by30_2000.tif"),
                      raster("data/land_use/LandUse_mont_aafc_buffered_30by30_2010.tif"))
lu.stack.ag <- stack(raster("data/land_use/aggregated/aggregated_lu_new_roads_1990.tif"), 
                     raster("data/land_use/aggregated/aggregated_lu_new_roads_2000.tif"),
                     raster("data/land_use/aggregated/aggregated_lu_new_roads_2010.tif"))
lu.stack.buf.ag <- stack(raster("data/land_use/aggregated/aggregated_lu_buffered_new_roads_1990.tif"), 
                         raster("data/land_use/aggregated/aggregated_lu_buffered_new_roads_2000.tif"),
                         raster("data/land_use/aggregated/aggregated_lu_buffered_new_roads_2010.tif"))

names(lu.stack) <- c("lu_1990", "lu_2000", "lu_2010")
names(lu.stack.buf) <- c("lu_1990", "lu_2000", "lu_2010")
names(lu.stack.ag) <- c("lu_1990", "lu_2000", "lu_2010")
names(lu.stack.buf.ag) <- c("lu_1990", "lu_2000", "lu_2010")

mun <- st_read("data/mun/munic_SHP_clean.shp")

#-------------------------------------------------------------------------------
# Variables 
var.stack <- 
  stack(raster("data/distances/roads/lu_90_mont_roadsDis.tif"), 
        raster("data/distances/roads/lu_00_mont_roadsDis.tif"), 
        raster("data/distances/roads/lu_10_mont_roadsDis.tif"), 
        
        raster("data/distances/urban/lu_90_mont_urbDis.tif"), 
        raster("data/distances/urban/lu_00_mont_urbDis.tif"), 
        raster("data/distances/urban/lu_10_mont_urbDis.tif"), 
        
        raster("data/census/Income_1991.tif"), 
        raster("data/census/Income_2001.tif"),
        raster("data/census/Income_2011.tif"), 
        
        raster("data/census/Population_1991.tif"),
        raster("data/census/Population_2001.tif"),
        raster("data/census/Population_2011.tif"), 
        
        raster("data/env_vars/patch_size/Forest_patch_size_1990.tif"),
        raster("data/env_vars/patch_size/Forest_patch_size_2000.tif"),
        raster("data/env_vars/patch_size/Forest_patch_size_2010.tif"),
        
        raster("data/env_vars/elevation/elev_mont.tif"))

var.stack <- crop(var.stack, lu.stack)
names(var.stack) <- c("dis_90", "dis_00", "dis_10", 
                      "urb_90", "urb_00", "urb_10",
                      "in_90", "in_01", "in_11", 
                      "pop_90","pop_01", "pop_11", 
                      "for_90","for_00","for_10", 
                      "elev")  
# DEAL WITH NAs
var.stack$for_90[(is.na(var.stack$for_90))&(!is.na(var.stack$dis_90))] <- 0 
var.stack$for_00[(is.na(var.stack$for_00))&(!is.na(var.stack$dis_90))] <- 0 
var.stack$for_10[(is.na(var.stack$for_10))&(!is.na(var.stack$dis_90))] <- 0 

# MEAN IMPUTATION - NOT GOOD 
# Income
var.stack$in_90[((is.na(var.stack$in_90) & !is.na(var.stack$dis_10))==1) | 
                  var.stack$in_90==0] <- 
  mean(values(var.stack$in_90), na.rm=T)
var.stack$in_90[is.na(var.stack$dis_90)] <- NA

var.stack$in_01[((is.na(var.stack$in_01) & !is.na(var.stack$dis_10))==1) | 
                  var.stack$in_01==0] <- 
  mean(values(var.stack$in_01), na.rm=T)
var.stack$in_01[is.na(var.stack$dis_90)] <- NA

var.stack$in_11[((is.na(var.stack$in_11) & !is.na(var.stack$dis_10))==1) | 
                  var.stack$in_11==0] <- 
  mean(values(var.stack$in_11), na.rm=T)
var.stack$in_11[is.na(var.stack$dis_90)] <- NA

var.stack$in_90[((is.na(var.stack$in_90) & !is.na(var.stack$dis_10))==1) | 
                  var.stack$in_90==0] <- 
  mean(values(var.stack$in_90), na.rm=T)
var.stack$in_90[is.na(var.stack$dis_90)] <- NA

# Population
var.stack$pop_90[((is.na(var.stack$pop_90) & !is.na(var.stack$dis_10))==1) | 
                   var.stack$pop_90==0] <- 
  mean(values(var.stack$pop_90), na.rm=T)
var.stack$pop_90[is.na(var.stack$dis_90)] <- NA

var.stack$pop_01[((is.na(var.stack$pop_01) & !is.na(var.stack$dis_10))==1) | 
                   var.stack$pop_01==0] <- 
  mean(values(var.stack$pop_01), na.rm=T)
var.stack$pop_01[is.na(var.stack$dis_90)] <- NA

var.stack$pop_11[((is.na(var.stack$pop_11) & !is.na(var.stack$dis_10))==1) | 
                   var.stack$pop_11==0] <- 
  mean(values(var.stack$pop_11), na.rm=T)
var.stack$pop_11[is.na(var.stack$dis_90)] <- NA

# plot(var.stack)
# sum(is.na(values(var.stack$dis_90)))
# plot(var.stack$dis_90)

# Neigbors in 1990
##################

# my_mat <- matrix(c(0,0,1,0,0,
#                    0,1,1,1,0,
#                    1,1,0,1,1,
#                    0,1,1,1,0,
#                    0,0,1,0,0),5,5)

my_mat <- matrix(c(1,1,1,
                   1,0,1,
                   1,1,1),3,3)

if (R_AGGR$ag){
  
  print("Calculating neighbors...") ; Sys.time()
  
  # lu.stack.ag <- aggregate(lu.stack, fun=modal_custom_first, fact=R_AGGR$factor)
  class_names <- c(paste0(c("ag_", "urb_", "for_", "roa_", "wat_", "wet_"), "neigh"))
  
  neighbor_props <- stack(mapply(FUN = get_neighbors_custom,  value = c(1:6),
                                 MoreArgs=list(rast=lu.stack.ag$lu_1990, 
                                               mat_weight = my_mat)))
  names(neighbor_props) <- class_names
  
  neighbor_props_future <- stack(mapply(FUN = get_neighbors_custom,  value = c(1:6),
                                        MoreArgs=list(rast=lu.stack.ag$lu_2000, 
                                                      mat_weight = my_mat)))
  names(neighbor_props_future) <- class_names
  
} else {
  
  neighbor_props <- stack(mapply(FUN = get_neighbors_custom,  value = c(1:6),
                                 MoreArgs=list(rast=lu.stack$lu_1990, 
                                               mat_weight = my_mat)))
  names(neighbor_props) <- class_names
  
  neighbor_props_future <- stack(mapply(FUN = get_neighbors_custom,  value = c(1:6),
                                        MoreArgs=list(rast=lu.stack$lu_2000, 
                                                      mat_weight = my_mat)))
  names(neighbor_props_future) <- class_names
  
}

#-------------------------------------------------------------------------------
# Crop or not 
if (R_CROP){
  print("R_CROP map..") ; Sys.time()
  extent_to_crop <- extent(c(622000, 640000, 5030573 , 5043662))
  lu.stack <- crop(lu.stack, extent_to_crop)
  var.stack <- crop(var.stack, extent_to_crop)
} 

#-------------------------------------------------------------------------------
#  PREPARING ST SIM stratums

secondary_stratum_ag <- raster("data/stsim/aggregated/secondary_stratun_mun.tif")
secondary_stratum <- raster("data/stsim/secondary_stratun_mun_30by30.tif")

if (R_AGGR$ag){
  
  print("Aggregating rasters...") ; Sys.time()
  
  # vars
  var.stack.final <- aggregate(var.stack, fun=mean, fact=R_AGGR$factor)
  
  #secondary_stratum_layerized <- layerize(secondary_stratum_ag)
  secondary_stratum_final <- secondary_stratum_ag
  
} else {
  #secondary_stratum_layerized <- layerize(secondary_stratum)
  secondary_stratum_final <- secondary_stratum
}

# Secondary stratum
SS_final_cropped <- crop(secondary_stratum_final,  var.stack.final$dis_90)
extent(SS_final_cropped) <- extent(var.stack.final$dis_90)

#-------------------------------------------------------------------------------
# Prepare transition data

raster_base <- list()

# Forest and ag to Urb
raster_base[["urb"]] <- prepare_transition_data(lu.stack = lu.stack, 
                                                class_tr = 2, 
                                                from = c(1,3), 
                                                only_from = T, 
                                                aggregation = R_AGGR$ag,
                                                agfactor = R_AGGR$factor)

# Forest to ag
raster_base[["agex"]] <- prepare_transition_data(lu.stack = lu.stack, 
                                                 class_tr = 1, 
                                                 from = 3, 
                                                 only_from = T, 
                                                 aggregation = R_AGGR$ag,
                                                 agfactor = R_AGGR$factor)

writeRaster(raster_base[[1]][[1]],
            "data/temp/template.tif", overwrite=TRUE)

#-------------------------------------------------------------------------------
## DEAL WITH BUFFER OPERATIONS

if (R_AGGR$ag){
  buffer_all_empty_ag <- lu.stack.buf.ag$lu_1990
  values(buffer_all_empty_ag) <- NA
  
  cropped <- crop(buffer_all_empty_ag, raster_base[[1]][[1]]) # inelegant but works
  values(cropped) <-1
  buffer_allextent_with_ones_final <- mosaic(buffer_all_empty_ag, cropped, fun = max, na.rm=T,
                                             tolerance=0)
  ## Get the right IDs 
  frame_IDs <- as.vector(buffer_allextent_with_ones_final)==1
} else {
  buffer_all_empty <- lu.stack.buf$lu_1990
  values(buffer_all_empty) <- NA
  buffer_allextent_with_ones_final <- mosaic(buffer_all_empty, allextent_all_ones, fun = max, na.rm=T, 
                                             tolerance=0)
  frame_IDs <- NA
}

writeRaster(buffer_allextent_with_ones_final, "
            data/temp/template_with_buffer.tif", overwrite=TRUE)

#-------------------------------------------------------------------------------
## BUILD DATAFRAME

# Get coordinates
coords <- coordinates(raster_base[[1]][[1]])

# Build dataframe
df.trans <- cbind(as.data.frame(stack(raster_base$urb$chg.raster.final.class.2,
                                      raster_base$agex$chg.raster.final.class.1,
                                      var.stack.final, 
                                      neighbor_props, 
                                      SS_final_cropped)),coords) %>% 
  rename(mun = secondary_stratun_mun, 
         urb = new.1, agex = new.2) %>% 
  mutate(mun = as.factor(mun), 
         timestep = 1, 
         row_nb = as.numeric(row.names(.)))
# names(df.trans)[1:2] <- c("urb", "agex")

# Build dataframe for valdiation set
df.trans.future <- cbind(as.data.frame(stack(raster_base$urb$chg.raster.future.final.class.2,
                                             raster_base$agex$chg.raster.future.final.class.1,
                                             var.stack.final, 
                                             neighbor_props_future,
                                             SS_final_cropped)),coords) %>% 
  rename(mun = secondary_stratun_mun, 
         urb = new.1, agex = new.2) %>% 
  mutate(mun = as.factor(mun), 
         timestep = 2,
         row_nb = as.numeric(row.names(.)))
# names(df.trans.future)[1:2] <- c("urb", "agex") 

full.df <- bind_rows(df.trans, df.trans.future)

#-------------------------------------------------------------------------------
# Save outputs 
# saveRDS(data_temp, "data/temp/data_temp.RDS")
saveRDS(full.df, "data/temp/full_df.RDS")
print("Data Preparation Done") ; Sys.time()

# which(!((df.trans.mod %>% select(!contains("neigh")) %>% drop_na() %>% 
#            rownames() %>% as.numeric()) %in% (df.trans.mod %>% drop_na() %>% rownames() %>% 
#              as.numeric())))
