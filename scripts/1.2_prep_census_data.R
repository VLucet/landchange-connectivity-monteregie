#-------------------------------------------------------------------------------
## 1.2 Prepare census data
## 2020
## Inputs: Raw census data from CHASS
## Outputs: Processed data
#-------------------------------------------------------------------------------

# reviewed august 2019 to be able to aggregate on the spot for running the model
# reviewed 2020 => removed MRT steps and the 2011 short survey

# Remove all in environment
rm(list = ls())

# Load required packages
suppressPackageStartupMessages({
  library(raster)
  library(sf)
  library(tidyverse)
  library(fasterize)
  library(rasterVis)
  library(ggplot2)
})

# Import geometries and land use
mun_sub_18_clean <- st_read("data/mun/munic_SHP_clean.shp", quiet = TRUE)
raster_mont <- raster("data/land_use/AAFC_zone18_mont_mask_1990.tif")

#------------------------------------------------------------------------------

# Working with CHASS data

# 1991

# import dataset and shapefile
chass.DA.1991.data <- read.csv("data/raw/census/CHASS_data/1991/1991_data.csv") %>%
  rename(EA = COL0)
# Dim:  10245   514
# Also  10245 unique EA
chass.DA.1991.sh.sf <- st_read(paste0(
  "data/raw/census/boundary_files/EAs_1991/EAs_1991_shapes/",
  "DLI_1991_Census_DBF_Eng_Nat_ea.shp"
), quiet = TRUE)
chass.DA.1991.sh.sf <- st_transform(chass.DA.1991.sh.sf, crs = st_crs(mun_sub_18_clean))
#length(unique(chass.DA.1991.sh.sf$EA)) # 7831

#sum(unique(chass.DA.1991.sh.sf$EA) %in% unique(chass.DA.1991.data$EA)) # 7347
#sum(unique(chass.DA.1991.data$EA) %in% unique(chass.DA.1991.sh.sf$EA)) # 7347

test <- chass.DA.1991.sh.sf %>%
  left_join(chass.DA.1991.data, by = "EA")
test <- fasterize(sf = test, raster = raster_mont, field = "COL7")
#plot(is.na(test))

## INTERSERCT DOESNT WORK
# chass.DA.1991.sh.sf.i = st_intersection(chass.DA.1991.sh.sf, mun_sub_18_clean)

chass.DA.1991.sh.sf.j <-
  st_join(st_centroid(chass.DA.1991.sh.sf), mun_sub_18_clean, left = F)
#length(unique(chass.DA.1991.sh.sf.j$EA))
#dim(chass.DA.1991.sh.sf.j)
# 1493   26
#plot(chass.DA.1991.sh.sf.j$geometry)
#plot(chass.DA.1991.sh.sf.j["MUS_NM_MUN"])

# re-inject right geometry
right_geom <- chass.DA.1991.sh.sf %>%
  filter(EA %in% unique(chass.DA.1991.sh.sf.j$EA))
#plot(right_geom$geometry, col = 2)
st_geometry(chass.DA.1991.sh.sf.j) <- st_geometry(right_geom)
#plot(chass.DA.1991.sh.sf.j["MUS_NM_MUN"])

# Test, they match
test_2 <- chass.DA.1991.sh.sf.j %>%
  filter(MUS_NM_MUN == "Yamaska")
test_3 <- chass.DA.1991.sh.sf %>%
  filter(EA %in% test_2$EA)
#plot(test_2$geometry, col = 2)
#plot(test_3$geometry)
# See https://www12.statcan.gc.ca/census-recensement/2011/geo/
# map-carte/pdf/CSD_SDR/2011-92145-2453072-00.pdf

# Cheack for NAs
matches <- match(chass.DA.1991.sh.sf.j$EA, chass.DA.1991.data$EA)
matches.NA <- which(is.na(matches))
unmachted <- chass.DA.1991.sh.sf.j[matches.NA, ] # (55/1413)*100 4% unmatched 

# Plot to see where the NAs are
# ggplot() +
#   geom_sf(data = chass.DA.1991.sh.sf.j) +
#   geom_sf(data = unmachted, aes(fill = AREA))

chass.DA.1991.data.matched <-
  chass.DA.1991.sh.sf.j[which(!is.na(match(
    chass.DA.1991.sh.sf.j$EA,
    chass.DA.1991.data$COL0
  ))), ]

# Rename the col 0 so it matches and perform the join
colnames(chass.DA.1991.data)[1] <- "EA"
CHASS.1991 <- left_join(chass.DA.1991.sh.sf.j, chass.DA.1991.data, by = "EA")

#plot(CHASS.1991["COL7"])

# ggplot() + geom_sf(data = CHASS.1991, aes(fill = CHASS.1991$COL2), color = 'black', lwd =
# 0.05) + theme(panel.background = element_rect(fill = NA), legend.position = 'top',
# legend.direction = 'horizontal') + scale_fill_viridis_c(trans = 'sqrt', alpha = .4)

# Rasterizzing
st_write(CHASS.1991, "data/census/vector/CHASS_1991.shp", delete_dsn = T, quiet = TRUE)

# VAR: Income, col 494
income.90 <- fasterize(st_read("data/census/vector/CHASS_1991.shp", quiet = TRUE),
                       raster = raster_mont,
                       field = "COL494"
)
#plot(income.90)
writeRaster(income.90, "data/census/Income_1991.tif", 
            overwrite = TRUE, format = "GTiff")

#-------------------------------------------------------------------------------

# 2001

# import dataset and shapefile
chass.DA.2001.data <- read.csv("data/raw/census/CHASS_data/2001/2001_data.csv")

chass.DA.2001.sh.sf <- 
  st_read("data/raw/census/boundary_files//DAs_2001/DAs_2001_shapes/DLI_2001_Census_CBF_Eng_Nat_da.shp",
          quiet = TRUE)
chass.DA.2001.sh.sf <- st_transform(chass.DA.2001.sh.sf, crs = st_crs(mun_sub_18_clean))

chass.DA.2001.sh.sf.j <-
  st_join(st_centroid(chass.DA.2001.sh.sf), mun_sub_18_clean, left = F)
#length(unique(chass.DA.2001.sh.sf.j$DAUID))
#dim(chass.DA.2001.sh.sf.j)
#plot(chass.DA.2001.sh.sf.j$geometry)
#plot(chass.DA.2001.sh.sf.j["MUS_NM_MUN"])

# re-inject right geometry
right_geom <- chass.DA.2001.sh.sf %>% filter(DAUID %in% unique(chass.DA.2001.sh.sf.j$DAUID))
#plot(right_geom$geometry, col = 2)
st_geometry(chass.DA.2001.sh.sf.j) <- st_geometry(right_geom)
#plot(chass.DA.2001.sh.sf.j["MUS_NM_MUN"])

# Join with census data
matches <- match(chass.DA.2001.sh.sf.j$DAUID, chass.DA.2001.data$COL0)

matches.NA <- which(is.na(matches))
unmachted <- chass.DA.2001.sh.sf.j[matches.NA, ] # (44/1882) * 100 2.3 %percent

#chass.DA.2001.data[matches, ] # NOT EMPTY good

chass.DA.2001.data.matched <-
  chass.DA.2001.sh.sf.j[which(!is.na(match(
    chass.DA.2001.sh.sf.j$DAUID,
    chass.DA.2001.data$COL0
  ))), ]

# Rename the col 0 so it matches and perform the join
colnames(chass.DA.2001.data)[1] <- "DAUID"
# need to cast twice
chass.DA.2001.sh.sf.j$DAUID <- as.double(as.character(chass.DA.2001.sh.sf.j$DAUID)) 
CHASS.2001 <- left_join(chass.DA.2001.sh.sf.j, chass.DA.2001.data, by = "DAUID")

#plot(CHASS.2001["COL899"])

# Rasterizing
st_write(CHASS.2001, delete_dsn = T, "data/census/vector/CHASS_2001.shp", quiet = TRUE)

# VAR: Income, col 899
income.01 <- fasterize(st_read("data/census/vector/CHASS_2001.shp", quiet = TRUE),
                       raster = raster_mont,
                       field = "COL899"
)
#plot(income.01)
writeRaster(income.01, "data/census/Income_2001.tif", overwrite = TRUE, format = "GTiff")

#-------------------------------------------------------------------------------

# 2011 SHORT 

# import dataset and shapefile
chass.DA.2011.sht.data <- read.csv("data/raw/census/CHASS_data/2011/2011_short_data.csv")

# chass.DA.2011.sh = shapefile(paste0(inputDir, 'Boundary_files/DAs_2011/DAs_2011_shapes/',
# 'DLI_2011_Census_CBF_Eng_Nat_da.shp'))
chass.DA.2011.sh.sf <- 
  st_read("data/raw/census/boundary_files/DAs_2011/DAs_2011_shapes/DLI_2011_Census_CBF_Eng_Nat_da.shp",
          quiet = TRUE)

# chass.DA.2011.sh = spTransform(chass.DA.2011.sh, crs(mun.data))
chass.DA.2011.sh.sf <- st_transform(chass.DA.2011.sh.sf, crs = st_crs(mun_sub_18_clean))

#chass.DA.2011.sh = crop(chass.DA.2011.sh.sf, mun.data)  # might crash
chass.DA.2011.sh.sf.i <- st_intersection(chass.DA.2011.sh.sf, mun_sub_18_clean)

# test length
#sum(table(unique(chass.DA.2011.sh.sf.i$DAUID)))

# Need to union
chass.DA.2011.sh.sf.iU <- chass.DA.2011.sh.sf.i %>% aggregate(list(chass.DA.2011.sh.sf.i$DAUID), 
                                                              function(x) x[1])

# Join with census data
matches <- match(chass.DA.2011.sh.sf.iU$DAUID, chass.DA.2011.sht.data$COL0)

matches.NA <- which(is.na(matches))
unmachted <- chass.DA.2011.sh.sf.iU[matches.NA, ] #0 %

#chass.DA.2011.sht.data[matches, ]  # NOT EMPTY good

# # Plot to see where the NAs are # NO NAs ggplot()+ geom_sf(data = chass.DA.1991.sh.sf.iU) +
# geom_sf(data = unmachted, aes(fill = AREA))

chass.DA.2011.data.matched <- 
  chass.DA.2011.sh.sf.iU[which(!is.na(match(chass.DA.2011.sh.sf.iU$DAUID, 
                                            chass.DA.2011.sht.data$COL0))), ]

# Rename the col 0 so it matches and perform the join
colnames(chass.DA.2011.sht.data)[1] <- "DAUID"
# need to cast twice
chass.DA.2011.sh.sf.iU$DAUID <- as.double(as.character(chass.DA.2011.sh.sf.iU$DAUID)) 
CHASS.2011.short <- left_join(chass.DA.2011.sh.sf.iU, chass.DA.2011.sht.data, by = "DAUID")

#plot(CHASS.2011.short["COL88"])

# Attempt at rasterizing
st_write(CHASS.2011.short, delete_dsn = T, "data/census/vector/CHASS_2011_short.shp")

short.11 <- fasterize(st_read("data/census/vector/CHASS_2011_short.shp", quiet = TRUE), 
                      raster = raster_mont, 
                      field = "COL88")
#plot(short.11)

# Summarize 

CHASS.2011.short %>% 
  group_by(MUS_NM_MUN) %>% 
  summarise_all(list(if(is.numeric(.)) mean(., na.rm = TRUE) 
                     else ~.[1])) ->
  CHASS.2011.short.summarized

# DIFFERENT JOIN FOR MRT

chass.DA.2011.sh.sf.j.mun <- 
  st_join(mun_sub_18_clean, st_centroid(chass.DA.2011.sh.sf), left=F)
# need to cast twice
chass.DA.2011.sh.sf.j.mun$DAUID = as.double(as.character(chass.DA.2011.sh.sf.j.mun$DAUID))  
CHASS.2011.mun = left_join(chass.DA.2011.sh.sf.j.mun, chass.DA.2011.sht.data, by = "DAUID")

CHASS.2011.mun %>% 
  group_by(MUS_NM_MUN) %>% 
  dplyr::select(MUS_NM_MUN, COL6) %>% 
  summarise_if(.predicate = is.numeric, ~sum(., na.rm = TRUE)) ->
  CHASS.2011.all.sum
CHASS.2011.mun %>% 
  group_by(MUS_NM_MUN) %>% 
  dplyr::select(MUS_NM_MUN, COL88) %>% 
  summarise_all(list(if(is.numeric(.)) mean(., na.rm = TRUE) else ~.[1])) ->
  CHASS.2011.income.mean
st_geometry(CHASS.2011.income.mean) <- NULL
class(CHASS.2011.income.mean) <- "data.frame" 

CHASS.2011.all.interest <- 
  left_join(CHASS.2011.all.sum, as_tibble(CHASS.2011.income.mean), 
            by="MUS_NM_MUN") %>% 
  dplyr::rename(Pop_11 = COL6, 
                Inc_11 = COL88)

#-------------------------------------------------------------------------------

# 2011 NHS

# import dataset and shapefile
chass.DA.2011.nhs.data <- read.csv("data/raw/census/CHASS_data/2011/2011_NHS_data.csv")

# chass.DA.2011.sh = shapefile(paste0(inputDir, 'Boundary_files/DAs_2011/DAs_2011_shapes/',
# 'DLI_2011_Census_CBF_Eng_Nat_da.shp'))
chass.DA.2011.sh.sf <- 
  st_read("data/raw/census/boundary_files/DAs_2011/DAs_2011_shapes/DLI_2011_Census_CBF_Eng_Nat_da.shp", 
          quiet = TRUE)

# chass.DA.2011.sh = spTransform(chass.DA.2011.sh, crs(mun.data))
chass.DA.2011.sh.sf <- st_transform(chass.DA.2011.sh.sf, crs = st_crs(mun_sub_18_clean))

# chass.DA.2011.sh = crop(chass.DA.2011.sh, mun.data) # might crash
chass.DA.2011.sh.sf.i <- st_intersection(chass.DA.2011.sh.sf, mun_sub_18_clean)

# Length differ (but should be 2479)
#sum(table(unique(chass.DA.2011.sh.sf.i$DAUID)))

# Need to union
chass.DA.2011.sh.sf.iU <- chass.DA.2011.sh.sf.i %>% aggregate(
  list(chass.DA.2011.sh.sf.i$DAUID),
  function(x) x[1]
)

# Join with census data
matches <- match(chass.DA.2011.sh.sf.iU$DAUID, chass.DA.2011.nhs.data$COL0)

matches.NA <- which(is.na(matches))
unmachted <- chass.DA.2011.sh.sf.iU[matches.NA, ] # (46/2453)*100 #1.8 %

#chass.DA.2011.nhs.data[matches, ] # NOT EMPTY good

chass.DA.2011.data.matched <- chass.DA.2011.sh.sf.iU[which(!is.na(match(
  chass.DA.2011.sh.sf.iU$DAUID,
  chass.DA.2011.nhs.data$COL0
))), ]

# Rename the col 0 so it matches and perform the join
colnames(chass.DA.2011.nhs.data)[1] <- "DAUID"
# need to cast twice
chass.DA.2011.sh.sf.iU$DAUID <- as.double(as.character(chass.DA.2011.sh.sf.iU$DAUID))
CHASS.2011.NHS <- left_join(chass.DA.2011.sh.sf.iU, chass.DA.2011.nhs.data, by = "DAUID")

#plot(CHASS.2011.NHS["COL366"])

# Rasterizing
st_write(CHASS.2011.NHS, delete_dsn = T, "data/census/vector/CHASS_2011_NHS.shp")

# VAR: Income, col 366
income.11 <- fasterize(st_read("data/census/vector/CHASS_2011_NHS.shp", quiet = TRUE),
                       raster = raster_mont,
                       field = "COL366"
)
#plot(income.11)
writeRaster(income.11, "data/census/Income_2011.tif", overwrite = TRUE, format = "GTiff")

#-------------------------------------------------------------------------------
## AUTOMATION WITH FUNCTION [to be ran after below but put here for convenience]
source("scripts/functions/get_census_var.R")

var_list <- 
  list(Income = c("COL494", "COL899", "COL88"),
       Population = c("COL7", "COL7", "COL6"))
year_list <- c(1991, 2001, 2011)

full_stack <- stack()

for(var_name in names(var_list)){
  print(var_name)
  rast <- get_census_var(year_list = year_list, 
                         var_vec = var_list[[var_name]],
                         var_name = var_name, 
                         datadir = "data/census/vector/", 
                         raster = raster_mont)
  
  full_stack <- stack(full_stack, rast)
  
}

writeRaster(
  full_stack,
  bylayer = T,
  paste0("data/census/", names(full_stack)),
  format = "GTiff", 
  overwrite=T
)
