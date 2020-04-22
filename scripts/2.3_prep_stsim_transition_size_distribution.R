#-------------------------------------------------------------------------------
## 2.3 Prepare transition size distributions for stsim
## 2020
## Inputs: land use
## Outputs: Transition size distribution
#-------------------------------------------------------------------------------

# reviewed 2020 

# Remove all in environment
rm(list = ls())

# Load packages 
library(raster)
library(sf)
library(tidyverse)
library(rlist)

# Reset Raster tmp files
removeTmpFiles(0)
showTmpFiles()

source("scripts/functions/get_change_raster.R")

#-------------------------------------------------------------------------------

lu <- stack(raster("data/land_use/LandUse_mont_aafc_30by30_1990.tif"),
            raster("data/land_use/LandUse_mont_aafc_30by30_2000.tif"),
            raster("data/land_use/LandUse_mont_aafc_30by30_2010.tif"))

lu_chg <- list(
  urb = list(t1 = get_change_raster(raster_1 = lu$LandUse_mont_aafc_30by30_1990, 
                                    raster_2 = lu$LandUse_mont_aafc_30by30_2000, 
                                    class = 2, 
                                    from = c(1,3), only_from=T), 
             t2 = get_change_raster(raster_1 = lu$LandUse_mont_aafc_30by30_2000, 
                                    raster_2 = lu$LandUse_mont_aafc_30by30_2010, 
                                    class = 2, 
                                    from = c(1,3), only_from=T)),
  agex = list(t1 = get_change_raster(raster_1 = lu$LandUse_mont_aafc_30by30_1990, 
                                     raster_2 = lu$LandUse_mont_aafc_30by30_2000, 
                                     class = 1, 
                                     from = 3, only_from = T), 
              t2 = get_change_raster(raster_1 = lu$LandUse_mont_aafc_30by30_2000, 
                                     raster_2 = lu$LandUse_mont_aafc_30by30_2010, 
                                     class = 1, 
                                     from = 3, only_from = T))
)


# in R, slow but works

# function to calculate areas of change
getPatchDist <- function(rast, gaps = F, # inputs for clump
                         directions = 8) {
  rast.clumped <- clump(rast, directions = directions, gaps = gaps)
  rast.clumped[rast.clumped == 0] <- NA
  pols <- st_as_sf(rasterToPolygons(rast.clumped, dissolve = T))
  areas <- as.numeric(st_area(pols))
  return(list(areas=areas, rasters=rast.clumped))
}

# Get areas for each time step possible
all_areas <- list()

for (dim1 in c("urb", "agex")){
  
  for (dim2 in c("t1", "t2")){
    areas <-  getPatchDist(lu_chg[[dim1]][[dim2]])$areas
    areas.df <- as.data.frame(table(sort(areas/10000)))
    areas.df$Var1 <- ceiling(as.numeric(as.character(areas.df$Var1)))
    names(areas.df) <- c("MaximumArea", "Amount")
    df <- data.frame(areas.df,
                     Timestep = dim2,
                     Transition = dim1, 
                     stringsAsFactors = FALSE)
    
    all_areas <- list.append(all_areas, df)
    #str(all_areas)
  }
  
}

all_areas_df <- bind_rows(all_areas)

# Reformat the df
df_urban_change <- all_areas_df %>% 
  group_by(Transition, Timestep, MaximumArea) %>% 
  summarise(Amount = sum(Amount)) %>% 
  mutate(RelativeAmount = Amount/sum(Amount))

write.csv(df_urban_change, "config/stsim/TransitionSizeDistribution.csv", 
          row.names = F)
