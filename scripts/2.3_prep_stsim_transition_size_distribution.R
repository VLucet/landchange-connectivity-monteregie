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
suppressPackageStartupMessages({
  library(raster)
  library(sf)
  library(tidyverse)
  library(rlist)
})

# Reset Raster tmp files
removeTmpFiles(0)
showTmpFiles()

source("scripts/functions/get_change_raster.R")

#-------------------------------------------------------------------------------

lu <- stack(raster("data/land_use/LandUse_mont_aafc_30by30_1990.tif"),
            raster("data/land_use/LandUse_mont_aafc_30by30_2000.tif"),
            raster("data/land_use/LandUse_mont_aafc_30by30_2010.tif"))

lu_chg <- list(
  urb =  get_change_raster(raster_1 = lu$LandUse_mont_aafc_30by30_1990, 
                           raster_2 = lu$LandUse_mont_aafc_30by30_2010, 
                           class = 2, 
                           from = c(1,3), only_from=T),
  agex =  get_change_raster(raster_1 = lu$LandUse_mont_aafc_30by30_1990, 
                            raster_2 = lu$LandUse_mont_aafc_30by30_2010, 
                            class = 1, 
                            from = 3, only_from = T))

# in R, slow but works

# function to calculate areas of change
getPatchDist <- function(rast, gaps = F, # inputs for clump
                         directions = 8) {
  rast.clumped <- clump(rast, directions = directions, gaps = gaps)
  rast.clumped[rast.clumped == 0] <- NA
  pols <- st_as_sf(rasterToPolygons(rast.clumped, dissolve = T))
  areas <- as.numeric(st_area(pols))
  return(list(areas=areas, rasters=rast.clumped, pols=pols))
}

# Get areas for each time step possible
all_areas <- list()

for (class in c("urb", "agex")){
  
  res <-  getPatchDist(lu_chg[[class]])
  areas <-  res$areas
  areas.df <- as.data.frame(table(sort(areas/10000)))
  areas.df$Var1 <- ceiling(as.numeric(as.character(areas.df$Var1)))
  names(areas.df) <- c("MaximumArea", "Amount")
  df <- data.frame(areas.df,
                   TransitionGroupID = class, 
                   stringsAsFactors = FALSE)
  
  all_areas <- list.append(all_areas, df)
  #str(all_areas)
}

all_areas_df <- bind_rows(all_areas)

# Reformat the df
df_change <- all_areas_df %>% 
  group_by(TransitionGroupID, MaximumArea) %>% 
  summarise(Amount = sum(Amount)) %>%
  mutate(RelativeAmount = Amount/sum(Amount)) %>% 
  select(-Amount) %>% ungroup() %>% 
  mutate(group = cut(MaximumArea, breaks=c(0,1,5,100,1000))) %>% 
  group_by(TransitionGroupID, group) %>% 
  summarise(RelativeAmount=sum(RelativeAmount), MaximumArea=max(MaximumArea)) %>% 
  select(-group) %>% ungroup()
  
df_change$TransitionGroupID[df_change$TransitionGroupID=="urb"] <- 
  "Urbanisation"
df_change$TransitionGroupID[df_change$TransitionGroupID=="agex"] <- 
  "Agricultural Expansion Gr"

write.csv(df_change, "config/stsim/TransitionSizeDistribution.csv", 
          row.names = F)

df_change <- read_csv("config/stsim/TransitionSizeDistribution.csv") %>%
  filter(TransitionGroupID != "Reforestation Gr")
  
# 2 par 2 4 // 3 par 3 8 // 3 par 5 13
df_change <- df_change %>% bind_rows(
  
  data.frame(TransitionGroupID = rep("Reforestation Gr", 2), 
             RelativeAmount = c(0, 1), 
             MaximumArea = c(50, 100))
  
)

write.csv(df_change, "config/stsim/TransitionSizeDistribution.csv", 
          row.names = F)

prior <- data.frame(TransitionGroupID = c("Reforestation Gr", "Urbanisation", "Agricultural Expansion Gr"), 
                    MaximizeFidelityToTotalArea = c("Yes", "Yes", "Yes"))
write.csv(prior, "config/stsim/TransitionSizePrioritization.csv", 
          row.names = F)
