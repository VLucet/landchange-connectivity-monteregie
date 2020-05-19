#-------------------------------------------------------------------------------
## 1.x Prep Neighbors
## 2020
## Inputs: Land use data
## Outputs: Reclassified land use data
#-------------------------------------------------------------------------------

rm(list = ls())

suppressPackageStartupMessages({
  library(tidyverse)
  library(raster)
  library(sf)
  library(rgrass7)
})

initGRASS(gisBase = "/usr/lib/grass76", #"/usr/lib/grass78/"
          gisDbase = "libraries/grass/", location = "landusechangemodel", 
          mapset = "monteregie", 
          override = TRUE)

lu <- raster("data/land_use/aggregated/aggregated_lu_1990.tif")
# lu_urb <- lu == 2
# lu_urb[lu_urb !=1] <- NA
# plot(lu_urb)
# lu_clumped <- clump(lu_urb, directions = 8)
# lu_freq <- freq(lu_clumped)
# 
# hist(lu_freq[,2])
# 
# tbl <- table(lu_freq[,2])
# hist(lu_freq[lu_freq<50], prob=TRUE)
# tbl.df <- as.data.frame(tbl)
# tbl.df
# hist(as.numeric(names(tbl)))

# Run section below to set projection again
execGRASS("g.mapset", mapset = "PERMANENT")
execGRASS("g.proj", flags = c("c"), 
          proj4 = projection(lu))
execGRASS("g.mapset", mapset = "monteregie")

execGRASS("r.in.gdal", input = "data/land_use/aggregated/aggregated_lu_1990.tif",
          output = "lu.1990", flags = c("overwrite", "o"))
execGRASS("r.in.gdal", input = "data/land_use/aggregated/aggregated_lu_2010.tif",
          output = "lu.2010", flags = c("overwrite", "o"))

execGRASS("g.region", raster = "lu.1990")
execGRASS("r.mapcalc", expression="lu.1990.urb=(lu.1990==2)", 
          flags = c("overwrite"))
execGRASS("r.mapcalc", expression="lu.2010.urb=(lu.2010==2)", 
          flags = c("overwrite"))
execGRASS("r.mapcalc", 
          expression="lu.1990.2010.urb.dif=((lu.2010.urb-lu.1990.urb)==1)", 
          flags = c("overwrite"))
execGRASS("r.null", map = "lu.1990.2010.urb.dif",
          setnull="0")

execGRASS("r.out.gdal", input = "lu.1990.urb",
          output = "data/neighbors/lu.1990.urb.tif",
          format = "GTiff", flags = c("overwrite"))
execGRASS("r.out.gdal", input = "lu.2010.urb",
          output = "data/neighbors/lu.2010.urb.tif",
          format = "GTiff", flags = c("overwrite"))
execGRASS("r.out.gdal", input = "lu.1990.2010.urb.dif",
          output = "data/neighbors/lu.1990.2010.urb.dif.tif",
          format = "GTiff", flags = c("overwrite"))

execGRASS("r.mapcalc", expression="lu.1990.ag=(lu.1990==1)", 
          flags = c("overwrite"))
execGRASS("r.mapcalc", expression="lu.2010.ag=(lu.2010==1)", 
          flags = c("overwrite"))
execGRASS("r.mapcalc", 
          expression="lu.1990.2010.ag.dif=((lu.2010.ag-lu.1990.ag)==1)", 
          flags = c("overwrite"))
execGRASS("r.null", map = "lu.1990.2010.ag.dif",
          setnull="0")

execGRASS("r.out.gdal", input = "lu.1990.ag",
          output = "data/neighbors/lu.1990.ag.tif",
          format = "GTiff", flags = c("overwrite"))
execGRASS("r.out.gdal", input = "lu.2010.ag",
          output = "data/neighbors/lu.2010.ag.tif",
          format = "GTiff", flags = c("overwrite"))
execGRASS("r.out.gdal", input = "lu.1990.2010.ag.dif",
          output = "data/neighbors/lu.1990.2010.ag.dif.tif",
          format = "GTiff", flags = c("overwrite"))

#dif <- raster("data/neighbors/lu.1990.2010.ag.dif.tif")

# URB

n_df_urb <- data.frame()
temp_list_urb <- list()

dist_seq <- seq(from = 1, to = 101, by=2)
for (neighborhood in dist_seq){
  execGRASS("r.neighbors", input = "lu.1990.urb", 
            output=paste0("lu.1990.urb.",neighborhood), 
            method = "average", size = neighborhood, 
            selection="lu.1990.2010.urb.dif",
            flags = c("overwrite", "c"))
  execGRASS("r.out.gdal", input = paste0("lu.1990.urb.",neighborhood), 
            output = paste0("data/neighbors/TEMP.tif"), 
            format = "GTiff", flags = c("overwrite"))
  temp <- raster(paste0("data/neighbors/TEMP.tif"))

  temp_values <- values(temp)
  temp_values <- temp_values[temp_values != 0]
  temp_values <- temp_values[temp_values != 1]
  temp_values <- temp_values[!is.na(temp_values)]
  
  temp_list_urb[[paste0("n", neighborhood)]] <- temp_values
  
  temp_mean <- mean(temp_values, na.rm=TRUE)
  n_df_urb <- bind_rows(n_df_urb, 
                    data.frame(neigh = neighborhood, 
                               mean_nb = temp_mean))
}
un <- unlist(temp_list_urb)
#hist_vals <-  hist(un, breaks =20, prob =TRUE)
#(hist_vals$density/sum(hist_vals$density))[1:5]
#mean(hist_vals$density)

all_means_urb <- (unlist(lapply(temp_list_urb, mean)))
plot(all_means_urb~dist_seq)
abline(a=mean(all_means_urb[-1]),b =0)
which.min(abs(all_means_urb[-1] - mean(all_means_urb[-1])))
39*90+(90/2)
# 0.1572956 // 3555

# AGEX

n_df_ag <- data.frame()
temp_list_ag <- list()

dist_seq <- seq(from = 1, to = 101, by=2)
for (neighborhood in dist_seq){
  execGRASS("r.neighbors", input = "lu.1990.ag", 
            output=paste0("lu.1990.ag.",neighborhood), 
            method = "average", size = neighborhood, 
            selection="lu.1990.2010.ag.dif",
            flags = c("overwrite", "c"))
  execGRASS("r.out.gdal", input = paste0("lu.1990.ag.",neighborhood), 
            output = paste0("data/neighbors/TEMP.tif"), 
            format = "GTiff", flags = c("overwrite"))
  temp <- raster(paste0("data/neighbors/TEMP.tif"))
  
  temp_values <- values(temp)
  temp_values <- temp_values[temp_values != 0]
  temp_values <- temp_values[temp_values != 1]
  temp_values <- temp_values[!is.na(temp_values)]
  
  temp_list_ag[[paste0("n", neighborhood)]] <- temp_values
  
  temp_mean <- mean(temp_values, na.rm=TRUE)
  n_df_ag <- bind_rows(n_df_ag, 
                    data.frame(neigh = neighborhood, 
                               mean_nb = temp_mean))
}
un <- unlist(temp_list_ag)
all_means_ag <- (unlist(lapply(temp_list_ag, mean)))
plot(all_means_ag~dist_seq)
abline(a=mean(all_means_ag[-1]), b = 0)
which.min(abs(all_means_urb[-1] - mean(all_means_urb[-1])))
39*90+(90/2)
# 0.4411392 // 3555