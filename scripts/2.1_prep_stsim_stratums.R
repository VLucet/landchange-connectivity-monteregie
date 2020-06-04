#-------------------------------------------------------------------------------
## 2.1 Prepare stratum rasters for stsim
## 2020
## Inputs: land use, municipality, RMN data, ect..
## Outputs: stratum raster and datasheets for stsim
#-------------------------------------------------------------------------------

# reviewed 2020 

# Remove all in environment
rm(list = ls())

# Load required packages
suppressPackageStartupMessages({
  library(raster)
  library(lwgeom)
  library(sf)
  library(tidyverse)
  library(fasterize)
})

# Reset Raster tmp files
removeTmpFiles(0)
showTmpFiles()
#-------------------------------------------------------------------------------
# Set parameters 

R_AGGR <- list(ag = as.logical(Sys.getenv("R_AGGR", unset = TRUE)), 
               factor = as.numeric(Sys.getenv("R_AGGR_FACT", unset = 3)))

source("scripts/functions/aggregation_helpr.R")

#-------------------------------------------------------------------------------
# Import data
lu.stack <- stack(raster("data/land_use/LandUse_mont_aafc_30by30_1990.tif"), 
                  raster("data/land_use/LandUse_mont_aafc_30by30_2000.tif"),
                  raster("data/land_use/LandUse_mont_aafc_30by30_2010.tif"))
lu.stack.buf <- stack(raster("data/land_use/LandUse_mont_aafc_buffered_30by30_1990.tif"), 
                      raster("data/land_use/LandUse_mont_aafc_buffered_30by30_2000.tif"),
                      raster("data/land_use/LandUse_mont_aafc_buffered_30by30_2010.tif"))
names(lu.stack) <- c("lu_1990", "lu_2000", "lu_2010")
names(lu.stack.buf) <- c("lu_1990", "lu_2000", "lu_2010")

#-------------------------------------------------------------------------------
# PRIMARY STRATUM => MONT, NOT MONT, PA

# datasheet
stratum_df <- data.frame(Name= c("Not_Monteregie", "Monteregie", "PA"),
                         ID = c(0, 1, 2))
write.csv(stratum_df, "config/stsim/Stratum.csv", row.names=F)

# raster
buffer_all_empty <- lu.stack.buf$lu_1990
values(buffer_all_empty) <- NA

# Build buffers and mosaics
mont_all_ones <- lu.stack$lu_1990
mont_all_ones[!is.na(mont_all_ones)] <- 1

allextent_all_ones <- lu.stack$lu_1990
values(allextent_all_ones) <- 1

buffer_mont_with_ones <- mosaic(buffer_all_empty, mont_all_ones, fun = max, na.rm=T,
                                tolerance=0)
buffer_allextent_with_ones <- mosaic(buffer_all_empty, allextent_all_ones, fun = max, na.rm=T,
                                     tolerance=0)
stratum <- buffer_mont_with_ones
stratum[is.na(stratum)] <- 0 

# raster
RMN_data <- st_read("data/rmn/MCTPQ/MCTPQ_extraitQC20190527.shp", quiet = TRUE) %>% 
  st_transform(crs=crs(lu.stack$lu_1990))
RMN_data_cropped <- st_crop(sf::st_make_valid(RMN_data), lu.stack$lu_1990)
RMN_data_cropped$stratum <- 1

RMN_data_cropped_rast <- 
  fasterize(sf = st_collection_extract(RMN_data_cropped, "POLYGON"), 
            raster = lu.stack$lu_1990, field="stratum") %>% 
  mask(lu.stack$lu_1990)
RMN_data_cropped_rast[RMN_data_cropped_rast==1] <- 2

primary_stratum <- merge(RMN_data_cropped_rast, stratum)

writeRaster(primary_stratum, "data/stsim/primary_stratum_mont_or_not_or_PA_30by30", 
            format="GTiff", overwrite=T)
# sum(values(stratum == 1))  => 12543056

#-------------------------------------------------------------------------------

# SECONDARY STRATUM => MUNICIPALITIES (still needed for targets)
mun.sub.18.clean <- st_read("data/mun/munic_SHP_clean.shp", quiet = TRUE)

# datasheet
mun_for_stsim <- mun.sub.18.clean %>% 
  dplyr::select(MUS_NM_MUN) %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry) %>% 
  unique() %>% 
  mutate("ID" = seq(1:length(MUS_NM_MUN)))
colnames(mun_for_stsim) <- c("Name", "ID")
mun_for_stsim_with0 <- rbind(mun_for_stsim, data.frame(Name = "Not Monteregie", ID = 0))

write.csv(mun_for_stsim_with0, "config/stsim/SecondaryStratum.csv", row.names=F)

# rasterize
mun_for_stsim_renamed <- mun_for_stsim_with0 %>% 
  rename(MUS_NM_MUN = Name)

aggregated <- aggregate(mun.sub.18.clean, 
                        by = list(as.numeric(as.factor(mun.sub.18.clean$MUS_NM_MUN))), 
                        first) %>% 
  left_join(mun_for_stsim_renamed, by="MUS_NM_MUN") 
aggregated$geometry <- st_cast(aggregated$geometry, "MULTIPOLYGON")

mun.sub.18.clean.rast.30by30 <- 
  fasterize(sf = aggregated, raster = lu.stack$lu_1990, field="ID")
mun.sub.18.clean.rast.30by30[!is.na(mun.sub.18.clean.rast.30by30) & 
                               is.na(lu.stack$lu_1990)] <- NA
secondary_stratum <- 
  mosaic(mun.sub.18.clean.rast.30by30, buffer_all_empty, fun = max, na.rm=T, 
         toleraOMP_NUM_THREADSe=0)

writeRaster(secondary_stratum, 
            "data/stsim/secondary_stratun_mun_30by30",
            format="GTiff",
            overwrite=TRUE)

# secondarystratum.ag[is.na(secondarystratum.ag)] <- 0

#-------------------------------------------------------------------------------
## TERTIARY STRATUM => LAND TYPES
tertiary_stratum_ag <- raster("data/landis/spatial/mont_land_types.tif")

# datasheet
tertiary_stratum_df <- as.data.frame(freq(tertiary_stratum_ag))
tertiary_stratum_df_final <- tertiary_stratum_df %>% 
  dplyr::select(ID=value, -count) %>% 
  filter(!is.na(ID)) %>% 
  mutate(Name=paste0("lty_", ID))
tertiary_stratum_df_final[tertiary_stratum_df_final$ID==0,2] <- "Not_Forest"
tertiary_stratum_df_final[tertiary_stratum_df_final$ID==99,2] <- "Not_Monteregie"

write_csv(tertiary_stratum_df_final, "config/stsim/TertiaryStratum.csv")

#land_types_buf <- raster("data/landis/spatial/buf_mont_land_types.tif")
#land_types_buf_no_mont <- land_types_buf
#land_types_buf_no_mont[land_types != 99] <- NA
#land_types_freq <- freq(land_types)
#land_types_buf_no_mont_freq <- freq(land_types_buf_no_mont)

writeRaster(tertiary_stratum_ag, 
            "data/stsim/aggregated/tertiary_stratum_land_types.tif", 
            format="GTiff",
            overwrite = TRUE)
# sum(values(stratum == 1))  => 12543056
#-------------------------------------------------------------------------------

## AGGREGATION
# Primary
primary_stratum_ag <- aggregate(primary_stratum, fun=modal_custom_first, 
                                fact=R_AGGR$factor)
writeRaster(primary_stratum_ag, "data/stsim/aggregated/primary_stratum_mont_or_not_or_PA", 
            format="GTiff", overwrite=T)

# Secondary 
secondary_stratum_ag <- aggregate(secondary_stratum, fun=modal_custom_first, 
                                  fact=R_AGGR$factor)
writeRaster(secondary_stratum_ag, "data/stsim/aggregated/secondary_stratun_mun", 
            format="GTiff", overwrite=T)

# Tertiary 
# ==> ALREADY AGGREGATED
