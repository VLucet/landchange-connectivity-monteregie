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
  library(sf)
  library(tidyverse)
  library(fasterize)
})

# Reset Raster tmp files
removeTmpFiles(0)
showTmpFiles()

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
# PRIMARY STRATUM => MONT or not
# datasheet
stratum_df <- data.frame(Name= c("Monteregie", "Not_Monteregie"), 
                         ID = c(1,0))
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

# Write out 1st stratum
stratum <- buffer_mont_with_ones
stratum[is.na(stratum)] <- 0 
writeRaster(stratum, "data/stsim/primary_stratum_mont_or_not_30by30", 
            format="GTiff", overwrite=T)
# sum(values(stratum == 1))  => 12543056

#-------------------------------------------------------------------------------

# SECONDARY STRATUM => MUNICIPALITIES
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
mun.sub.18.clean.rast.30by30.with.buf <- 
  mosaic(mun.sub.18.clean.rast.30by30, buffer_all_empty, fun = max, na.rm=T, 
         toleraOMP_NUM_THREADSe=0)

writeRaster(mun.sub.18.clean.rast.30by30.with.buf, 
            "data/stsim/secondary_stratun_mun_30by30",
            format="GTiff",
            overwrite=TRUE)

# secondarystratum.ag[is.na(secondarystratum.ag)] <- 0

#-------------------------------------------------------------------------------
## TERTIARY STRATUM => RMN DATA
# datasheet
stratum_df <- data.frame(Name= c("PA", "Not PA"), 
                         ID = c(1,0))
write.csv(stratum_df, "config/stsim/TertiaryStratum.csv", row.names=F)

# raster
RMN_data <- st_read("data/rmn/MCTPQ/MCTPQ_extraitQC20190527.shp", quiet = TRUE) %>% 
  st_transform(crs=crs(lu.stack$lu_1990))
RMN_data_cropped <- st_crop(sf::st_make_valid(RMN_data), lu.stack$lu_1990)
RMN_data_cropped$stratum <- 1

RMN_data_cropped_rast <- 
  fasterize(sf = st_collection_extract(RMN_data_cropped, "POLYGON"), 
            raster = lu.stack$lu_1990, field="stratum") %>% 
  mask(lu.stack$lu_1990)
RMN_data_cropped_rast[!is.na(lu.stack$lu_1990) & is.na(RMN_data_cropped_rast)] <- 0
RMN_data_cropped_rast_with_buf <- 
  mosaic(RMN_data_cropped_rast, buffer_all_empty, fun = max, na.rm=T, 
         tolerance=0)
writeRaster(RMN_data_cropped_rast_with_buf, 
            "data/stsim/tertiary_stratum_PA_30by30", 
            format="GTiff",
            overwrite = TRUE)
#-------------------------------------------------------------------------------