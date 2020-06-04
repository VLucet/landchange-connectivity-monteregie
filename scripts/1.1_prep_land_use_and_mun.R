#-------------------------------------------------------------------------------
## 1.1 Process raw land use + municipality
## 2020
## Inputs: Raw land use data (AAFC) + municipality shapefiles
## Outputs: Processed land use data + clean municipality shapefile
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
  # library(rgrass7)
})

# Reset Raster tmp files
removeTmpFiles(0)
showTmpFiles()

#-------------------------------------------------------------------------------
# PART 1
#-------------------------------------------------------------------------------

# Load Land use data from AAFC

# Land use rasters are projected in UTM
# For Montreal/Mnteregie: UTM 18

# UTM 18
# Stack and check crs - can only stack if same extent and resolution
lu_18 <- stack(
  raster("data/raw/land_use/LandUse_AAFC/Z18_1990/IMG_AAFC_LANDUSE_Z18_1990.tif"),
  raster("data/raw/land_use/LandUse_AAFC/Z18_2000/IMG_AAFC_LANDUSE_Z18_2000.tif"),
  raster("data/raw/land_use/LandUse_AAFC/Z18_2010/IMG_AAFC_LANDUSE_Z18_2010.tif")
)
names(lu_18) <- c("lu_1990_18", "lu_2000_18", "lu_2010_18")
print(crs(lu_18))
crs_lu_18 <- crs(lu_18)

# Load table of land use classes - might be needed later
classes <- read.csv("data/raw/land_use/LandUse_AAFC/landUse_class.csv", sep = ";")

# Load administrative data (MRC/Municipality)

# Vector data: Municipalities and MRCs - data from Donnees Quebec
# Using sf package
mun_all <- st_read("data/raw/vector/munic_SHP/munic_polygone.shp", quiet = TRUE)
mrc_all <- st_read("data/raw/vector/mrc_SHP/mrc_polygone.shp", quiet = TRUE)

# Select Monteregie ONLY and reproject

# Select only polygons in monteregie
mun_sub <- mun_all %>%
  filter(MUS_NM_REG == "Montérégie")
mrc_sub <- mrc_all %>%
  filter(MRS_NM_REG == "Montérégie")

# Transfrom to UTM 18 - Monteregie
mun_sub_18 <- st_transform(mun_sub, crs = as.character(crs_lu_18))
mrc_sub_18 <- st_transform(mrc_sub, crs = as.character(crs_lu_18))

# In monteregie: the municipality polygons on water have NA for MUS_NM_MUN
# plotting to confirm:
# plot(mun_sub_18$geometry)
# plot(mun_sub_18[(is.na(mun_sub_18$MUS_NM_MUN)), ]$geometry, col = 2, add = T)
# plot(mun_sub_18[grep("TNO", as.character(mun_sub_18$MUS_NM_MUN)), ]$geometry,
#      col = 2, add = T
# )

# Removing NAs and TNOs
mun_sub_18_clean <- mun_sub_18 %>%
  filter(!is.na(MUS_NM_MUN)) %>%
  filter(!grepl("TNO", as.character(.$MUS_NM_MUN)))

# In CMM: the municipality polygons on water have NA for MUS_NM_MUN
# plotting to confirm:
# plot(cmm.all$geometry)
# plot(cmm.all[(is.na(cmm.all$MUS_NM_MUN)), ]$geometry, col = 2, add = T)
# Removing NAs
mun_sub_18_clean <- mun_sub_18_clean %>%
  filter(!is.na(MUS_NM_MUN))

# drop levels and be character
mun_sub_18_clean$MUS_NM_MUN <- droplevels(mun_sub_18_clean$MUS_NM_MUN)
mun_sub_18_clean$MUS_NM_MUN <- as.character(mun_sub_18_clean$MUS_NM_MUN)

## CREATES STANDARD HERE
st_write(mun_sub_18_clean, "data/mun/munic_SHP_clean.shp", 
         delete_dsn = T)

# Crop land use to selected extent
# Rectangular extent first - takes time - only need once
lu_18_sub_rect <- crop(lu_18, y = mun_sub_18_clean)
writeRaster(lu_18_sub_rect,
            "data/land_use/AAFC_zone18_mont_rect.tif",
            bylayer = TRUE,
            suffix = c("1990", "2000", "2010"),
            overwrite = TRUE
)

# Mask - takes time - only need once
masked <- mask(lu_18_sub_rect, mun_sub_18_clean)
suppressWarnings(lu_18_sub <- stack(masked))
writeRaster(lu_18_sub,
            "data/land_use/AAFC_zone18_mont_mask.tif",
            bylayer = TRUE,
            suffix = c("1990", "2000", "2010"),
            overwrite = TRUE
)

# With 50k buffer
buffer <- st_as_sf(st_buffer(st_union(mun_sub_18_clean), 50000))
lu_18_sub_buf <- trim(stack(mask(crop(lu_18, buffer), buffer)))
writeRaster(lu_18_sub_buf,
            "data/land_use/AAFC_zone18_mont_mask_buffered.tif",
            bylayer = TRUE,
            suffix = c("1990", "2000", "2010"),
            overwrite = TRUE
)
