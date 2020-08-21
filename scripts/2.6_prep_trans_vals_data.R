#-------------------------------------------------------------------------------
## 2.6 Processing of land use change trends data
## 2020
#-------------------------------------------------------------------------------

# Remove all in environment
rm(list = ls())

## Load required packages ##

suppressPackageStartupMessages({
  library(raster)
  library(tidyverse)
  library(sf)
})

# Import ------------------------------------------------------------------


mun.sub.18.clean <- st_read("data/mun/munic_SHP_clean.shp")


# Calculate ---------------------------------------------------------------

source("scripts/functions/extract_vals_and_trans.R")
lu.18 <- stack(raster("data/raw/land_use/LandUse_AAFC/Z18_1990/IMG_AAFC_LANDUSE_Z18_1990.tif"),
               raster("data/raw/land_use/LandUse_AAFC/Z18_2000/IMG_AAFC_LANDUSE_Z18_2000.tif"),
               raster("data/raw/land_use/LandUse_AAFC/Z18_2010/IMG_AAFC_LANDUSE_Z18_2010.tif"))

names(lu.18) <- c("lu.1990.18", "lu.2000.18", "lu.2010.18")
mun.sub.18.clean <- st_read("data/mun//munic_SHP_clean.shp", quiet = TRUE)
mun.list <- as.character(unique((mun.sub.18.clean$MUS_NM_MUN)))
classes <- read_csv2( "data/raw/land_use/LandUse_AAFC/landUse_class.csv")

lu.18.sub <- mask(crop(lu.18, mun.sub.18.clean), mun.sub.18.clean)

All.Mont <- ExtractValsAndTrans(
  shape_vec = mun.list,
  shape_sf = mun.sub.18.clean,
  attribute = "MUS_NM_MUN",
  landUse_stack = lu.18.sub,
  classes = classes
)

#saveRDS(All.Mont, "temp.RDS")
#All.Mont <- readRDS("temp.RDS")

# Transform ---------------------------------------------------------------

TransTotal_saved.df <- data.frame()
for (mun in names(All.Mont$Transitions)) {
  trans.df <- All.Mont$Transitions[[mun]] %>%
    mutate(Municipality = mun)
  TransTotal_saved.df <- bind_rows(TransTotal_saved.df, trans.df)
}

Values_saved <- All.Mont$Values
names(Values_saved) <- c("1990", "2000", "2010")
Values_saved.df <- data.frame()
for (ts in names(Values_saved)){
  for (mun in names(Values_saved[[1]])) {
    trans.df <- t(Values_saved[[ts]][[mun]]) %>% as.data.frame() %>% 
      mutate(Year = ts, Municipality = mun) 
    Values_saved.df <- bind_rows(Values_saved.df, trans.df)
  }
}
Values_saved.df$Year <- as.numeric(Values_saved.df$Year)

# ----

write_csv(TransTotal_saved.df, "outputs/final/land_use_trans_df.csv")
write_csv(Values_saved.df, "outputs/final/land_use_vals_df.csv")

#-------------------------------------------------------------------------------
# Old data viz code

# # urb versus ag increase
# ggplot() + geom_sf(data = mun.sub.18.clean.mod, aes(fill = Trans_Pca$x[,1])) +
#   scale_fill_gradient2(low="black", high="yellow") +
#   geom_sf(data = mrc.mont.reproj, fill=NA, color=alpha("green",1))
# 
# ## !!
# 
# # forest or not forest converted
# ggplot() + geom_sf(data = mun.sub.18.clean.mod, aes(fill = Pca_1$x[,2])) +
#   scale_fill_gradient2() +
#   geom_sf(data = mrc.mont.reproj, fill=NA, color=alpha("green",1))
# 
# alpha.scale <- rescale((abs(Pca_1$x[,1]) + abs(Pca_1$x[,1])), to=c(0, 1))
# # membership versus strength of impact
# ggplot() + geom_sf(data = mun.sub.18.clean.mod, 
#                    aes(fill = memberships_names), alpha = alpha.scale) +
#   scale_fill_manual(values = c("yellow", "orange", "blue", "black")) +
#   geom_sf(data = mrc.mont.reproj, fill=NA, color=alpha("green",1)) +
#   theme_bw()

#-------------------------------------------------------------------------------

# with transition groups
# ggbiplot(Values_Pca, scale = 1,
#          obs.scale = 1, var.scale = 1,
#          groups =  trans_memberships_names,
#          ellipse = T, circle = TRUE) +
#   theme(panel.background = element_rect(fill = "white", colour = "grey50"),
#         legend.direction = 'horizontal', legend.position = 'top') +
#   ggtitle("PCA of Municipality profiles")


#####

# trans_mat_1990to2000 <-
#   TransTotal_saved.df %>%
#   group_by(Municipality, From, To) %>%
#   summarise(Freq=sum(Freq)) %>%
#   ungroup() %>% 
#   dplyr::filter(From!=To) %>%
#   mutate(trans_type = paste0(From,"to", To)) %>%
#   dplyr::select(-From, -To) %>%
#   spread(key=trans_type, value=Freq) %>%
#   rename(Mun=Municipality) %>%
#   arrange(Mun) %>% 
#   as.data.frame()

#-------------------------------------------------------------------------------