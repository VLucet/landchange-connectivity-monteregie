#-------------------------------------------------------------------------------
## 2.6 Multivariate analysis of land use change patterns
## 2020
#-------------------------------------------------------------------------------

# Remove all in environment
rm(list = ls())

## Load required packages ##

suppressPackageStartupMessages({
  library(raster)
  library(vegan)
  library(dendextend)
  library(ggbiplot)
  library(tidyverse)
  library(sf)
  #library(mvpart)
  library(spdep)
  library(ClustGeo)
  #library(const.clust)
  library(ggnewscale)
})

# Import ------------------------------------------------------------------


mun.sub.18.clean <- st_read("data/mun/munic_SHP_clean.shp")
mrc <- st_read("data/raw/vector/mrc_SHP/mrc_polygone.shp")
mrc.mont <- mrc %>% filter(MRS_NM_REG=="Montérégie") 
mrc.mont.reproj <- st_transform(mrc.mont, raster::crs(mun.sub.18.clean))


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

trans_mat_1990to2000 <- 
  TransTotal_saved.df %>% 
  dplyr::filter(Trans == "1990to2000") %>% 
  dplyr::filter(From!=To) %>% 
  mutate(trans_type = paste0(From,"to", To)) %>% 
  dplyr::select(-Trans, -From, -To) %>%
  spread(key=trans_type, value=Freq)
trans_only_mat <- 
  Filter(function(x)!all(is.na(x)), trans_mat_1990to2000) %>% 
  dplyr::select(-Municipality)
NameCode <- paste0(str_sub(trans_mat_1990to2000$Mun,1,3), 
                   1:length(trans_mat_1990to2000$Mun))
rownames(trans_only_mat) <- NameCode
trans_only_mat[is.na(trans_only_mat)] <- 0

trans_only_mat_noZero <- trans_only_mat[, colSums(trans_only_mat)>0]
trans_only_mat_noZero <- 
  trans_only_mat_noZero[, order(colSums(trans_only_mat_noZero), decreasing=T)]

values_mat_1990to2000 <- 
  Values_saved.df %>% 
  dplyr::filter(Year == 2010) %>%
  dplyr::select(-Year, -Unclassified)
values_only_mat <- 
  Filter(function(x)!all(is.na(x)), values_mat_1990to2000) %>% 
  dplyr::select(-Municipality)
NameCode <- paste0(str_sub(values_mat_1990to2000$Mun,1,3), 
                   1:length(values_mat_1990to2000$Mun))
rownames(values_only_mat) <- NameCode
values_only_mat[is.na(values_only_mat)] <- 0

values_only_mat_noZero <- values_only_mat[, colSums(values_only_mat)>0]
values_only_mat_noZero <- 
  values_only_mat_noZero[, order(colSums(values_only_mat_noZero), decreasing=T)]

# Clust
trans_dist <- dist(decostand(trans_only_mat_noZero[,1:4], "hel"))
trans_clust <- hclust(trans_dist, method = "ward.D2")

values_dist <- dist(decostand(values_only_mat_noZero[,1:4], "hel"))
values_clust <- hclust(values_dist, method = "ward.D2")

# Plotting

trans_dendro <- as.dendrogram(trans_clust)
trans_memberships <- cutree(trans_clust,4)

trans_memberships_names <- c("Urb-Def","AgEx-Def",
                             "AgEx-DeTree","Urb-DeAg")[trans_memberships]
labels_colors(trans_dendro) <- trans_memberships[trans_clust$order]
plot(trans_dendro)

# -----

values_dendro <- as.dendrogram(values_clust)
values_memberships <- cutree(values_clust,5)

values_memberships_names <- c("Forest - Dominant","Forest - Agriculture",
                              "Agriculture - Dominant"," Urban - MD", 
                              "Urban - HD", "6", "7", "8")[values_memberships]
labels_colors(values_dendro) <- values_memberships[values_clust$order]
plot(values_dendro)

# PCA
trans_minim.mat_noZero <- decostand(trans_only_mat_noZero[,1:4], 
                                    "norm")
names(trans_minim.mat_noZero) <- names(trans_only_mat_noZero[1:4])
names(trans_minim.mat_noZero) <- c("Fo_to_Ag", "Ag_to_Urb", "Tre_to_Ag", "Fo_to_Urb")
Trans_Pca <- prcomp(trans_minim.mat_noZero)

ggbiplot(Trans_Pca, scale = 1,
         obs.scale = 1, var.scale = 1,
         groups =  trans_memberships_names, #
         ellipse = T, circle = TRUE) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        legend.direction = 'horizontal', legend.position = 'top') +
  ggtitle("PCA of Municipality profiles")

trans_member.table <- tibble(MUS_NM_MUN = sort(trans_mat_1990to2000$Mun), 
                             NameCode = NameCode, Memberships = trans_memberships, 
                             Memberships_names = trans_memberships_names)

# values
values_minim.mat_noZero <- decostand(values_only_mat_noZero[,c(1:4)], 
                                     "hel")
Values_Pca <- prcomp(values_minim.mat_noZero)

ggbiplot(Values_Pca, scale = 1,
         obs.scale = 1, var.scale = 1,
         groups =  values_memberships_names,
         ellipse = T, circle = TRUE) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        legend.direction = 'horizontal', legend.position = 'top') +
  ggtitle("PCA of Municipality profiles")

# with transition groups
ggbiplot(Values_Pca, scale = 1,
         obs.scale = 1, var.scale = 1,
         groups =  trans_memberships_names,
         ellipse = T, circle = TRUE) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        legend.direction = 'horizontal', legend.position = 'top') +
  ggtitle("PCA of Municipality profiles")

values_member.table <- tibble(MUS_NM_MUN = sort(values_mat_1990to2000$Mun), 
                              NameCode = NameCode, Memberships = values_memberships, 
                              Memberships_names = values_memberships_names)

## Plot

trans_mun.sub.18.clean.mod <- 
  left_join(mun.sub.18.clean.merged, trans_member.table, by="MUS_NM_MUN")
ggplot() + geom_sf(data = trans_mun.sub.18.clean.mod, 
                   aes(fill = Memberships_names)) +
  scale_fill_manual(name = "Profiles",
                    values = c("#a1dab4", "#ffffcc", "#41b6c4", "#225ea8"), 
                    labels = c("Agricultural Expansion/Fragmentation",
                               "Agricultural Expansion/Deforestation",
                               "Urban Spread/Agricultural Loss",
                               "Urbain Spread/Deforestation")) +
  geom_sf(data = mrc.mont.reproj, fill=NA, color=alpha("black",1)) +
  theme_bw() +
  theme(legend.justification=c(0,1), legend.position=c(0.02,0.98))

values_mun.sub.18.clean.mod <- 
  left_join(mun.sub.18.clean.merged, values_member.table, by="MUS_NM_MUN")
ggplot() + geom_sf(data = values_mun.sub.18.clean.mod, 
                   aes(fill = Memberships_names)) +
  scale_fill_manual(name = "Profiles",
                    values = c("#228b22", "#9acd32", "#ffd700", "#b0c4de", "#2f4f4f"), 
                    labels = c("Forest - Dominant",
                               "Forest - Agriculture",
                               "Agriculture - Dominant",
                               "Urban - Medium Density", 
                               "Urban - High Density")) +
  geom_sf(data = mrc.mont.reproj, fill=NA, color=alpha("black",1)) +
  theme_bw() +
  theme(legend.justification=c(0,1), legend.position=c(0.02,0.98))

# urb versus ag increase
ggplot() + geom_sf(data = mun.sub.18.clean.mod, aes(fill = Trans_Pca$x[,1])) +
  scale_fill_gradient2(low="black", high="yellow") +
  geom_sf(data = mrc.mont.reproj, fill=NA, color=alpha("green",1))

## !!

# forest or not forest converted
ggplot() + geom_sf(data = mun.sub.18.clean.mod, aes(fill = Pca_1$x[,2])) +
  scale_fill_gradient2() +
  geom_sf(data = mrc.mont.reproj, fill=NA, color=alpha("green",1))

alpha.scale <- rescale((abs(Pca_1$x[,1]) + abs(Pca_1$x[,1])), to=c(0, 1))
# membership versus strength of impact
ggplot() + geom_sf(data = mun.sub.18.clean.mod, 
                   aes(fill = memberships_names), alpha = alpha.scale) +
  scale_fill_manual(values = c("yellow", "orange", "blue", "black")) +
  geom_sf(data = mrc.mont.reproj, fill=NA, color=alpha("green",1)) +
  theme_bw()

#-------------------------------------------------------------------------------
