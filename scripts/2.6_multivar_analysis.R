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
  library(RColorBrewer)
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

# lu.18.sub <- mask(crop(lu.18, mun.sub.18.clean), mun.sub.18.clean)
# 
# All.Mont <- ExtractValsAndTrans(
#   shape_vec = mun.list,
#   shape_sf = mun.sub.18.clean,
#   attribute = "MUS_NM_MUN",
#   landUse_stack = lu.18.sub,
#   classes = classes
# )

#saveRDS(All.Mont, "temp.RDS")
All.Mont <- readRDS("temp.RDS")

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

# NameCode <- paste0(str_sub(trans_mat_1990to2000$Mun,1,3), 
#                    1:length(trans_mat_1990to2000$Mun))

# Trans

trans_mat_1990to2000 <- 
  TransTotal_saved.df %>% 
  dplyr::filter(Trans == "1990to2000", From!=To) %>% 
  mutate(trans_type = paste0(From,"to", To)) %>% 
  dplyr::select(-Trans, -From, -To) %>%
  spread(key=trans_type, value=Freq) %>% 
  rename(Mun=Municipality) %>% 
  arrange(Mun)
rownames(trans_mat_1990to2000) <- trans_mat_1990to2000$Mun
trans_only_mat <- Filter(function(x)!all(is.na(x)), trans_mat_1990to2000) %>% 
  mutate(Mun = paste0(str_sub(Mun,1,3), 1:length(Mun)))
trans_only_mat[is.na(trans_only_mat)] <- 0

trans_only_mat_noZero <- trans_only_mat[, c(TRUE, colSums(select(trans_only_mat, -Mun))>0)]
trans_only_mat_noZero <- 
  trans_only_mat_noZero[, c(1, order(colSums(select(trans_only_mat_noZero, -Mun)), decreasing=T)+1)]
rownames(trans_only_mat_noZero) <- trans_only_mat$Mun 

# Values

values_mat_1990to2000 <- 
  Values_saved.df %>% 
  dplyr::filter(Year == 2010) %>%
  dplyr::select(-Year, -Unclassified) %>% 
  rename(Mun=Municipality) %>% 
  arrange(Mun) %>% 
  select(Mun, everything())
rownames(values_mat_1990to2000) <- values_mat_1990to2000$Mun
values_only_mat <- Filter(function(x)!all(is.na(x)), values_mat_1990to2000) %>% 
  mutate(Mun = paste0(str_sub(Mun,1,3), 1:length(Mun)))
values_only_mat[is.na(values_only_mat)] <- 0

values_only_mat_noZero <- values_only_mat[, c(TRUE, colSums(select(values_only_mat, -Mun))>0)]
values_only_mat_noZero <- 
  values_only_mat_noZero[, c(1, order(colSums(select(values_only_mat_noZero, -Mun)), decreasing=T)+1)]
rownames(values_only_mat_noZero) <- values_only_mat$Mun

# -----------------------------------------------------------------------------

# Clust

# Trans

trans_dist <- dist(decostand(trans_only_mat_noZero[,2:5], "hel"))
trans_clust <- hclust(trans_dist, method = "ward.D2")

trans_dendro <- as.dendrogram(trans_clust)
trans_memberships <- cutree(trans_clust,4)
# trans_memberships_names <- c("Urb-Def","AgEx-Def",
#                              "AgEx-DeTree","Urb-DeAg")[trans_memberships]
labels_colors(trans_dendro) <- trans_memberships[trans_clust$order]
plot(trans_dendro)

# Vals

values_dist <- dist(decostand(values_only_mat_noZero[,2:5], "hel"))
values_clust <- hclust(values_dist, method = "ward.D2")

values_dendro <- as.dendrogram(values_clust)
values_memberships <- cutree(values_clust,5)
# values_memberships_names <- c("Forest - Dominant","Forest - Agriculture",
#                               "Agriculture - Dominant"," Urban - MD", 
#                               "Urban - HD")[values_memberships]
labels_colors(values_dendro) <- values_memberships[values_clust$order]
plot(values_dendro)

# Figure out % of urban land in smallest group

# grp <- as.numeric(names(sort(table(values_memberships))[1]))
# urb <- values_only_mat_noZero[which(values_memberships == 4),]

# -----------------------------------------------------------------------------

# c('#1f78b4','#33a02c','#b2df8a','#a6cee3')

# PCA + map 

the_dpi = 300

# Trans

trans_minim.mat_noZero <- decostand(trans_only_mat_noZero[,2:5], "hel")
                                 # "41to51"  "51to21"     "45to51"     "41to21"
names(trans_minim.mat_noZero) <- c("Fo_to_Ag", "Ag_to_Urb", "Tre_to_Ag", "Fo_to_Urb")

Trans_Pca <- prcomp(trans_minim.mat_noZero)

trans_biplot <- ggbiplot(Trans_Pca, scale = 1,
         obs.scale = 1, var.scale = 1,
         groups =  as.factor(trans_memberships),
         ellipse = T, circle = TRUE) +
  scale_color_manual(name = "Profiles",
                     values = c('#1f78b4','#33a02c','#b2df8a','#a6cee3'), 
                     labels = c("Urbain Spread/Deforestation",
                                "Agricultural Expansion/Deforestation",
                                "Agricultural Expansion/Fragmentation", 
                                "Urban Spread/Agricultural Loss")) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        legend.direction = 'horizontal', legend.position = 'top') +
  ggtitle("PCA of Municipality profiles")

ggsave(plot = trans_biplot, "thesis/figures/PCA_trans_profiles.png", 
       height = 8, width = 10, dpi = the_dpi)

trans_member.table <- data.frame(MUS_NM_MUN = rownames(trans_mat_1990to2000)) %>% 
  mutate(code = paste0(str_sub(MUS_NM_MUN,1,3), 1:length(MUS_NM_MUN)), 
         membership = as.factor(trans_memberships))

# tbl.names <- table(mun.sub.18.clean$MUS_NM_MUN)
# more.than.one <- tbl.names[tbl.names>1]

mun.sub.18.clean %>% 
  select(MUS_NM_MUN) %>% 
  group_by(MUS_NM_MUN) %>%
  dplyr::summarise(MUS_NM_MUN_New = first(MUS_NM_MUN)) ->
  mun.sub.18.clean.merged

trans_mun.sub.18.clean.mod <- 
  left_join(mun.sub.18.clean.merged, trans_member.table, by="MUS_NM_MUN")

map_trans <- ggplot() + geom_sf(data = trans_mun.sub.18.clean.mod, 
                   aes(fill = as.factor(membership))) +
  scale_fill_manual(name = "Profiles",
                    values = c('#1f78b4','#33a02c','#b2df8a','#a6cee3'), 
                    labels = c("Urbain Spread/Deforestation",
                               "Agricultural Expansion/Deforestation",
                               "Agricultural Expansion/Fragmentation", 
                               "Urban Spread/Agricultural Loss")) +
  geom_sf(data = mrc.mont.reproj, fill=NA, color=alpha("black",1)) +
  theme_bw() +
  theme(legend.justification=c(0,1), legend.position=c(0.02,0.98))

ggsave(plot = map_trans, "thesis/figures/transition_prof_map.png", 
       height = 8, width = 12, dpi = the_dpi)

# -----------------------------------------------------------------------------

# Vals

# c("#B2DF8A", "#33A02C", "#FF7F00", "#CAB2D6", "#6A3D9A")

values_minim.mat_noZero <- decostand(values_only_mat_noZero[,c(2:5)], "hel")

Values_Pca <- prcomp(values_minim.mat_noZero)

vals_biplot <- ggbiplot(Values_Pca, scale = 1,
         obs.scale = 1, var.scale = 1,
         groups = as.factor(values_memberships),
         ellipse = T, circle = TRUE) +
  scale_color_manual(name = "Profiles",
                    values = c("#33A02C", "#B2DF8A", "#FF7F00", "#CAB2D6", "#6A3D9A"), 
                    labels = c("Forest - Agriculture",
                               "Forest - Dominant",
                               "Agriculture - Dominant",
                               "Urban - Medium Density", 
                               "Urban - High Density"))+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        legend.direction = 'horizontal', legend.position = 'top') +
  ggtitle("PCA of Municipality profiles")

ggsave(plot = vals_biplot, "thesis/figures/PCA_data_profiles.png", 
       height = 8, width = 10, dpi = the_dpi)


values_member.table <- data.frame(MUS_NM_MUN = rownames(values_mat_1990to2000)) %>% 
  mutate(code = paste0(str_sub(MUS_NM_MUN,1,3), 1:length(MUS_NM_MUN)), 
         membership = as.factor(values_memberships))

values_mun.sub.18.clean.mod <- 
  left_join(mun.sub.18.clean.merged, values_member.table, by="MUS_NM_MUN")

map_vals <- ggplot() + geom_sf(data = values_mun.sub.18.clean.mod, 
                   aes(fill = as.factor(membership))) +
  scale_fill_manual(name = "Profiles",
                    values = c("#33A02C", "#B2DF8A", "#FF7F00", "#CAB2D6", "#6A3D9A"), 
                    labels = c("Forest - Agriculture",
                               "Forest - Dominant",
                               "Agriculture - Dominant",
                               "Urban - Medium Density", 
                               "Urban - High Density")) +
  geom_sf(data = mrc.mont.reproj, fill=NA, color=alpha("black",1)) +
  theme_bw() +
  theme(legend.justification=c(0,1), legend.position=c(0.02,0.98))

ggsave(plot = map_vals, "thesis/figures/profiles_land_use.png", 
       height = 8, width = 12, dpi = the_dpi)

#-------------------------------------------------------------------------------

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