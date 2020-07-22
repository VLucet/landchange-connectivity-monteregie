#-------------------------------------------------------------------------------
## 1.0 Multivariate analysis of land use change patterns
## 2020
#-------------------------------------------------------------------------------

# Remove all in environment
rm(list = ls())

## Load required packages ##

suppressPackageStartupMessages({
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
## Import 

mun.sub.18.clean <- st_read("data/mun/munic_SHP_clean.shp")
mrc <- st_read("data/raw/vector/mrc_SHP/mrc_polygone.shp")
mrc.mont <- mrc %>% filter(MRS_NM_REG=="Montérégie") 
mrc.mont.reproj <- st_transform(mrc.mont, raster::crs(mun.sub.18.clean))

trans_and_vals <- readRDS("data/temp/vals_and_trans.RDS")

TransTotal_saved.df <- data.frame()
for (mun in names(trans_and_vals$Transitions)) {
  trans.df <- trans_and_vals$Transitions[[mun]] %>%
    mutate(Mun = mun)
  TransTotal_saved.df <- bind_rows(TransTotal_saved.df, trans.df)
}

Values_saved.df <-

trans_mat_1990to2000 <- 
  TransTotal_saved.df %>% 
  dplyr::filter(Trans == "1990to2000") %>% 
  dplyr::filter(From!=To) %>% 
  mutate(trans_type = paste0(From,"to", To)) %>% 
  dplyr::select(-Trans, -From, -To) %>%
  spread(key=trans_type, value=Freq)
trans_only_mat <- 
  Filter(function(x)!all(is.na(x)), trans_mat_1990to2000) %>% 
  dplyr::select(-Mun)
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
trans_kmeans <- cascadeKM(decostand(trans_only_mat_noZero[,1:4], "hel"), 1,4)
trans_kmeans_memberships <- trans_kmeans$partition[,2] # 2groups

values_dist <- dist(decostand(values_only_mat_noZero[,1:4], "hel"))
values_clust <- hclust(values_dist, method = "ward.D2")
values_kmeans <- cascadeKM(decostand(values_only_mat_noZero[,1:4], "hel"), 1,10)
values_kmeans_memberships <- values_kmeans$partition[,8] # 2groups

# Spatial 

tbl.names <- table(mun.sub.18.clean$MUS_NM_MUN)
more.than.one <- tbl.names[tbl.names>1]
mun.sub.18.clean %>% 
  select(MUS_NM_MUN) %>% 
  group_by(MUS_NM_MUN) %>%
  dplyr::summarise(MUS_NM_MUN_New = first(MUS_NM_MUN)) ->
  mun.sub.18.clean.merged

centroids <- st_centroid(mun.sub.18.clean.merged)
mun.coords <- (st_coordinates(centroids))
centroids.withCoords <- centroids %>% cbind(mun.coords) 
st_geometry(centroids.withCoords) <- NULL

listW <- nb2listw(graph2nb(gabrielneigh(mun.coords)), zero.policy=T)
neighbors <- listw2sn(listW)[,1:2]

# plot(mun.coords, type='n',asp=1)
# title("Delaunay triangulation")
# text(mun.coords, labels=1:16, pos=3)
# for(i in 1:nrow(neighbors)) {
#   lines(rbind(mun.coords[neighbors[i,1],], mun.coords[neighbors[i,2],]))
# }

links.mat.dat <- contiguity.mat(neighbors, 177)

trans_spatial <- constrained.clust(trans_dist,links.mat.dat)
trans_spatial_2 <- hclustgeo(D0=dist_1, D1=as.dist(links.mat.dat), alpha=0.1)

values_spatial <- constrained.clust(values_dist,links.mat.dat)

# Plotting

trans_dendro <- as.dendrogram(trans_clust)
trans_memberships <- cutree(trans_clust,4)
#memberships <- kmeans_memberships
#memberships <- replace(x=memberships, list=memberships==3, 2)

trans_memberships_names <- c("Urb-Def","AgEx-Def",
                             "AgEx-DeTree","Urb-DeAg")[trans_memberships]
labels_colors(trans_dendro) <- trans_memberships[trans_clust$order]
plot(trans_dendro)

values_dendro <- as.dendrogram(values_clust)
values_memberships <- cutree(values_clust,5)
#values_memberships <- values_kmeans_memberships

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
#-------------------------------------------------------------------------------

## !

# => Constrained on census data

# Import census data 
census <- st_read("Part_A_statModel/Model_Outputs/Multivar_CensusData.shp")
setwd(wd)  

st_geometry(census) <- NULL
minim.mat_noZero.df <- c()
minim.mat_noZero.df <- 
  trans_only_mat_noZero %>% #not transformed
  as.data.frame() %>% 
  dplyr::mutate(NameCode = rownames(trans_only_mat_noZero))

census[is.na(census)] <- 0

# CANT TRUST INCOME 2011
# Making new vars and saving outputs
census %>% mutate(in_change = (Inc_01-Inc_91)) %>%
  mutate(pop_change = (Pop_11-Pop_91)) %>% 
  left_join(trans_member.table, by = "MUS_NM_MUN") %>% 
  left_join(minim.mat_noZero.df, by = "NameCode")  ->
  census.withChange

saveRDS(census.withChange, "Part_A_statModel/Model_Data/CensusData/Data_gam/CensusDataWithChange.RDS")

## For this analysis (older code)
census.diff <- census[,9:15] - census[,2:8]
names(census.diff) <- paste0("Diff_", names(census.diff))
census.diff[census.diff == Inf] <- 0
census.diff[apply(census.diff,2,is.nan)] <- 0
census <- cbind(census, census.diff)

census.withcode <- census %>% 
  left_join(trans_member.table, by = "MUS_NM_MUN") %>% 
  left_join(minim.mat_noZero.df, by = "NameCode") 

rda.1 <- rda(decostand(census.withcode[,26:29], "normalize"),
             decostand(census.withcode[,16:22], "normalize"))

plot(rda.1, type = "n")
adj.cols = trans_memberships[match(census.withcode$NameCode, names(trans_memberships))]
adj.names = trans_memberships_names[match(census.withcode$NameCode, names(trans_memberships))]
points(rda.1, display="sites", col = adj.cols)
points(rda.1, display = 'species', pch = '+', cex = 3, col = 1)
text(rda.1, display="bp", col=2)

mvpart(Memberships_names~
         Pop_91+Mob_91+Edu_0_91+Edu_1_91+LabF_91+TotDw_91+Inc_91, data=census.withcode)
