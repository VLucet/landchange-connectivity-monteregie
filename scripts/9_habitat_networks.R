# Load libraries

library(tidyverse)
library(raster)
library(sf)
library(fasterize)

library(grainscape)
library(igraph)

library(foreach)
library(doParallel)

# Load file list in patches

# media_path <- "/media/vlucet/backup/results_march_2021"
media_path <- "/home/rstudio/land_con_monteregie"

patches <- list.files(file.path(media_path, "hab_suit/"), 
                      pattern = ".tif") # DOES NOT HAVE TRUE

grab_element <- function(string, patt, index, numeric = TRUE){
  splitted <- unlist(str_split(string, patt))[index]
  if(numeric){
    ret <- gsub("[^0-9.-]", "", splitted)
    ret <- as.numeric(ret)
  } else {
    ret <- gsub("[:digit:]", "", splitted)
  }
  return(ret)
}

patches_df <- data.frame(patches_raw_name = patches) %>%
  mutate(patches_short_name = gsub(patches_raw_name,
                                   pattern = "_for_b_scl_multiplied_s_su_all",
                                   replacement = "", fixed=T)) %>%
  rowwise() %>%
  mutate(timestep = grab_element(patches_short_name, "[.]", 3)) %>%
  mutate(iteration = grab_element(patches_short_name, "[.]", 2)) %>%
  mutate(species = grab_element(patches_short_name, "[_]", 1, numeric = FALSE)) %>%
  mutate(scenario = grab_element(patches_short_name, "[_]", 3)) %>%
  ungroup()


# Load files in reclassed (cost)
cost <- list.files(file.path(media_path, "reclassed/"), pattern = ".tif")
cost_sub <- cost[!sapply(cost, grepl, pattern="TRUE", fixed = TRUE)]

cost_sub_df <- data.frame(cost_raw_name = cost_sub) %>%
  rowwise() %>%
  mutate(timestep = grab_element(cost_raw_name, "[_]", 6)) %>%
  mutate(iteration = grab_element(cost_raw_name, "[_]", 4)) %>%
  mutate(species = grab_element(cost_raw_name, "[_]", 7, numeric = FALSE)) %>%
  mutate(scenario = grab_element(cost_raw_name, "[_]", 2)) %>%
  ungroup()

# Join them
all_joined <- left_join(patches_df, cost_sub_df,
                        by = c("timestep", "iteration", "species", "scenario")) %>%
  dplyr::select(-c(patches_short_name)) # %>% 
# mutate(numNodes = NA, numLinks = NA, totArea = NA, meanArea = NA, 
#        totQuality = NA, meanQuality = NA, totAreaQuality = NA, 
#        meanAreaQuality = NA, totCost = NA, meanCost = NA, 
#        overallECgap = NA, overallECnatal = NA)

dim(all_joined)

# Define clip area
clipAreaThreshold <- 0.8

DISP <- data.frame(Species = sort(unique(all_joined$species)),
                   PATCH   = c(0,   120,   0,  0,   60),
                   Natal   = c(459, 46659, 16, 564, 55088),
                   Gap     = c(39,  220,   10, 39,  236))

source("scripts/functions/hab_suit.R")

# -------------------------------------------------------------------------

# Get muns convec hull
mun <-
  st_read("data/mun/munic_SHP_clean.shp") %>%
  st_union() %>%
  st_as_sf()

mun_convex <- mun %>%
  st_convex_hull()

mun_buffer <- mun %>%
  st_buffer(4000) %>%
  st_crop(mun_convex)

# -------------------------------------------------------------------------

n_cores = 10 # very important smaller number of cores!!
clust <- makeCluster(n_cores, outfile="log.txt")
registerDoParallel(cl = clust)

final_df <- foreach(i = 1:8, .combine = dplyr::bind_rows) %dopar% {
  
  library(tidyverse)
  library(raster)
  library(sf)
  library(fasterize)
  
  library(grainscape)
  library(igraph)
  
  temp_row <- 
    data.frame(species = all_joined$species[i], iteration = all_joined$iteration[i], 
               timestep = all_joined$timestep[i], scenario = all_joined$scenario[i],
               numNodes = NA, numLinks = NA, totArea = NA, meanArea = NA, 
               totQuality = NA, meanQuality = NA, totAreaQuality = NA, 
               meanAreaQuality = NA, totCost = NA, meanCost = NA, 
               overallECgap = NA, overallECnatal = NA)
  
  print(i)
  print(temp_row)
  
  #for(i in 1:nrow(all_joined)){
  
  # Get test files
  test_patches <-
    crop(raster(file.path(media_path, "patches/hab_suit/", all_joined$patches_raw_name[i]))>0,
         mun_convex)
  test_cost <-
    crop(raster(file.path(media_path, "reclassed/", all_joined$cost_raw_name[i])),
         mun_convex)
  
  # Fasterize muns
  mun_raster <- fasterize(mun, raster=test_patches)
  mun_buffer_raster <- fasterize(mun_buffer, raster=test_patches)
  mun_convex_raster <- fasterize(mun_convex, raster=test_patches)
  
  # 1. get mpg
  test <-
    MPG(patch = mask(test_patches, mun_convex_raster),
        cost = mask(test_cost, mun_convex_raster))
  
  # 2. get the objects
  raster_list <- list(mpgpatchId = slot(test, "patchId"),
                      voronoi = slot(test, "voronoi"),
                      lcpPerimWeight = slot(test, "lcpPerimWeight"),
                      lcpLinkId = slot(test, "lcpLinkId"),
                      mpgPlot = slot(test, "mpgPlot"))
  
  df_list <- unlist(graphdf(slot(test, "mpg")), recursive = FALSE)
  
  # 3. Crop to mun
  raster_list_cropped <- lapply(raster_list, function(x){mun_buffer_raster*x})
  
  # 4. Evaluate set
  diff_set <-
    setdiff(unique(raster_list$mpgpatchId), unique(raster_list_cropped$mpgpatchId))
  
  # 5. Deal with patches out
  count_in <- freq(raster_list_cropped$mpgpatchId)
  
  replacelist <- matrix(c(NA,1,1,NA),ncol=2)
  mun_raster_OUT <- raster::reclassify(mun_buffer_raster,replacelist)
  
  raster_list_cropped_OUT <- lapply(raster_list, function(x){mun_raster_OUT*x})
  
  count_out <- freq(raster_list_cropped_OUT$mpgpatchId)
  
  # which patches straddle the boundary?
  overlap_patches_in <- count_in[count_in[,1] %in% count_out[,1],]
  overlap_patches_out <- count_out[count_out[,1] %in% count_in[,1],]
  
  #calculate their percernt area inside the ecological region
  overlap_patches <-
    data.frame(id=overlap_patches_in[,1],
               incount=overlap_patches_in[,2],
               outcount=overlap_patches_out[,2])
  overlap_patches$percentin <-
    overlap_patches$incount/(overlap_patches$incount+overlap_patches$outcount)
  
  # identify patches to remove if they have less than clipAreaThreshold proportion
  # of their area inside the ecological region
  remove_patches_overlap <- overlap_patches[overlap_patches$percentin < clipAreaThreshold, ]
  keep_patches_overlap <- overlap_patches[overlap_patches$percentin >= clipAreaThreshold, ]
  
  remove_patches_notoverlap <- setdiff(diff_set, keep_patches_overlap$id)
  remove_patches <- union(remove_patches_notoverlap, remove_patches_overlap$id)
  
  #Links to delete
  node1_rowids <- which(df_list$e$e1 %in% remove_patches)
  node2_rowids <- which(df_list$e$e2 %in% remove_patches)
  rowids_del <- union(node1_rowids, node2_rowids)
  
  #draw clipped link layer
  replacelist <- cbind(-df_list$e[rowids_del, "linkId"], NA)
  linkidmap_reg <- raster::reclassify(raster_list$lcpLinkId, replacelist)
  linkStats_reg <- df_list$e[-rowids_del, ]
  
  # draw clipped patch layer with patches on the boundary removes if their area
  # inside ecol region is < 0.8
  # also removes patches that were only connected to
  # other patches in the ecoregion indirectly via paths outside the ecoregion
  replacelist <- cbind(c(0, setdiff(unique(raster_list$mpgpatchId),
                                    unique(c(linkStats_reg$e1,
                                             linkStats_reg$e2)))), NA)
  patchidmap_reg<-raster::reclassify(raster_list_cropped$mpgpatchId, replacelist)
  
  
  # Now we have linkidmap_reg, linkStats_reg, patchidmap_reg to proceeed with
  
  # 6. Define graph object using data.frame
  landscape.graph <-
    graph.data.frame(data.frame(linkStats_reg[,c("e2","e1")]), directed=FALSE) #,vertices=patchquality)
  
  # find biggest cluster
  C <- clusters(landscape.graph)
  
  if(C$no > 1){
    bigclus<-which(C$csize==max(C$csize))
    bigClusNodes<-as.numeric(V(landscape.graph)$name[which(C$membership==bigclus)])
    
    #Define graph object in which all the nodes belong to a single cluster
    landscape.graph.NotInBigClus<-delete.vertices(landscape.graph,which(C$membership==bigclus))
    linksNotInBigClus<-matrix(as.numeric(get.edgelist(landscape.graph.NotInBigClus)),
                              ecount(landscape.graph.NotInBigClus), 2)
    
    links_to_delete<-c(linkStats_reg$linkId[which(linkStats_reg$e1 %in% linksNotInBigClus[,1] & linkStats_reg$e2 %in% linksNotInBigClus[,2])],
                       linkStats_reg$linkId[which(linkStats_reg$e2 %in% linksNotInBigClus[,1] & linkStats_reg$e1 %in% linksNotInBigClus[,2])])
    linkStats_reg<-linkStats_reg[-which(linkStats_reg$linkId %in% links_to_delete),]
    
    #draw clipped link layer
    replacelist<-cbind(-links_to_delete,NA)
    linkidmap_reg<-raster::reclassify(linkidmap_reg,replacelist)
    
    #draw clipped patchid layer
    replacelist<-cbind(as.numeric(V(landscape.graph)$name[which(C$membership!=bigclus)]), NA)
    patchidmap_reg<-raster::reclassify(patchidmap_reg,replacelist)
  }
  
  patchStats_reg <- df_list$v[which(df_list$v$patchId%in%unique(c(linkStats_reg$e1,
                                                                  linkStats_reg$e2))),]
  
  # -------------------------------------------------------------------------
  
  ############################
  # Analyze habitat networks #
  ############################
  
  
  # If this section is being run after the previous sections then set up output tables and load in the necessary files from disk
  # for(i in 4:5){#1:length(speciesList)){
  species <- all_joined$species[i]
  
  #species-specific estimates of gap-crossing and natal median dispersal distances
  d50GAP<-DISP$Gap[DISP$Species == species]
  d50NATAL<-DISP$Natal[DISP$Species == species]
  
  #dispersal distance coefficients
  coefficientGAP<-log(0.5)/d50GAP
  coefficientNATAL<-log(0.5)/d50NATAL
  
  # BTSL extent
  # nodes<-patchStats_ecol
  # links<-linkStats_ecol
  # patchId<-patchidmap_ecol
  nodes <- patchStats_reg
  links <- linkStats_reg
  patchId <- patchidmap_reg
  
  #produce patch-level summary of habitat quality
  habitatquality <- crop(raster(file.path(media_path, "patches/hab_suit/", 
                                          all_joined$patches_raw_name[i])), patchId)
  nodeQuality <- data.frame(zonal(habitatquality, patchId, fun='mean'))
  
  #add in node quality
  nodes <- merge(nodes, nodeQuality, by.x="patchId", by.y="zone")
  nodes <- data.frame(name=nodes$patchId, area=nodes$patchArea, quality=nodes$mean, 
                      areaquality=(nodes$patchArea*nodes$mean/100))
  
  #define graph object using data.frame
  landscape.graph.clipped<-graph.data.frame(links, directed=FALSE, vertices=nodes)
  
  # Network Summary Statistics
  temp_row$numNodes <- vcount(landscape.graph.clipped)
  temp_row$numLinks <- ecount(landscape.graph.clipped)
  temp_row$totArea <- sum(as.numeric(V(landscape.graph.clipped)$area))
  temp_row$meanArea <- mean(as.numeric(V(landscape.graph.clipped)$area))
  temp_row$totQuality <- sum(as.numeric(V(landscape.graph.clipped)$quality))
  temp_row$meanQuality <- mean(as.numeric(V(landscape.graph.clipped)$quality))
  temp_row$totAreaQuality <- sum(as.numeric(V(landscape.graph.clipped)$areaquality))
  temp_row$meanAreaQuality <- mean(as.numeric(V(landscape.graph.clipped)$areaquality))
  temp_row$totCost <- sum(as.numeric(E(landscape.graph.clipped)$lcpPerimWeight))
  temp_row$meanCost <- mean(as.numeric(E(landscape.graph.clipped)$lcpPerimWeight))
  temp_row$overallECgap <- overall.indices.standard(landscape.graph.clipped, coefficientGAP,'lcpPerimWeight','areaquality')
  temp_row$overallECnatal <- overall.indices.standard(landscape.graph.clipped, coefficientNATAL,'lcpPerimWeight','areaquality')
  
  # ### edge.betweenness.community
  # ebc <- edge.betweenness.community(landscape.graph.clipped, modularity=TRUE, membership=TRUE)
  #
  # netsumBTSL[i, "modularity"]<-max(ebc$modularity)
  # netsumBTSL[i, "numCommunities"]<-length(ebc)
  # netsumBTSL[i, "meanNodesperComm"]<-mean(sizes(ebc))
  #
  # V(landscape.graph.clipped)$comm<-membership(ebc)
  # Vmatrix<-data.frame(V(landscape.graph.clipped)$patchId, V(landscape.graph.clipped)$area, V(landscape.graph.clipped)$areaquality, V(landscape.graph.clipped)$comm)
  # names(Vmatrix)<-c("v1", "v2", "v3", "v4")
  # netsumBTSL[i, "meanAreaperComm"]<-mean(ddply(Vmatrix, "v4", function(df) sumarea=sum(df$v2))[,2])
  # netsumBTSL[i, "meanAreaQualityperComm"]<-mean(ddply(Vmatrix, "v4", function(df) sumarea=sum(df$v3))[,2])
  
  # Betweenness
  
  #tabular betweenness output
  btwn<-data.frame(patchId=as.numeric(V(landscape.graph.clipped)$name), 
                   btwn=betweenness(landscape.graph.clipped, 
                                    weights=E(landscape.graph.clipped)$lcpPerimWeight, 
                                    directed=FALSE))
  
  #raster betweenness output raw
  #replace patch ids with betweeness values
  btwnMap<-reclassify(patchId, btwn)
  
  #raster betweenness output 0 - 1
  #make a look-up table between patch id and betweenness value
  btwn_lookup<-cbind(patchId=btwn$patchId, 
                     btwn=1/(max(btwn$btwn)-min(btwn$btwn))*(btwn$btwn-min(btwn$btwn)))
  
  #replace patch ids with betweeness values 0 - 1
  btwnMap01<-reclassify(patchId, btwn_lookup)
  
  #Save betweennness outputs
  btwnName<-paste0(tools::file_path_sans_ext(all_joined$cost_raw_name[i]), 
                   "betweenness_BTSL.csv")
  btwnMapName<-paste0(tools::file_path_sans_ext(all_joined$cost_raw_name[i]), 
                      "betweenness_BTSL.tif")
  btwnMapName01<-paste0(tools::file_path_sans_ext(all_joined$cost_raw_name[i]), 
                        "betweenness_BTSL_01.tif")
  
  networkDir <- file.path(media_path, "network")
  
  writeRaster(btwnMap, filename=file.path(networkDir, btwnMapName), overwrite=TRUE)
  writeRaster(btwnMap01, filename=file.path(networkDir, btwnMapName01), overwrite=TRUE)
  write.csv(btwn, file.path(networkDir, btwnName), row.names=F)
  
  # Return value
  print(temp_row)
  temp_row
  
}

stopCluster(clust)

write_csv(final_df, "outputs/final/final_df_network_stats.csv")
