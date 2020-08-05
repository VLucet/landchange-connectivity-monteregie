draw_scenario <- function(sce = NULL, 
                          iter = 1,
                          mxp = 10000,
                          sce_folder = "../libraries/stsim/monteregie-conncons-scripted.ssim.output"){
  
  # Classes stuff
  classes <- read_csv("../config/rcl_tables/land_use/recode_table.csv")
  forest_classes <- read_csv("../config/rcl_tables/land_use/recode_table_forest.csv") %>%
    rename(new_code = "ID", new_class = "Name")
  classes_unique <- unique(classes[, c("new_code", "new_class")]) %>%
    bind_rows(forest_classes)
  
  stratum <- 
    raster("../data/stsim/aggregated/primary_stratum_mont_or_not_or_PA.tif ")
  
  # Sce dirs
  # compare_dir <- file.path(
  #   "../libraries/stsim/monteregie-conncons-scripted.ssim.output/Scenario-38/stsim_OutputSpatialState")
  
  sce_dirs <- unlist(lapply(list.dirs(sce_folder, recursive=F), 
                            FUN = grep, pattern="Scenario", value=T))
  sce_dirs <- file.path(sce_dirs, "stsim_OutputSpatialState")
  sce_dir <- sce_dirs[str_detect(sce_dirs, as.character(sce))]
  
  compare_file <- file.path(sce_dir,paste0("sc.it",as.character(iter),".ts2.tif"))
  compare_raster <- raster(compare_file)
  compare_raster[stratum==0] <- NA
  compare_raster <- trim(compare_raster)
  
  sce_file <-  file.path(sce_dir,paste0("sc.it",as.character(iter),".ts11.tif"))
  sce_raster <- raster(sce_file)
  sce_raster[stratum==0] <- NA
  sce_raster <- trim(sce_raster)
  
  stack_to_plot <- stack(compare_raster, sce_raster)
  names(stack_to_plot) <- c("BAU", "Alternative")
  
  BAU <- ratify(stack_to_plot$BAU)
  rat <- levels(BAU)[[1]] %>% 
    left_join(classes_unique, by = c(ID = "new_code"))
  levels(BAU) <- rat
  BAU_plot <- levelplot(BAU,
                        col.regions=rev(c('#006600','#339933','#339966', #con
                                          '#009933','#00cc00','#00cc66', #mix
                                          '#33cc33','#00ff00','#66ff66', #decid
                                          '#6600ff','#0000ff',
                                          '#595959','#000000', 
                                          '#ffff00')),
                        scales=list(draw=FALSE),
                        maxpixels = mxp,
                        colorkey=FALSE,
                        cex = 0.1,
                        main = "2010")
  BAU_plot <- as.ggplot(BAU_plot)
  
  ALT <- ratify(stack_to_plot$Alternative)
  rat <- levels(BAU)[[1]] %>% 
    left_join(classes_unique, by = c(ID = "new_code"))
  levels(ALT) <- rat
  ALT_plot <- levelplot(ALT,
                        col.regions=rev(c('#006600','#339933','#339966', #con
                                          '#009933','#00cc00','#00cc66', #mix
                                          '#33cc33','#00ff00','#66ff66', #decid
                                          '#6600ff','#0000ff',
                                          '#595959','#000000', 
                                          '#ffff00')),
                        scales=list(draw=FALSE),
                        maxpixels = mxp,
                        colorkey=list(space="right"),
                        labels = list(cex = 5),
                        main = "2100")
  ALT_plot <- as.ggplot(ALT_plot)
  
  final_plot <- BAU_plot + ALT_plot  + plot_layout(widths = c(1, 1.08))
  
  return(final_plot)
}
