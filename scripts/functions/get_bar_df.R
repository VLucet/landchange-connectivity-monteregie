## Useful function for drawing side by side maps

get_bar_df <- function(ts_start = 2, ts_end = 11, 
                       sce_folder = "libraries/stsim/monteregie-conncons-scripted.ssim.output/", 
                       iter=5){
  
  # Classes
  classes <- read_csv("config/rcl_tables/land_use/recode_table.csv")
  forest_classes <- read_csv("config/rcl_tables/land_use/recode_table_forest.csv") %>%
    rename(new_code = "ID", new_class = "Name")
  classes_unique <- unique(classes[, c("new_code", "new_class")]) %>%
    bind_rows(forest_classes)
  
  stratum <- raster("data/stsim/aggregated/primary_stratum_mont_or_not_or_PA.tif")
  sce_dirs <- unlist(lapply(list.dirs(sce_folder, recursive=F), 
                            FUN = grep, pattern="Scenario", value=T))
  sce_dirs <- paste0(sce_dirs, "/stsim_OutputSpatialState/")
  
  all_sce <- data.frame()
  
  for (sce in sce_dirs[2:length(sce_dirs)]) {
    
    the_list <- list.files(sce, full.names = T)
    
    # sub_list <-  
    #   the_list[str_detect(the_list, c(paste0("it", as.character(iter))))]
    sub_list_start <- 
      the_list[str_detect(the_list, c(paste0("ts", as.character(ts_start))))]
    sub_list_end <- 
      the_list[str_detect(the_list, c(paste0("ts", as.character(ts_end))))]
    
    sce_stack <- stack(lapply(c(sub_list_start, sub_list_end), raster))
    
    print(sce_stack)
    
    the_names <- names(sce_stack)
    sce_stack[stratum == 0] <- NA
    names(sce_stack) <- the_names
    
    start <- sce_stack[[which(stringr::str_detect(names(sce_stack), 
                                                  paste0("ts", as.character(ts_start))))]]
    end <- sce_stack[[which(stringr::str_detect(names(sce_stack), 
                                                paste0("ts", as.character(ts_end))))]]
    
    ts_2_freq <- freq(start, useNA="no")
    ts_11_freq <- freq(end, useNA="no")
    
    full_df_2 <- data.frame()
    for (x in 1:length(ts_2_freq)){
      temp_df <- ts_2_freq[[x]] %>% 
        as.data.frame() %>% 
        mutate(iteration = paste0("iter_", x)) %>% 
        mutate(timestep = 2010)
      full_df_2 <- bind_rows(full_df_2, temp_df)
    }
    
    full_df_11 <- data.frame()
    for (x in 1:length(ts_11_freq)){
      temp_df <- ts_11_freq[[x]] %>% 
        as.data.frame() %>% 
        mutate(iteration = paste0("iter_", x)) %>% 
        mutate(timestep = 2100)
      full_df_11 <- bind_rows(full_df_11, temp_df)
    }
    
    full_df <- bind_rows(full_df_2, full_df_11)
    
    mean_df <- full_df %>% 
      group_by(timestep, value) %>% 
      summarise(mean_count = mean(count)) %>% ungroup()
    
    sd_df <- full_df %>% 
      group_by(timestep, value) %>% 
      summarise(sd_count = sd(count)) %>% ungroup()
    
    final_df <- left_join(mean_df, sd_df,  by = c("timestep", "value")) %>% 
      left_join(classes_unique, 
                by=c("value"="new_code")) %>% 
      #filter(!(value %in% c(4,5,6))) %>%
      mutate(area = mean_count*((90*90)/10000)) %>% 
      mutate(scenario = as.numeric(str_split(str_split(sce, "-", simplify=T)[4], "/", simplify =T)[1]))
    
    all_sce <- bind_rows(all_sce, final_df)
    print("done")
  }
  return(all_sce)
}