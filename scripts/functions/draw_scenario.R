## Useful function for drawing side by side maps

draw_scenario <- function(sce = NULL, ts_start = 2, ts_end = 11, 
                          sce_folder = "test/july", iter=5, 
                          plot_raw=TRUE, return_df=FALSE){
  
  stratum <- raster("data/stsim/aggregated/primary_stratum_mont_or_not_or_PA.tif")
  
  sce_dirs <- unlist(lapply(list.dirs(sce_folder), FUN = grep, pattern="sce", value=T))
  sce_index <- grep(x=sce_dirs, pattern = as.character(sce))
  
  if (length(sce_index) == 1) {
    sce_stack <- stack(lapply((list.files(sce_dirs[sce_index], full.names = T)), raster))
  } else {
    stop("Wrong length")
  }
  
  the_names <- names(sce_stack)
  sce_stack[stratum == 0] <- NA
  names(sce_stack) <- the_names
  
  if (plot_raw & !return_df){
    
    two_stack <-  stack(sce_stack[[paste0("sc.it",iter,".ts2")]],
                        sce_stack[[paste0("sc.it",iter,".ts11")]])
    
    two_stack[stratum == 0] <- NA
    
    names(two_stack) <- c("2010", "2100")
    
    plot(trim(two_stack))
  
    } else if (return_df){
    
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
      mutate(scenario = sce)
    
    return(final_df)
  }
  
}
