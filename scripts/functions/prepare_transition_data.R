# Prep data, calls get_change_raster
# Reviewed 2020 - no change

prepare_transition_data <- function(lu.stack, # stack of landuse data
                                    class_tr, # class of targeted change 
                                    from, # class of transition from
                                    only_from=T, # only with 'from' states
                                    aggregation # aggregated or not?
                                    ){
  
  # get the right change
  print(paste("Getting change rasters for class", class_tr)) ; Sys.time()
  chg.raster <- get_change_raster(raster_1 = lu.stack$lu_1990, 
                                      raster_2 = lu.stack$lu_2000, 
                                      class = class_tr, 
                                      from=from, only_from=only_from)
  
  # get change for validation set
  chg.raster.future <- get_change_raster(raster_1 = lu.stack$lu_2000, 
                                             raster_2 = lu.stack$lu_2010, 
                                             class = class_tr, 
                                             from=from, only_from=only_from)
  
  if (aggregation$ag){
    
    print(paste("Aggregating model rasters for class", class_tr)) ; Sys.time()
    
    # For model
    chg.raster.final <- aggregate(chg.raster, fun=modal_custom_highest,
                                      fact=aggregation$factor)
    chg.raster.future.final <- aggregate(chg.raster.future, fun=modal_custom_highest,
                                             fact=aggregation$factor)
    
  } else {
    
    # Nothing changes
    chg.raster.final <- chg.raster
    chg.raster.future.final <- chg.raster.future
  }
  
  out <- list(chg.raster.final=chg.raster.final, 
              chg.raster.future.final=chg.raster.future.final)
  
  names(out) <- paste0(names(out), ".class.", class_tr)
  
  out
}