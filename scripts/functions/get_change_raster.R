# Obtain the appropriate change from land use raster
# Reviewed 2020 - no change

get_change_raster <- function(raster_1, raster_2, class, from=NA, only_from=FALSE){
  
  # if no from is suplied
  if (sum(is.na(from))>0){
    new <- (((raster_2 == class) - (raster_1 == class)) == 1)
    changed <- (((raster_2 == class) - (raster_1 == class)) == -1)
    out <- stack(new, changed)
    
    names(out) <- c("new", "changed")
    
  # if a from is suplied
  } else if (sum(is.na(from))==0){
    changed <- ((raster_2 == class) & (raster_1 %in% from) == 1)
    out <- changed
    
    names_out <- paste0("from_", paste(from, collapse="&"),"_to_", class)
    
    print(names_out)
    if (only_from==TRUE){
      out[!(raster_1 %in% from)] <- NA
    }
    names(out) <- "new"
  }
  out
}
