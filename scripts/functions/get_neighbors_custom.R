# Obtain the neihbors
# Reviewed 2020 - no change

get_neighbors_custom <- function(rast, value, mat_weight){
  rast_mod <- (rast == value) 
  #plot(rast_mod)
  out <- focal(x = rast_mod, 
               w = mat_weight, 
               fun = function(x){sum(x, na.rm=T)/(length(x)-1)})
  out[is.nan(out)] <- NA
  return(out)
}

# my_mean <-  function(x, v, na.rm=T){
#   
#   y <- x==v
#   out <- mean(y, na.rm=t)
#   
# }

# test <-  raster::getValuesFocal(lu.stack.ag[[1]], ngb=c(5,5), names=T)
# test <- test[]
# test2 <- apply(test, function(x){mean(x==2, na.rm=T)}, MARGIN = 1)
# 
# g <- lu.stack.ag$lu_1990
# values(g) <- test2