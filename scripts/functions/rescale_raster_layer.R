# Rescale the raster layer 
# Reviewed 2020 - No Change

rescale_raster_layer <- function(raster.layer) {
  min.value <- cellStats(raster.layer, min)
  if (min.value < 0) {
    raster.layer <- raster.layer + (0 - min.value)
    max.value <- cellStats(raster.layer, max)
    raster.layer <- raster.layer/max.value
    return(raster.layer)
  } else {
    max.value <- cellStats(raster.layer, max)
    raster.layer <- raster.layer/max.value
    return(raster.layer)
  }
}

#rescale_raster_layer_CS <- function(raster.layer, min.value, max.value) {
#  if (min.value < 0) {
#    raster.layer <- raster.layer + (0 - min.value)
#    raster.layer <- raster.layer/max.value
#    return(raster.layer)
#  } else {
#    raster.layer <- raster.layer/max.value
#    return(raster.layer)
#  }
#}
