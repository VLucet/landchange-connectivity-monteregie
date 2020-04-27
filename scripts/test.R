library(tidyverse)
library(raster)
assembled_list <- readRDS("outputs/final_raster_means.rds")

print("HERE")

# FULL SUM FOR FINAL OUTPUTS
full_stack=list()
count <- 1
for (sce in 1:length(assembled_list)) { 
  temp <- assembled_list[[sce]]
  print(sce)
  for (timestep in 1:length(temp)) {
    print(timestep)
    
    the_stack <- stack(lapply(temp, `[[`, timestep))
    the_sum <- sum(the_stack)
    
    full_stack[[count]] <- the_sum
    names(full_stack[[count]]) <- paste("sce",sce, "ts", timestep,"", sep = "_")
    names(full_stack[count]) <- paste("sce",sce, "ts", timestep,"", sep = "_")
    
    count <- count+1
  }
}  

for(raster in full_stack){
  file_name <- names(raster)
  writeRaster(raster,
              file.path("outputs", "current_density_sum", 
                        paste0("full_sum_", file_name, ".tif")), 
              overwrite=T)
}

print("THERE")