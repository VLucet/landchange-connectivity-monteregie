library(tidyverse)

final_df <- readRDS("outputs/final_df_current_density.RDS")

print("HERE")

# FULL SUM FOR FINAL OUTPUTS
full_stack=list()
count <- 1
for (sce in 1:length(assembled_list)) { 
  temp_list <- assembled_list[[sce]]
  for (ts in 1:length(temp)) {
    the_stack <-  (stack(lapply(temp, `[[`, ts)))
    the_sum <- sum(the_stack)
    full_stack[[count]] <- the_sum
    count <- count+1
  }
}  
print(head(names(full_stack)))

for(raster in names(full_stack)){
  writeRaster(full_stack[[raster]],
              file.path("outputs", "current_density_sum", 
                        paste0("full_sum_", raster, ".tif")), 
              overwrite=T)
}

print("THERE")