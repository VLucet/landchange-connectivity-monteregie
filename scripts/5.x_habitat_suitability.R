#-------------------------------------------------------------------------------
## 5.x
## 2020
## Inputs: 
## Outputs: 
#-------------------------------------------------------------------------------
rm(list=ls())
# Set parameters 
STSIM_ITER <- as.numeric(Sys.getenv("STSIM_ITER", unset = 2))
R_AGGR_FACT <- as.numeric(Sys.getenv("R_AGGR_FACT", unset = 3))
STSIM_STEP_SAVE <- as.numeric(Sys.getenv("STSIM_STEP_SAVE", unset = 1))
#-------------------------------------------------------------------------------

# Load important libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(raster)
  library(rgrass7)
  library(rgrassdoc)
  library(assertthat)
})

# Reset Raster tmp files
removeTmpFiles(0)
showTmpFiles()

#-------------------------------------------------------------------------------

## Load inputs
# Get result secenario directory 
sce_dir_vec <- list.files("libraries/stsim/monteregie-conncons-scripted.ssim.output", 
                          full.names = T)
sce_nb_vec <- as.numeric(unlist(lapply(str_split(sce_dir_vec, "-"), FUN = last)))

# Template
iter_template <- paste0("it", 1:STSIM_ITER)

## Load classification matrices
species_list <- tools::file_path_sans_ext(list.files("config/rcl_tables/species/"))
patch_size <- read_csv("config/rcl_tables/grass/patch_size.csv")
non_habitat <- read_csv("config/rcl_tables/grass/non_habitat.csv")
too_small <- read_csv("config/rcl_tables/grass/too_small.csv")

#-------------------------------------------------------------------------------

true_landuse_list <- list.files("data/land_use/aggregated/", full.names=T, 
                                pattern = "patched")
assert_that(length(true_landuse_list)==3)

#-------------------------------------------------------------------------------

plot_temp <- function(name){
  execGRASS("r.out.gdal", input = name, output="temp.tif", 
            format='GTiff',createopt='COMPRESS=LZW', flags=c('overwrite'))
  return(plot(raster("temp.tif")))
}

unlink("libraries/grass/habsuit/", recursive = T)
initGRASS(gisBase = "/usr/lib/grass76/", gisDbase = "libraries/grass/", 
          location = "habsuit", mapset = "PERMANENT", 
          override = TRUE)
execGRASS("g.proj", flags = c("c"), proj4 = projection(raster(true_landuse_list[1])))
execGRASS("g.mapset", mapset = "habsuit_1", flags = c("c", "overwrite"))

for (true_lu in true_landuse_list){
  
  base_name <- tools::file_path_sans_ext(basename(true_lu))
  
  # Input **
  execGRASS("r.in.gdal", 
            input = true_lu, 
            output = base_name, 
            flags = c("overwrite"))
  
  execGRASS("g.region", raster = base_name)
  
  # Get only forest **
  write_lines(c("0 thru 10 = NULL", "* = *"), "config/rcl_tables/grass/rule.txt")
  forest_name <- paste0(base_name,"_for")
  execGRASS("r.reclass", 
            input = base_name, 
            rules = "config/rcl_tables/grass/rule.txt",
            output = forest_name, 
            flags = c("overwrite"))
  
  # clump it **
  forest_clumped_name <- paste0(forest_name,"_c")
  execGRASS("r.clump", 
            input = forest_name, 
            output = forest_clumped_name,
            flags = c("overwrite", "d"))                                 
  
  # Get the no forest **
  write_lines(c("10 thru 100 = NULL", "* = *"), "config/rcl_tables/grass/rule.txt")
  no_forest_name <- paste0(base_name,"_no_f")
  execGRASS("r.reclass", 
            input = base_name, 
            output = no_forest_name, 
            rules = "config/rcl_tables/grass/rule.txt",
            flags = "overwrite")

  #for (specie in species_list){}
  for (specie in species_list){
    
    # RECLASS NON-FOREST **
    no_forest_reclassed_name <- paste0(specie,"_", no_forest_name,"_r")
    execGRASS("r.reclass", 
              input = no_forest_name, 
              output = no_forest_reclassed_name, 
              rules = paste0("config/rcl_tables/species/",specie,".txt"), 
              flags = "overwrite")
    
    ## Reclass all forest based on prefered versus non prefered pixels
    binary_forest_name <- paste0(specie, "_", forest_name, "_b") 
    execGRASS("r.reclass", 
              input = forest_name, 
              output = binary_forest_name, 
              rules = paste0("config/rcl_tables/species/",specie,".txt"), 
              flags = "overwrite")
    
    # r.stats.zonal per patch for patch suitability
    stat_zonal_name <- paste0(binary_forest_name,"_s")
    execGRASS("r.stats.zonal", 
              base = forest_clumped_name,
              cover = binary_forest_name, 
              method = "average",
              output = stat_zonal_name,
              flags = "overwrite")
    
    # separate unsuitable from suitable patches 
    habitat_suit <- paste0(stat_zonal_name, "_su")
    execGRASS("r.mapcalc", 
              expression=paste0(habitat_suit," = ",stat_zonal_name, " >= 0.5"), 
              flags = "overwrite")
    execGRASS("r.null", map = habitat_suit, setnull="0")
    habitat_unsuit <- paste0(stat_zonal_name, "_un")
    execGRASS("r.mapcalc", 
              expression=paste0(habitat_unsuit, " = ",stat_zonal_name, " < 0.5"), 
              flags = "overwrite")
    execGRASS("r.null", map = habitat_unsuit, setnull="0")
    
    # Reclass all unsuit based on non habitat rule
    write_lines(paste0("* = ", as.character(subset(non_habitat, species==specie)$value)), 
                "config/rcl_tables/grass/rule.txt")
    habitat_unsuit_reclassed <- paste0(habitat_unsuit, "_r")
    execGRASS("r.reclass", 
              input = habitat_unsuit,
              output = habitat_unsuit_reclassed,
              rules = "config/rcl_tables/grass/rule.txt")
    
    # GREATER
    greater_area_name <- paste0(habitat_suit, "_ra_g")
    execGRASS("r.reclass.area",
              input = habitat_suit,
              output = greater_area_name,
              value =  subset(patch_size, species == specie)$value,
              mode = "greater",
              flags = c("overwrite", "d")) # diagonal neighbors
    greater_area_reclassed_name <-  paste0(greater_area_name, "_r")
    write_lines(paste0("* = 1"), "config/rcl_tables/grass/rule.txt")
    execGRASS("r.reclass",
              input = greater_area_name,
              output = greater_area_reclassed_name,
              rules = "config/rcl_tables/grass/rule.txt",
              flags = "overwrite")
    #execGRASS("g.remove", type = "raster", name = greater_area_name)

    # SMALLER
    lesser_area_name <- paste0(habitat_suit, "_ra_l")
    execGRASS("r.reclass.area",
              input = habitat_suit,
              output = lesser_area_name,
              value =  subset(patch_size, species == specie)$value,
              mode = "lesser",
              flags = c("overwrite", "d")) # diagonal neighbors
    lesser_area_reclassed_name <- paste0(lesser_area_name, "_r")
    write_lines(paste0("* = ", as.character(subset(too_small, species==specie)$value)), 
                "config/rcl_tables/grass/rule.txt")
    execGRASS("r.reclass",
              input = lesser_area_name,
              output = lesser_area_reclassed_name,
              rules = "config/rcl_tables/grass/rule.txt",
              flags = "overwrite")
    #execGRASS("g.remove", type = "raster", name = lesser_area_name)
    
    # PATCH WITH RECLASSED NO FOREST
    final_name <- paste(specie, base_name, "final", sep = "_")
    execGRASS("r.patch", 
              input = c(no_forest_reclassed_name, 
                        habitat_unsuit_reclassed,
                        lesser_area_reclassed_name,
                        greater_area_reclassed_name
                        ), 
              output = final_name, 
              flags = "overwrite")
    ts <- which(grepl(true_landuse_list, pattern = base_name))-1
    execGRASS("r.out.gdal", 
              input = final_name, 
              format='GTiff',createopt='COMPRESS=LZW', 
              output = paste0("outputs/reclassed/TRUE_lu_",ts,"_", specie, ".tif"),
              flags=c('overwrite'))
  }
}

test <- stack(map(list.files("outputs/reclassed/", full.names = T), raster))
plot(test$TRUE_lu_2_MAAM==1)
