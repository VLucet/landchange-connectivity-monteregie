#-------------------------------------------------------------------------------
## 5.1 Reclassify
## 2020
## Inputs: stsim outputs
## Outputs: reclassed rasters
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
  library(gtools)
})

# Reset Raster tmp files
removeTmpFiles(0)
showTmpFiles()

#-------------------------------------------------------------------------------

## Load inputs
# Get result secenario directory
sce_dir_vec <- list.files("libraries/stsim/monteregie-conncons-scripted.ssim.output",
                          full.names = T)

# Template
iter_template <- paste0("it", 1:STSIM_ITER)
# iter_template <- paste0("it", 1)

## Load classification matrices
species_list <- tools::file_path_sans_ext(list.files("config/rcl_tables/species/"))
patch_size <- read_csv("config/rcl_tables/grass/patch_size.csv")
non_habitat <- read_csv("config/rcl_tables/grass/non_habitat.csv")
too_small <- read_csv("config/rcl_tables/grass/too_small.csv")
# interm  <- read_csv("config/rcl_tables/grass/interm.csv")

## Patch zone
patch_raster_path  <- 'data/land_use/aggregated/aggregated_lu_buffered_1990_to_patch.tif'

#-------------------------------------------------------------------------------
if (R_AGGR_FACT){
  true_landuse_list <- list.files("data/land_use/aggregated/", full.names=T,
                                  pattern = "patched")
} else {
  stop()
}
assert_that(length(true_landuse_list)==3)

#-------------------------------------------------------------------------------

message("Reclassifying") ; Sys.time()
message("TRUE LAND USE")

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

  execGRASS("r.in.gdal",
            input = patch_raster_path,
            output = "patch_raster",
            flags = c("o","overwrite"))
  execGRASS("r.null", map = "patch_raster", null=0)

for (true_lu in true_landuse_list){

  base_name <- tools::file_path_sans_ext(basename(true_lu))
  # Shorten base_name
  base_name <- gsub("aggregated_lu_buffered_", "", base_name, fixed="TRUE")

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

  # Change value in Patch
  execGRASS("r.mapcalc",
              expression = paste0(forest_name, " = if((", forest_name, " == 22 && patch_raster==1), 12, ", forest_name, ")"),
              flags = "overwrite")

  # execGRASS("r.out.gdal",
  #             input = forest_name,
  #             format='GTiff',createopt='COMPRESS=LZW',
  #             output = paste0("outputs/reclassed/",forest_name,".tif"),
  #             flags=c('overwrite'))

  # Get only forest **
  # FOREST TYPES
  write_lines(c("0 thru 10 = NULL", "* = *"), "config/rcl_tables/grass/rule.txt")
  forest_name <- paste0(base_name,"_for")
  execGRASS("r.reclass",
            input = base_name,
            rules = "config/rcl_tables/grass/rule.txt",
            output = forest_name,
            flags = c("overwrite"))

  # ONLY FOREST
  write_lines(c("0 thru 10 = NULL", "* = 1"), "config/rcl_tables/grass/rule.txt")
  forest_only_name <- paste0(base_name,"_for_only")
  execGRASS("r.reclass",
            input = base_name,
            rules = "config/rcl_tables/grass/rule.txt",
            output = forest_only_name,
            flags = c("overwrite"))

  # # clump it **
  # forest_clumped_name <- paste0(forest_only_name,"_c")
  # execGRASS("r.clump",
  #           input = forest_only_name,
  #           output = forest_clumped_name,
  #           flags = c("overwrite"))

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
    ## not really binary here because nuances
    reclassed_forest_name <- paste0(specie, "_", forest_name, "_b")
    execGRASS("r.reclass",
              input = forest_name,
              output = reclassed_forest_name,
              rules = paste0("config/rcl_tables/species/",specie,".txt"),
              flags = "overwrite")

    # # Divide to put on 0-1 scale
    # binary_forest_name_scaled <- paste0(binary_forest_name, "_scl")
    # execGRASS("r.mapcalc",
    #           expression = paste0(binary_forest_name_scaled, " = ", binary_forest_name, " / 10.0"),
    #           flags = "overwrite")

    # ----------------------------------------------------------------------

    # Modulate habitat quality based on distance

    # Get only roads
    write_lines(c("4 = 1", "* = NULL"), "config/rcl_tables/grass/rule.txt")
    roads_name <- paste0(base_name,"_roads")
    execGRASS("r.reclass",
              input = base_name,
              rules = "config/rcl_tables/grass/rule.txt",
              output = roads_name,
              flags = c("overwrite"))

    # Grow distance
    roads_name_distance <- paste0(roads_name,"_dist")
    execGRASS("r.grow.distance", input = roads_name,
              distance = roads_name_distance,
              flags = c("overwrite"))

    # Reclass it
    roads_name_distance_reclassed <- paste0(specie, "_", roads_name_distance, "_rcl")
    execGRASS("r.reclass",
              input = roads_name_distance,
              output = roads_name_distance_reclassed,
              rules = paste0("config/rcl_tables/distance/roads/",specie,".txt"),
              flags = "overwrite")

    # Divide to put on 0-1 scale
    roads_name_distance_reclassed_scaled <- paste0(roads_name_distance_reclassed, "_scl")
    execGRASS("r.mapcalc",
              expression = paste0(roads_name_distance_reclassed_scaled, " = ", roads_name_distance_reclassed, " / 10.0"),
              flags = "overwrite")

    # execGRASS("r.out.gdal",
    #           input = roads_name_distance_reclassed_scaled,
    #           format='GTiff',createopt='COMPRESS=LZW',
    #           output = paste0("outputs/reclassed/", roads_name_distance_reclassed_scaled, ".tif"),
    #           flags=c('overwrite'))

    if (specie == "RASY"){

      # Get only Wetlands
      write_lines(c("6 = 1", "* = NULL"), "config/rcl_tables/grass/rule.txt")
      wetlands_name <- paste0(base_name,"_wet")
      execGRASS("r.reclass",
                input = base_name,
                rules = "config/rcl_tables/grass/rule.txt",
                output = wetlands_name,
                flags = c("overwrite"))

      # Grow distance
      wetlands_name_distance <- paste0(wetlands_name,"_dist")
      execGRASS("r.grow.distance", input = wetlands_name,
                distance = wetlands_name_distance,
                flags = c("overwrite"))

      # Reclass it
      wetlands_name_distance_reclassed <- paste0(specie, "_", wetlands_name_distance, "_rcl")
      execGRASS("r.reclass",
                input = wetlands_name_distance,
                output = wetlands_name_distance_reclassed,
                rules = paste0("config/rcl_tables/distance/wetlands/",specie,".txt"),
                flags = "overwrite")

      # Divide to put on 0-1 scale
      wetlands_name_distance_reclassed_scaled <- paste0(wetlands_name_distance_reclassed, "_scl")
      execGRASS("r.mapcalc",
                expression = paste0(wetlands_name_distance_reclassed_scaled, " = ", wetlands_name_distance_reclassed, " / 10.0"),
                flags = "overwrite")

      # execGRASS("r.out.gdal",
      #           input = wetlands_name_distance_reclassed_scaled,
      #           format='GTiff',createopt='COMPRESS=LZW',
      #           output = paste0("outputs/reclassed/", wetlands_name_distance_reclassed_scaled, ".tif"),
      #           flags=c('overwrite'))

    } else {
      wetlands_name_distance_reclassed_scaled <- ""
    }

    if (specie == "URAM"){

      # Get only urban
      write_lines(c("2 = 1", "* = NULL"), "config/rcl_tables/grass/rule.txt")
      urban_name <- paste0(base_name,"_urb")
      execGRASS("r.reclass",
                input = base_name,
                rules = "config/rcl_tables/grass/rule.txt",
                output = urban_name,
                flags = c("overwrite"))

      # Grow distance
      urban_name_distance <- paste0(urban_name,"_dist")
      execGRASS("r.grow.distance", input = urban_name,
                distance = urban_name_distance,
                flags = c("overwrite"))

      # Reclass it
      urban_name_distance_reclassed <- paste0(specie, "_", urban_name_distance, "_rcl")
      execGRASS("r.reclass",
                input = urban_name_distance,
                output = urban_name_distance_reclassed,
                rules = paste0("config/rcl_tables/distance/urban/",specie,".txt"),
                flags = "overwrite")

      # Divide to put on 0-1 scale
      urban_name_distance_reclassed_scaled <- paste0(urban_name_distance_reclassed, "_scl")
      execGRASS("r.mapcalc",
                expression = paste0(urban_name_distance_reclassed_scaled, " = ", urban_name_distance_reclassed, " / 10.0"),
                flags = "overwrite")

      # execGRASS("r.out.gdal",
      #           input = urban_name_distance_reclassed_scaled,
      #           format='GTiff',createopt='COMPRESS=LZW',
      #           output = paste0("outputs/reclassed/", urban_name_distance_reclassed_scaled, ".tif"),
      #           flags=c('overwrite'))

    } else {
      urban_name_distance_reclassed_scaled <- ""
    }

    # Get all multipliers
    multipliers <- c(roads_name_distance_reclassed_scaled,
                     wetlands_name_distance_reclassed_scaled,
                     urban_name_distance_reclassed_scaled)

    # Only keep non-empty ones
    multipliers <- multipliers[!(multipliers == "")]

    # Build the expression
    multiplied_forest_name <- paste0(reclassed_forest_name, "_multiplied")
    multiplier_expression <-
      paste0(multiplied_forest_name, " = ",
             paste0(c(reclassed_forest_name, multipliers), collapse = " * "))

    # Perform multiplication
    execGRASS("r.mapcalc",
              expression = multiplier_expression,
              flags = "overwrite")

    # execGRASS("r.out.gdal",
    #           input = multiplied_forest_name,
    #           format='GTiff',createopt='COMPRESS=LZW',
    #           output = paste0("outputs/reclassed/", multiplied_forest_name, ".tif"),
    #           flags=c('overwrite'))

    # ----------------------------------------------------------------------

    # # r.stats.zonal per patch for patch suitability
    # stat_zonal_name <- paste0(multiplied_forest_name,"_s")
    # execGRASS("r.stats.zonal",
    #           base = forest_clumped_name,
    #           cover = multiplied_forest_name,
    #           method = "average",
    #           output = stat_zonal_name,
    #           flags = "overwrite")

    # separate unsuitable from suitable patches
    habitat_suit <- paste0(multiplied_forest_name, "_su")
    execGRASS("r.mapcalc",
              expression=paste0(habitat_suit," = ",multiplied_forest_name, " >= 60"),
              flags = "overwrite")
    execGRASS("r.null", map = habitat_suit, setnull="0")

    # habitat_suit_interm <- paste0(stat_zonal_name, "_su_interm")
    # execGRASS("r.mapcalc",
    #           expression=paste0(habitat_suit_interm," = (", stat_zonal_name, " >= 0.25 && ", stat_zonal_name, "< 0.5)"),
    #           flags = "overwrite")
    # execGRASS("r.null", map = habitat_suit_interm, setnull="0")

    habitat_unsuit <- paste0(multiplied_forest_name, "_un")
        execGRASS("r.mapcalc",
                  expression=paste0(habitat_unsuit, " = ",multiplied_forest_name, " < 60"),
                  flags = "overwrite")
        execGRASS("r.null", map = habitat_unsuit, setnull="0")

    # Reclass all interm habitat
    # write_lines(paste0("* = ", as.character(subset(interm, species==specie)$value)),
    #             "config/rcl_tables/grass/rule.txt")
    # habitat_suit_interm_reclassed <- paste0(habitat_suit_interm, "_r")
    # execGRASS("r.reclass",
    #           input = habitat_suit_interm,
    #           output = habitat_suit_interm_reclassed,
    #           rules = "config/rcl_tables/grass/rule.txt")

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
              flags = c("overwrite")) # diagonal neighbors
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
              flags = c("overwrite")) # diagonal neighbors
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
                        # habitat_suit_interm_reclassed,
                        lesser_area_reclassed_name,
                        greater_area_reclassed_name
              ),
              output = final_name,
              flags = "overwrite")

    ts <- which(grepl(true_landuse_list, pattern = base_name))-1

    if (specie == "RASY"){
      # Change value in Patch
        execGRASS("r.mapcalc",
                  expression = paste0(final_name, " = if((", final_name, " == 1 && patch_raster==1), 4, ", final_name, ")"),
                  flags = "overwrite")
    }
    
    if(specie == "BLBR"){
      # Change value in Patch
        execGRASS("r.mapcalc",
                  expression = paste0(final_name, " = if((", final_name, " == 1 && patch_raster==1), 4, ", final_name, ")"),
                  flags = "overwrite")
    }

    execGRASS("r.out.gdal",
              input = final_name,
              format='GTiff',createopt='COMPRESS=LZW',
              output = paste0("outputs/reclassed/TRUE_lu_",ts,"_", specie, ".tif"),
              flags=c('overwrite'))
  }
}


# test <- stack(map(list.files("outputs/reclassed/", full.names = T), raster))
# plot(test$TRUE_lu_2_MAAM==1)

#-------------------------------------------------------------------------------

message("MODELLED LAND USE")
unlink("libraries/grass/habsuit/", recursive = T)
initGRASS(gisBase = "/usr/lib/grass76/", gisDbase = "libraries/grass/",
          location = "habsuit", mapset = "PERMANENT",
          override = TRUE)
execGRASS("g.proj", flags = c("c"), proj4 = projection(raster(true_landuse_list[1])))
execGRASS("g.mapset", mapset = "habsuit_2", flags = c("c", "overwrite"))

  execGRASS("r.in.gdal",
            input = patch_raster_path,
            output = "patch_raster",
            flags = c("o","overwrite"))
  execGRASS("r.null", map = "patch_raster", null=0)

for (sce in sce_dir_vec[-c(2,5,8,11,14)]){ # *4*,7,10,13,16 ## -c(2,5,8,11,14)

  sce_nb <- as.numeric(unlist(lapply(str_split(sce, "-"), FUN = last)))

  # Get file list and iteration memberships
  files_iter_list <-
    mixedsort(list.files(paste0(sce, "/stsim_OutputSpatialState"),
                         full.names = T))

  # Determibe ts template for this iteration
  ts_vec <- unique(as.numeric(unlist( # as numeric cause the whole thing
    lapply(                           # returns a string
      str_split(                      # also get the unique values
        unlist(
          lapply(
            unlist(
              lapply(files_iter_list,str_split,"\\."), # split after "it1.ts"
              recursive = F), # unlist, but only one level
            nth, n=5)), # get number 5 (where the ts nb is)
        "s"), nth, # split after s
      n = 2)))) # get number 2

  iter_membreships <- lapply(X=paste0(iter_template, "\\."), FUN=grep, x=files_iter_list)
  split_by_iter <- lapply(seq_along(iter_membreships),
                          function(x){files_iter_list[iter_membreships[[x]]]})
  names(split_by_iter) <- iter_template

  ts_template <- unique(paste0("ts_", seq(min(ts_vec), #from min for this sce
                                          max(ts_vec), # to max
                                          STSIM_STEP_SAVE))) # by whatever we save

  for (it in split_by_iter){
    print(it)

    # if(length(it) == 3){
    #   # DO NOTHING
    # }else{
    #   it <- it[c(1,10)]
    # }

    for(ts in unlist(it)){ # c(1, 10)
      print(ts)

      base_name <- paste("s",sce_nb, tools::file_path_sans_ext(basename(ts)), sep = "_")

      # Input **
      execGRASS("r.in.gdal",
                input = ts,
                output = base_name,
                flags = c("overwrite"))

      execGRASS("g.region", raster = base_name)

      # Get only forest **
      # FOREST TYPES
      write_lines(c("0 thru 10 = NULL", "* = *"), "config/rcl_tables/grass/rule.txt")
      forest_name <- paste0(base_name,"_for")
      execGRASS("r.reclass",
                input = base_name,
                rules = "config/rcl_tables/grass/rule.txt",
                output = forest_name,
                flags = c("overwrite"))

      # Change value in Patch
      execGRASS("r.mapcalc",
                expression = paste0(forest_name, " = if((", forest_name, " == 22 && patch_raster==1), 12, ", forest_name, ")"),
                flags = "overwrite")

      # ONLY FOREST
      write_lines(c("0 thru 10 = NULL", "* = 1"), "config/rcl_tables/grass/rule.txt")
      forest_only_name <- paste0(base_name,"_for_only")
      execGRASS("r.reclass",
                input = base_name,
                rules = "config/rcl_tables/grass/rule.txt",
                output = forest_only_name,
                flags = c("overwrite"))

      # # clump it **
      # forest_clumped_name <- paste0(forest_only_name,"_c")
      # execGRASS("r.clump",
      #           input = forest_only_name,
      #           output = forest_clumped_name,
      #           flags = c("overwrite"))

      #  execGRASS("r.out.gdal",
      #            input = forest_clumped_name,
      #            format='GTiff',createopt='COMPRESS=LZW',
      #            output = paste0("outputs/reclassed/", forest_clumped_name, ".tif"),
      #            flags=c('overwrite'))

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
        
        print(specie)
        
        # RECLASS NON-FOREST **
        no_forest_reclassed_name <- paste0(specie,"_", no_forest_name,"_r")
        execGRASS("r.reclass",
                  input = no_forest_name,
                  output = no_forest_reclassed_name,
                  rules = paste0("config/rcl_tables/species/",specie,".txt"),
                  flags = "overwrite")

        ## Reclass all forest based on prefered versus non prefered pixels
        reclassed_forest_name <- paste0(specie, "_", forest_name, "_b")
        execGRASS("r.reclass",
                  input = forest_name,
                  output = reclassed_forest_name,
                  rules = paste0("config/rcl_tables/species/",specie,".txt"),
                  flags = "overwrite")

        # Divide to put on 0-1 scale
        # reclassed_forest_name_scaled <- paste0(reclassed_forest_name, "_scl")
        # execGRASS("r.mapcalc",
        #           expression = paste0(reclassed_forest_name_scaled, " = ", reclassed_forest_name, " / 10.0"),
        #           flags = "overwrite")

        # execGRASS("r.out.gdal",
        #           input = binary_forest_name_scaled,
        #           format='GTiff',createopt='COMPRESS=LZW',
        #           output = paste0("outputs/reclassed/", binary_forest_name_scaled, ".tif"),
        #           flags=c('overwrite'))

        # ----------------------------------------------------------------------

        # Modulate habitat quality based on distance

        # Get only roads
        write_lines(c("4 = 1", "* = NULL"), "config/rcl_tables/grass/rule.txt")
        roads_name <- paste0(base_name,"_roads")
        execGRASS("r.reclass",
                  input = base_name,
                  rules = "config/rcl_tables/grass/rule.txt",
                  output = roads_name,
                  flags = c("overwrite"))

        # Grow distance
        roads_name_distance <- paste0(roads_name,"_dist")
        execGRASS("r.grow.distance", input = roads_name,
                  distance = roads_name_distance,
                  flags = c("overwrite"))

        # Reclass it
        roads_name_distance_reclassed <- paste0(specie, "_", roads_name_distance, "_rcl")
        execGRASS("r.reclass",
                  input = roads_name_distance,
                  output = roads_name_distance_reclassed,
                  rules = paste0("config/rcl_tables/distance/roads/",specie,".txt"),
                  flags = "overwrite")

        # Divide to put on 0-1 scale
        roads_name_distance_reclassed_scaled <- paste0(roads_name_distance_reclassed, "_scl")
        execGRASS("r.mapcalc",
                  expression = paste0(roads_name_distance_reclassed_scaled, " = ", roads_name_distance_reclassed, " / 10.0"),
                  flags = "overwrite")

        #  execGRASS("r.out.gdal",
        #            input = roads_name_distance_reclassed_scaled,
        #            format='GTiff',createopt='COMPRESS=LZW',
        #            output = paste0("outputs/reclassed/", roads_name_distance_reclassed_scaled, ".tif"),
        #            flags=c('overwrite'))

        if (specie == "RASY"){

          # Get only Wetlands
          write_lines(c("6 = 1", "* = NULL"), "config/rcl_tables/grass/rule.txt")
          wetlands_name <- paste0(base_name,"_wet")
          execGRASS("r.reclass",
                    input = base_name,
                    rules = "config/rcl_tables/grass/rule.txt",
                    output = wetlands_name,
                    flags = c("overwrite"))

          # Grow distance
          wetlands_name_distance <- paste0(wetlands_name,"_dist")
          execGRASS("r.grow.distance", input = wetlands_name,
                    distance = wetlands_name_distance,
                    flags = c("overwrite"))

          # Reclass it
          wetlands_name_distance_reclassed <- paste0(specie, "_", wetlands_name_distance, "_rcl")
          execGRASS("r.reclass",
                    input = wetlands_name_distance,
                    output = wetlands_name_distance_reclassed,
                    rules = paste0("config/rcl_tables/distance/wetlands/",specie,".txt"),
                    flags = "overwrite")

          # Divide to put on 0-1 scale
          wetlands_name_distance_reclassed_scaled <- paste0(wetlands_name_distance_reclassed, "_scl")
          execGRASS("r.mapcalc",
                    expression = paste0(wetlands_name_distance_reclassed_scaled, " = ", wetlands_name_distance_reclassed, " / 10.0"),
                    flags = "overwrite")

          #  execGRASS("r.out.gdal",
          #            input = wetlands_name_distance_reclassed_scaled,
          #            format='GTiff',createopt='COMPRESS=LZW',
          #            output = paste0("outputs/reclassed/", wetlands_name_distance_reclassed_scaled, ".tif"),
          #            flags=c('overwrite'))

        } else {
          wetlands_name_distance_reclassed_scaled <- ""
        }

        if (specie == "URAM"){

          # Get only urban
          write_lines(c("2 = 1", "* = NULL"), "config/rcl_tables/grass/rule.txt")
          urban_name <- paste0(base_name,"_urb")
          execGRASS("r.reclass",
                    input = base_name,
                    rules = "config/rcl_tables/grass/rule.txt",
                    output = urban_name,
                    flags = c("overwrite"))

          # Grow distance
          urban_name_distance <- paste0(urban_name,"_dist")
          execGRASS("r.grow.distance", input = urban_name,
                    distance = urban_name_distance,
                    flags = c("overwrite"))

          # Reclass it
          urban_name_distance_reclassed <- paste0(specie, "_", urban_name_distance, "_rcl")
          execGRASS("r.reclass",
                    input = urban_name_distance,
                    output = urban_name_distance_reclassed,
                    rules = paste0("config/rcl_tables/distance/urban/",specie,".txt"),
                    flags = "overwrite")

          # Divide to put on 0-1 scale
          urban_name_distance_reclassed_scaled <- paste0(urban_name_distance_reclassed, "_scl")
          execGRASS("r.mapcalc",
                    expression = paste0(urban_name_distance_reclassed_scaled, " = ", urban_name_distance_reclassed, " / 10.0"),
                    flags = "overwrite")

          #  execGRASS("r.out.gdal",
          #            input = urban_name_distance_reclassed_scaled,
          #            format='GTiff',createopt='COMPRESS=LZW',
          #            output = paste0("outputs/reclassed/", urban_name_distance_reclassed_scaled, ".tif"),
          #            flags=c('overwrite'))

        } else {
          urban_name_distance_reclassed_scaled <- ""
        }

        # Get all multipliers
        multipliers <- c(roads_name_distance_reclassed_scaled,
                         wetlands_name_distance_reclassed_scaled,
                         urban_name_distance_reclassed_scaled)

        # Only keep non-empty ones
        multipliers <- multipliers[!(multipliers == "")]

        # Build the expression
        multiplied_forest_name <- paste0(reclassed_forest_name, "_multiplied")
        multiplier_expression <-
          paste0(multiplied_forest_name, " = ",
                 paste0(c(reclassed_forest_name, multipliers), collapse = " * "))

        # Perform multiplication
        execGRASS("r.mapcalc",
                  expression = multiplier_expression,
                  flags = "overwrite")

        #  execGRASS("r.out.gdal",
        #            input = multiplied_forest_name,
        #            format='GTiff',createopt='COMPRESS=LZW',
        #            output = paste0("outputs/reclassed/", multiplied_forest_name, ".tif"),
        #            flags=c('overwrite'))

        # ----------------------------------------------------------------------

        # r.stats.zonal per patch for patch suitability
        # stat_zonal_name <- paste0(multiplied_forest_name,"_s")
        # execGRASS("r.stats.zonal",
        #           base = forest_clumped_name,
        #           cover = multiplied_forest_name,
        #           method = "average",
        #           output = stat_zonal_name,
        #           flags = "overwrite")

        # execGRASS("r.out.gdal",
        #            input = stat_zonal_name,
        #            format='GTiff',createopt='COMPRESS=LZW',
        #            output = paste0("outputs/reclassed/", stat_zonal_name, ".tif"),
        #            flags=c('overwrite'))

        # separate unsuitable from suitable patches
        habitat_suit <- paste0(multiplied_forest_name, "_su")
        execGRASS("r.mapcalc",
                  expression=paste0(habitat_suit," = ", multiplied_forest_name, " >= 60"),
                  flags = "overwrite")
        execGRASS("r.null", map = habitat_suit, setnull="0")

        # execGRASS("r.out.gdal",
        #            input = habitat_suit,
        #            format='GTiff',createopt='COMPRESS=LZW',
        #            output = paste0("outputs/reclassed/", habitat_suit, ".tif"),
        #            flags=c('overwrite'))

        # habitat_suit_interm <- paste0(stat_zonal_name, "_su_interm")
        # execGRASS("r.mapcalc",
        #           expression=paste0(habitat_suit_interm," = (", stat_zonal_name, " >= 0.25 && ", stat_zonal_name, "< 0.5)"),
        #           flags = "overwrite")
        # execGRASS("r.null", map = habitat_suit_interm, setnull="0")

        # habitat_suit_all <- paste0(stat_zonal_name, "_su_all")
        # execGRASS("r.mapcalc",
        #           expression=paste0(habitat_suit_all," = (", stat_zonal_name, " * (", stat_zonal_name, ">= 0.25))"),
        #           flags = "overwrite")
        # execGRASS("r.null", map = habitat_suit_all, setnull="0")

        # habitat_suit_all_clumped  <- paste0(habitat_suit_all, "_c")
        # execGRASS("r.clump",
        #     input = habitat_suit_all,
        #     output = habitat_suit_all_clumped,
        #     flags = c("overwrite"))

        # execGRASS("r.out.gdal",
        #            input = habitat_suit_all,
        #            format='GTiff',createopt='COMPRESS=LZW',
        #            output = paste0("outputs/hab_suit/", habitat_suit_all, ".tif"),
        #            flags=c('overwrite'))

        # execGRASS("r.out.gdal",
        #            input = habitat_suit_interm,
        #            format='GTiff',createopt='COMPRESS=LZW',
        #            output = paste0("outputs/reclassed/", habitat_suit_interm, ".tif"),
        #            flags=c('overwrite'))

        habitat_unsuit <- paste0(multiplied_forest_name, "_un")
        execGRASS("r.mapcalc",
                  expression=paste0(habitat_unsuit, " = ",multiplied_forest_name, " < 60"),
                  flags = "overwrite")
        execGRASS("r.null", map = habitat_unsuit, setnull="0")

        # execGRASS("r.out.gdal",
        #            input = habitat_unsuit,
        #            format='GTiff',createopt='COMPRESS=LZW',
        #            output = paste0("outputs/reclassed/", habitat_unsuit, ".tif"),
        #            flags=c('overwrite'))

        # Reclass all interm habitat
        # write_lines(paste0("* = ", as.character(subset(interm, species==specie)$value)),
        #             "config/rcl_tables/grass/rule.txt")
        # habitat_suit_interm_reclassed <- paste0(habitat_suit_interm, "_r")
        # execGRASS("r.reclass",
        #           input = habitat_suit_interm,
        #           output = habitat_suit_interm_reclassed,
        #           rules = "config/rcl_tables/grass/rule.txt")

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
                  flags = c("overwrite")) # diagonal neighbors
        
        # ----------------------------------------------------------------------
        # EXPORT suitable patches
        greater_area_name_ex <- paste0(greater_area_name, "_ex")
        execGRASS("r.mapcalc",
                  expression=paste0(greater_area_name_ex, " = ", greater_area_name, " * ", multiplied_forest_name),
                  flags = "overwrite")
        execGRASS("r.null", map = greater_area_name_ex, setnull="0")
        
        execGRASS("r.out.gdal",
                   input = greater_area_name_ex,
                   format='GTiff',createopt='COMPRESS=LZW',
                   output = paste0("outputs/hab_suit/", greater_area_name_ex, ".tif"),
                   flags=c('overwrite'))
        
        # ALSO RECLASS + CLUMP THEM
        write_lines("* = 1",
                    "config/rcl_tables/grass/rule.txt")
        greater_area_reclassed_name  <- paste0(greater_area_name_ex, "_r")
        execGRASS("r.reclass",
                  input = greater_area_name_ex,
                  output = greater_area_reclassed_name,
                  rules = "config/rcl_tables/grass/rule.txt")

        greater_area_clumped_name <- paste0(greater_area_reclassed_name,"_c")
        execGRASS("r.clump",
                  input = greater_area_reclassed_name,
                  output = greater_area_clumped_name,
                  flags = c("overwrite"))
        
         execGRASS("r.out.gdal",
                   input = greater_area_clumped_name,
                   format='GTiff',createopt='COMPRESS=LZW',
                   output = paste0("outputs/hab_suit/", greater_area_clumped_name, ".tif"),
                   flags=c('overwrite'))
        # ----------------------------------------------------------------------
        
        # reclass greater
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
                  flags = c("overwrite")) # diagonal neighbors
        
        # reclass smaller
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
                            # habitat_suit_interm_reclassed,
                            lesser_area_reclassed_name,
                            greater_area_reclassed_name
                  ),
                  output = final_name,
                  flags = "overwrite")

        # if (specie == "RASY"){
        #   # Change value in Patch
        #     execGRASS("r.mapcalc",
        #               expression = paste0(final_name, " = if((", final_name, " == 1 && patch_raster==1), 4, ", final_name, ")"),
        #               flags = "overwrite")
        # }
        
        # if(specie == "BLBR"){
        #   # Change value in Patch
        #     execGRASS("r.mapcalc",
        #               expression = paste0(final_name, " = if((", final_name, " == 1 && patch_raster==1), 4, ", final_name, ")"),
        #               flags = "overwrite")
        # }

        the_it <- unlist(str_split(unlist(str_split(base_name, pattern = "it*"))[2], pattern = "\\."))[1]
        the_ts <- unlist(str_split(base_name, pattern = "ts"))[2]

        execGRASS("r.out.gdal",
                  input = final_name,
                  format='GTiff',createopt='COMPRESS=LZW',
                  output = paste0("outputs/reclassed/sce_",sce_nb,
                                  "_it_", the_it,
                                  "_ts_", the_ts,
                                  "_", specie,"_.tif"),
                  flags=c('overwrite'))

        execGRASS("g.remove", pattern=paste0(specie, "*"), type = "all", flags = "f")
      }
      execGRASS("g.remove", pattern=paste0("ts", the_ts), type = "all", flags = "f")
    }
    execGRASS("g.remove", pattern=paste0("it", the_it), type = "all", flags = "f")
  }
  execGRASS("g.remove", pattern=paste0("sce", sce_nb), type = "all", flags = "f")
}

#-------------------------------------------------------------------------------

## Build script for adding rows
print("Building script") ; Sys.time()
reclassed_list <- tools::file_path_sans_ext(list.files("outputs/reclassed/"))

custom_paste <- function(file){
  paste0("sh ","scripts/0.1_add_buffer_lowlands_modified.sh ",
         "-i ", "outputs/reclassed/", file,".tif -b ",30*R_AGGR_FACT,
         " -o ", "outputs/reclassed_with_buffer/", file, "with_buffer ",
         "-e n")
}
first_line <- paste0("sh ","scripts/0.1_add_buffer_lowlands_modified.sh ",
                     "-i ", "outputs/reclassed/", reclassed_list[1],".tif -b ",30*R_AGGR_FACT,
                     " -o ", "outputs/reclassed_with_buffer/", "circuitscape_buffer ",
                     "-e y")
lines <- list(start="#!/bin/sh", first_line, lapply(reclassed_list, function(x) custom_paste(x)))

fileConn<-file("scripts/0.2_add_buffer_to_all.sh")
writeLines(unlist(lines), fileConn)
close(fileConn)

saveRDS(reclassed_list,"data/temp/reclassed_list.RDS")
