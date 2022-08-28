#-------------------------------------------------------------------------------
## 6.1 Prep Circuitscape inputs
## 2020
## Inputs: reclassed resistance (names)
## Outputs: INI Files
#-------------------------------------------------------------------------------
rm(list=ls())
set.seed(77)
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Load important libraries
suppressPackageStartupMessages({
  library(stringr)
  library(raster)
})

print("Preparing circuitscape inputs"); Sys.time()

## Read in 
reclassed_list <- readRDS("data/temp/reclassed_list.RDS")

# Make new files list 
buffered_list <-paste0(reclassed_list, "with_buffer")

# Make all the INI files
INI_template <- readLines("config/ini_circuitscape/template/ini_template.ini")

make_INI_file = function(file, orientation) {
  INI <- lapply(X=INI_template, FUN=str_replace_all, 
                pattern ="RESISTANCE_FILE_NON_ORIENTED",
                replacement = paste0("outputs/reclassed_with_buffer/",file,".tif"))
  
  INI <- lapply(X=INI, FUN=str_replace_all, 
                pattern ="POINT_FILE_ORIENTED",
                replacement = paste0("outputs/reclassed_with_buffer/circuitscape_buffer_", 
                                     orientation, ".tif"))
  INI <- lapply(X=INI, FUN=str_replace_all, 
                pattern ="OUTPUT_FILE",
                replacement = paste0("../current_density_2/",file, "_", orientation, 
                                     "_out")) # Np need for .tif here
  fileConn<-file(paste0("config/ini_circuitscape/all/", file, "_",orientation,".ini"))
  writeLines(unlist(INI), fileConn)
  close(fileConn)
}

saved_output_EW <- lapply(buffered_list, make_INI_file, orientation="EW")
saved_output_NS <- lapply(buffered_list, make_INI_file, orientation="NS")
