#-------------------------------------------------------------------------------
## 6.2 Run Circuitscape 
## 2020
## Inputs: INI files, resistance rasters
## Outputs: connectivity rasters
#-------------------------------------------------------------------------------

using Pkg ; Pkg.activate(".") ; Pkg.instantiate()

using Distributed
using LinearAlgebra.BLAS

# Set BLAS threads to 1 to avoid oversubscription
BLAS.set_num_threads(1)

# Read the ini files
searchdir(path,key) = filter(x->occursin(key,x), readdir(path))
dir = "config/ini_circuitscape/all/"
ext = "ini"
ini_list = dir .* searchdir(dir, ext)

# Get cores
cores = 8

# Add cores with prokect flag
addprocs(cores, exeflags="--project")

# Still need to declare Circuitscape everywhere
@everywhere using Circuitscape

# META-PARALLELIZATION => Call to pmap, batch_size size in question

pmap(compute, ini_list, batch_size=3)

# -----------------------------------------------------------------------------
# Mask # if stopped and needs a mask
# mask = occursin.([r"((sce_)+(([4][4-9])|([5][0-9])))|(it_6_)"], ini_list)
# ini_list_masked = ini_list[mask]
# Split # If for loop
# group_size = Int64(round(length(ini_list_masked)/cores, RoundUp))
# -----------------------------------------------------------------------------
# Splitting # If need to split between cores
# ini_list_split = Iterators.partition(ini_list_masked, group_size) |> collect
# @everywhere ini_list_split = Iterators.partition(ini_list, 5) |> collect
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Custom functions if other approachaes
# @everywhere function compute_custom_loop(ini)
#   for file in ini
#     println(file)
#     compute(file)
#     println("****")
#   end
# end
# -----------------------------------------------------------------------------
# @everywhere function compute_custom(ini)
#   println(ini)
#   compute(ini)
#   println("****")
# end
# ----------------------------------------------------------------------------- 

# -----------------------------------------------------------------------------
# Look for diff in ini/curmap in case of crash 
#
# searchdir(path,key) = filter(x->occursin(key,x), readdir(path))
# list_outputs = searchdir("outputs/current_density", "tif")
# list_outputs_dump = searchdir("/home/ubuntu/data/val/current_density_dump/", "tif")
# full_list = vcat(list_outputs, list_outputs_dump)
# full_list = replace.(full_list, "_out_cum_curmap.tif" => "")
#
# list_ini_files = searchdir("config/ini_circuitscape/all/", "ini")
# list_ini_files = replace.(list_ini_files, ".ini" => "")
#
# intersect(full_list, list_ini_files)
# the_diff = setdiff(list_ini_files, full_list)
# using DelimitedFiles
# writedlm("config/ini_circuitscape/remaining_files.csv",  the_diff, ',')
#
# using DelimitedFiles
# the_diff = "config/ini_circuitscape/all/" .* String.(readdlm("config/ini_circuitscape/remaining_files.csv")) .* ".ini"
# -----------------------------------------------------------------------------

