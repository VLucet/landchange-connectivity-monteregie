#-------------------------------------------------------------------------------
## 6.2 Run Circuitscape 
## 2020
## Inputs: INI files, resistance rasters
## Outputs: connectivity rasters
#-------------------------------------------------------------------------------

@everywhere using Pkg
@everywhere Pkg.activate("."); Pkg.instantiate()

@everywhere using Distributed
@everywhere using LinearAlgebra.BLAS

# Set BLAS threads to 1 to avoid oversubscription
@everywhere BLAS.set_num_threads(1)

@everywhere using Circuitscape

# Read the ini files
@everywhere searchdir(path,key) = filter(x->occursin(key,x), readdir(path))
@everywhere dir = "config/ini_circuitscape/all/"
@everywhere ext = "ini"
@everywhere ini_list = dir .* searchdir(dir, ext)

pmap(compute, ini_list)
