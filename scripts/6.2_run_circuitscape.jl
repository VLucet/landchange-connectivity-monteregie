#-------------------------------------------------------------------------------
## 6.2 Run Circuitscape 
## 2020
## Inputs: INI files, resistance rasters
## Outputs: connectivity rasters
## Pkg.add(Pkg.PackageSpec(name="Circuitscape", 
##         rev="93fc58fc38d2e1f7b17241e7d6aa2764dd56ff13"))
#-------------------------------------------------------------------------------

@everywhere using Pkg
@everywhere using Distributed
@everywhere using LinearAlgebra.BLAS

# Set BLAS threads to 1 to avoid oversubscription
@everywhere BLAS.set_num_threads(1)

Pkg.activate(".")
@everywhere using Circuitscape

# Read the ini files
@everywhere searchdir(path,key) = filter(x->occursin(key,x), readdir(path))
@everywhere dir = "config/ini_circuitscape/all/"
@everywhere ext = "ini"
@everywhere ini_list = dir .* searchdir(dir, ext)

pmap(compute, ini_list)
