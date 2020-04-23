#-------------------------------------------------------------------------------
## 6.2 Run Circuitscape 
## 2020
## Inputs: INI files, resistance rasters
## Outputs: connectivity rasters
#-------------------------------------------------------------------------------

@everywhere using Pkg
@everywhere using Circuitscape

Pkg.build("Circuitscape")

# Read the ini files
@everywhere searchdir(path,key) = filter(x->occursin(key,x), readdir(path))
@everywhere ini_list = searchdir("config/ini_circuitscape/all/", "ini")

@everywhere function comp(file)
    println(file)
    compute(string("config/ini_circuitscape/all/",file))
    println("Done")
end

@time pmap(comp, ini_list)
