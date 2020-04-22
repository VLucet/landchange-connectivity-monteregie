# Function to create data from the census 
# Reviewed 2020 - changed

get_census_var <- function(year_list, # census year
                           var_vec, # var vector
                           var_name, # vraiable name
                           datadir, # where to find the shapefile
                           raster){ # raster template
  print(var_vec)
  output <- stack()
  
  for (index in 1:length(var_vec)){
    year <- year_list[index]
    # Check if correct year
    if (!(year %in% c(1991, 2001, 2011))){
      stop("error: census years 1991, 2001, 2011 only")
    } else {
      # Load the correct census year
      if (year == 1991){
        census.sf <- sf::st_read(file.path(datadir,"CHASS_1991.shp"), 
                                 quiet=T)
        print("1991 Census loaded")
      } else if (year == 2001){
        census.sf <- sf::st_read(file.path(datadir,"CHASS_2001.shp"), 
                                 quiet=T)
        print("2001 Census loaded")
      } else if (year == 2011){
        census.sf <- sf::st_read(file.path(datadir,"CHASS_2011_short.shp"), 
                                 quiet=T)
        print("2011 Census loaded")
      }
    }
    rast <- fasterize::fasterize(census.sf, raster, 
                                 field=var_vec[index])
    print( names(rast))
    names(rast) <- paste0(var_name, "_", year)
    output <- stack(output, rast)
  }
  
  output
}
