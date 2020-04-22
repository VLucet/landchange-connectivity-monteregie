# Extract the vals and trans values of rasters
# Reviewed 2020 - no change

ExtractValsAndTrans = function(shape_vec, # Vector of shape matched by names
                               shape_sf, # Sf object
                               attribute = "MUS_NM_MUN",
                               landUse_stack, # Stack with names = years
                               classes = classes) {
  # INITIALIZE 
  #-----------
  # Set the timestep names
  steps = names(landUse_stack)
  
  # Set up empty vectors
  AllExtracted = c()
  AllTransitions = c() 
  
  # Set up vectors of all possible categories to be matched
  from = rep(classes$Code, length(classes$Code))
  to = rep(classes$Code, each=length(classes$Code))
  template = classes[c("Code","Label")] # not Flexible
  
  print(from)
  print(to)
  print(template)
  
  # Progress bas
  pb = txtProgressBar(min = 0, max = length(shape_vec), style=3);i=0
  
  # LOOP 
  #-----
  for (shape in shape_vec) {
    
    # Filter for each municipality
    tmp.shape = shape_sf %>%
      filter(eval(parse(text=attribute)) == shape)
    
    # Set variables and xtract values
    tmp.summurized = list()
    extracted = raster::extract(landUse_stack, tmp.shape)
    masked = stack(mask(crop(landUse_stack, tmp.shape),tmp.shape))
    
    
    # VALUES - not hardcoded, flexible part
    for (step in 1:length(steps)){
      column = c()
      for (polygon in 1:length(extracted)){
        column = c(column, c(extracted[[polygon]][,step]))
      }
      tmp.table = table(column)
      
      # Match to keep the same format 
      tmp.matches = match(template[,1], names(tmp.table))
      tmp.table = c(tmp.table)[tmp.matches]
      names(tmp.table) = as.character(template[,2])
      tmp.summurized[[steps[step]]] = tmp.table
    }
    
    # Check sums
    check.sums = lapply(tmp.summurized, sum, na.rm = T)
    if(!(length(unique(check.sums))==1)){
      cat.sums = paste(unlist(check.sums), collapse = " ")
      print(paste0("Error with ", shape, " / Counts are ", cat.sums))}
    
    # Make list
    for (step in names(landUse_stack)){
      AllExtracted[[step]][[shape]] = c(tmp.summurized[[step]])
    }
    
    # TRANSITIONS - hardcoded number of years for simplicity, not flexible
    lu.1990_2000 = crosstab(x=masked$lu.1990.18, y=masked$lu.2000.18)
    lu.2000_2010 = crosstab(x=masked$lu.2000.18, y=masked$lu.2010.18)
    
    trans.1990_2000 = as.data.frame(lu.1990_2000) %>%
      setNames(c("lu.1990.18", "lu.2000.18", shape))
    trans.2000_2010 = as.data.frame(lu.2000_2010)%>%
      setNames(c("lu.2000.18", "lu.2010.18", shape))
    
    
    trans.both.90to00 = 
      suppressWarnings(data.frame(trans = "1990to2000",
                                  lu.1990.18=as.factor(from), 
                                  lu.2000.18=as.factor(to)) %>%
                         left_join(trans.1990_2000, keep = shape, 
                                   by=c("lu.1990.18", "lu.2000.18")) %>%
                         setNames(c("Trans", "From", "To", "Freq")))
    
    trans.both.00to10 = 
      suppressWarnings(data.frame(trans = "2000to2010",
                                  lu.2000.18=as.factor(from), 
                                  lu.2010.18=as.factor(to)) %>%
                         left_join(trans.2000_2010, keep = shape, 
                                   by=c("lu.2000.18", "lu.2010.18")) %>%
                         setNames(c("Trans", "From", "To", "Freq")))
    
    trans.both = rbind(trans.both.90to00,trans.both.00to10)
    
    AllTransitions[[shape]] = trans.both
    
    # Update progress bar
    i=i+1
    setTxtProgressBar(pb, i)
  }
  
  # Close the progress bar
  close(pb)
  return(list(Transitions=AllTransitions, Values=AllExtracted))
}