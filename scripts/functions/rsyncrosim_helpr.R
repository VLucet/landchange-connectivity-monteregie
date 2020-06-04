## Compiling functions for the rsyncrosim_helpr package
# Reviewed 2020 - no change

# Loads a datasheet from csv and saves it 
# wrapper for `savedatasheet` function

loadSheet <- function(sheetname, filename=NULL, proj_or_sce, path = getwd(), params = NULL){
  
  if (is.null(filename)){
    filename = sheetname
  }
  
  # Change parameters 
  if (!(is.null(params))){
    if (is.list(params)){
      mySheet <- as.data.frame(params)
      # for (par in names(params)){
      #   mySheet[[par]] <- params[[par]]
      # }
    } else{
      stop("ERROR: parameters not provided as a list")
    }
  } else {
    mySheet <- read.csv(paste0(path,filename,".csv"), header = T)
  }	
  
  # Save the sheet
  print(head(mySheet))
  out <- saveDatasheet(proj_or_sce, mySheet, sheetname)
  
  # Check if successful 
  if (out==FALSE){
#    print(paste0(sheetname, " Saved"))
#  } else{
    stop("ERROR: Datasheet could not be loaded. Check path or name.")
  }
}
