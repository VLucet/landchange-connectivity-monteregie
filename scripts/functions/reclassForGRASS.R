# Function to reclass problematic characters 
# Reviewed 2020 - no change

reclassForGRASS = function(shape, char){
  ClassA = (shape@data$CLASS_A)
  for (i in 1:length(ClassA)){
    if (is.na(ClassA[i])){
      ClassA[i] = 99
    } else if (ClassA[i] == char){
      ClassA[i] = 9
    } else {
      ClassA[i] = as.integer(ClassA[i])
    }
  }
  shape@data$CLASS_A = NA
  shape@data$CLASS_A = as.integer(ClassA)
  return(shape)
}