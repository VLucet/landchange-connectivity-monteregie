produce_new_transition_names <- function(trans){
  
  full <- NULL
  
  for (id in 1:length(trans)){
    
    tr <- trans[id]
    
    left_bit <- NULL
    right_bit <- NULL
    
    splitted <- unlist(strsplit(tr, "->"))
    #print(splitted)
    
    if (grepl(splitted[1], pattern = "Coniferes")){
      #print(splitted[1])
      left_bit[1] <- "conif"
      
      if (grepl(splitted[1], pattern = "Jeune")){
        left_bit[2] <- "1"
      } else if (grepl(splitted[1], pattern = "Moyenne")){
        left_bit[2] <- "2"
      } else if (grepl(splitted[1], pattern = "Vieille")){
        left_bit[2] <- "3"
      } else{
        stop("Step1")
      }
      
    } else if (grepl(splitted[1], pattern = "Feuillus")){
      #print(splitted[1])
      left_bit[1] <- "decid"
      
      if (grepl(splitted[1], pattern = "Jeune")){
        left_bit[2] <- "1"
      } else if (grepl(splitted[1], pattern = "Moyenne")){
        left_bit[2] <- "2"
      } else if (grepl(splitted[1], pattern = "Vieille")){
        left_bit[2] <- "3"
      } else{
        stop("Step2")
      }
      
    } else if (grepl(splitted[1], pattern = "Mixte")){
      left_bit[1] <- "mixt"
      
      if (grepl(splitted[1], pattern = "Jeune")){
        left_bit[2] <- "1"
      } else if (grepl(splitted[1], pattern = "Moyenne")){
        left_bit[2] <- "2"
      } else if (grepl(splitted[1], pattern = "Vieille")){
        left_bit[2] <- "3"
      } else{
        stop("Step2")
      }
      
    }
    
    if (grepl(splitted[2], pattern = "Coniferes")){
      #print(splitted[1])
      right_bit[1] <- "conif"
      
      if (grepl(splitted[2], pattern = "Jeune")){
        right_bit[2] <- "1"
      } else if (grepl(splitted[2], pattern = "Moyenne")){
        right_bit[2] <- "2"
      } else if (grepl(splitted[2], pattern = "Vieille")){
        right_bit[2] <- "3"
      } else{
        stop("Step3")
      }
      
    } else if (grepl(splitted[2], pattern = "Feuillus")){
      #print(splitted[1])
      right_bit[1] <- "decid"
      
      if (grepl(splitted[2], pattern = "Jeune")){
        right_bit[2] <- "1"
      } else if (grepl(splitted[2], pattern = "Moyenne")){
        right_bit[2] <- "2"
      } else if (grepl(splitted[2], pattern = "Vieille")){
        right_bit[2] <- "3"
      } else{
        stop("Step4")
      }
      
    } else if (grepl(splitted[2], pattern = "Mixte")){
      right_bit[1] <- "mixt"
      
      if (grepl(splitted[1], pattern = "Jeune")){
        right_bit[2] <- "1"
      } else if (grepl(splitted[1], pattern = "Moyenne")){
        right_bit[2] <- "2"
      } else if (grepl(splitted[1], pattern = "Vieille")){
        right_bit[2] <- "3"
      } else{
        stop("Step2")
      }
      
    }
    
    #print(left_bit)
    #print(right_bit)
    full[id] <- paste0(paste(left_bit, collapse = "_"), 
                       "_to_",
                       paste(right_bit, collapse = "_"))
    
    return(full)
  }
}