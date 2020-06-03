get_overlap <- function(x, y, return_true = TRUE, matching = c("exact", "fuzzy"), ...){
  # get unique
  x_u <- as.character(unique(x))
  y_u <- as.character(unique(y))
  
  if (length(x_u) > length(y_u)){
    stop("Set from x is larger than set from y, please swap them.")
  }
  
  # match methods
  method <- match.arg(arg = matching, choices = c("exact", "fuzzy"))
  
  # apply method
  switch(method, 
         
         exact = {
           bool_vec_unique <- x_u %in% y_u
           bool_vec_values_x <- x %in% y
           bool_vec_values_y <- y %in% x
           
           # No ambiguity
           fuzzy <- NULL
         }, 
         
         fuzzy={
           bool_vec_unique <- sapply(lapply(x_u, FUN = agrepl, y_u), sum) >= 1
           bool_vec_values_x  <- as.logical(apply(sapply(x, FUN = agrepl, x), FUN = max, MARGIN = 2))
           bool_vec_values_y <- as.logical(apply(sapply(y, FUN = agrepl, x), FUN = max, MARGIN = 2))
           
           # Assess ambiguity
           agrep_list <- lapply(x_u, FUN = agrep, y_u)
           ambig_vec <- sapply(lapply(x_u, FUN = agrep, y_u), length)>1
           ambig_ind <- unlist(agrep_list[ambig_vec])
           fuzzy <- list(x = x_u[ambig_vec], 
                         y = y_u[ambig_ind])
         }
  )
  
  if (return_true){
    ind_vec_unique <- x_u[which(bool_vec_unique)]
    match_ind <- list(x = which(bool_vec_values_x),
                      y = which(bool_vec_values_y))
  } else {
    ind_vec_unique <- x_u[which(!bool_vec_unique)]
    match_ind <- list(x = which(!bool_vec_values_x),
                      y = which(!bool_vec_values_y))
  }

  # Assess overlap
  overlap <- list(x = (length(match_ind$x)/length(x))*100,
                  y = (length(match_ind$y)/length(y))*100,
                  x_unique = (length(ind_vec_unique)/length(x_u))*100, 
                  y_unique = (length(ind_vec_unique)/length(y_u))*100)
  
  
  return(list(x_unique = x_u,
              y_unique = y_u,
              logical_vec = bool_vec_unique, 
              index_vec = ind_vec_unique, 
              fuzzy = fuzzy, 
              match_ind = match_ind, 
              overlap = overlap))
}