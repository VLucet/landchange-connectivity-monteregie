## Compiling functions useful for aggregating 
## Reviewed 2020 - no change

modal_custom_highest <- function(x, na.rm=T){modal(x, na.rm=T, ties="highest")}
modal_custom_first <- function(x, na.rm=T){modal(x, na.rm=T, ties="first")}
