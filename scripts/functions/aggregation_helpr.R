## Compiling functions useful for aggregating 
## Reviewed 2020 - no change

modal_custom_highest <- function(x, na.rm=T){modal(x, na.rm=T, ties="highest")}
modal_custom_first <- function(x, na.rm=T){modal(x, na.rm=T, ties="first")}
modal_custom_first_no_roads <- function(x, na.rm=T){
  temp <- modal(x, na.rm=T, ties="first")
  if(!is.na(temp)){
    if (temp == 4){
      x <- x[x != 4]
      temp <- modal(x, na.rm=T, ties="first")
    }
  }
  temp
}