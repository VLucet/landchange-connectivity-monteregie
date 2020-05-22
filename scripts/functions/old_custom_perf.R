# Custom performance functions
# Reviewed 2020 - changed

custom_perf <- function(predictions_list, # Raster of predictions
                        labels_list, # Actual data
                        predictions_steps, # names of the steps
                        method, # Fitting method
                        samplingmethod, # Sampling method
                        measures_list){  # performance measure
  
  
  # Create empty list
  perf_list <- vector(mode = "list", length=length(predictions_steps))
  names(perf_list) <- predictions_steps
  
  for (index in 1:length(predictions_steps)){
    
    the_predictions <- predictions_list[[index]]
    the_labels <- labels_list[[index]]
    the_step <- predictions_steps[[index]]
    
    if (class(predictions_list[[index]])=="RasterLayer"){
      # Locates NA is both prediction and labels and only look at cells
      # that are not NA in the raster
      NA_LOC <- which(is.na(raster::values(the_predictions)) | is.na(the_labels))
      pred_and_labs <- ROCR::prediction(predictions = raster::values(the_predictions)[-NA_LOC], 
                               labels = the_labels[-NA_LOC])
    } else{
      pred_and_labs <- ROCR::prediction(predictions=c(the_predictions),
                               labels=c(the_labels))
    }
    
    for (measure in measures_list){
      # Calculates performance in 2 different ways
      # perf_list[[]]
      # perf_auc <- ROCR::performance(pred, measure = "auc") 
      # measures_list <- list(pred=pred, rch=perf_rch, auc=perf_auc)
      perf <- (ROCR::performance(pred_and_labs, measure = measure))
      if (length(perf@y.values[[1]]) == 1 ){
        perf <- perf@y.values[[1]]
      } else{
        # invoke plotting here
        perf <- mean(perf@y.values[[1]])
      }
      #print(perf)
      perf_list[[the_step]][[measure]] <- perf
      #print(perf_list)
    }
  }
  
  saveRDS(perf_list, file.path("outputs", method, 
                               paste0(method, "_perf_", samplingmethod,".rds")))
  
  return(perf_list)
}
 
# print("****")
# print(head(the_predictions))
# print(head(the_labels))
# print("****")
# print(dim(the_predictions))
# print(dim(the_labels))
# print(length(the_predictions))
# print(length(the_labels))
# print("****")
# print(class(the_predictions))
# print(the_step)
# print("****")