# TRAIN NN
# Reviewed 2020 - no change

train_NN_model <-  function(train, test, response, predvars, epochs=NULL, 
                            lr=NULL, batch_size=NULL, validation_split=NULL, 
                            momentum=NULL){
  
  x_train <- as.matrix(train[,predvars])
  y_train <- train[,response]
  x_test <- as.matrix(test[,predvars])
  y_test <- test[,response]
  
 # if(is.factor(y_train)){
 #   y_train <- as.numeric(as.character(y_train))
 # }

  model <- keras_model_sequential() %>% 
    layer_dense(units = 12, activation = "relu", 
                input_shape = ncol(x_train)) %>% 
    layer_dense(units = 1, activation = "sigmoid")
  
  model %>% compile(
    optimizer = optimizer_adam(lr=lr),
    loss = "binary_crossentropy",
    metric = "accuracy")
  
  history <- model %>% fit(
    x=x_train,
    y=y_train,
    epochs = epochs,
    batch_size = batch_size,
    #validation_split = validation_split
    validation_data=list(x_test, y_test))
  
  return(list(model=model, history=history))
}
