# Custom gitting function with outputs and chekcs 
# Reviewed 2020 - changed

get_NON_NA_loc <- function(df){
  df <- df %>% 
    tidyr::drop_na()
  as.numeric(rownames(df))
}

custom_fit_and_predict <- function(modelformula, # Formulafor the model
                                   method = NULL, # one of "gam" or "rf" or "nn"
                                   samplingmethod, # one of the sampling methods
                                   train, # the train dataset
                                   test, # the test dataset
                                   
                                   IDs, # frame_IDs to fill the frame
                                   step_list, # current and future change
                                   nb_cores, # number of cores for parallel
                                   
                                   predict = TRUE, # Whether to predict
                                   check_model = TRUE, #whether to run checks
                                   save_rasters = TRUE, # Whether to save rasters
                                   make_plot = TRUE, # Whether to produce a plot
                                   save_fit = TRUE, # Whether to save the fit
                                   
                                   template = NULL, # template to save rasters
                                   template_buffer = NULL, # template to save rasters
                                   ntrees=1000 # number of trees
                                   ){
  
  # Out dir
  outdir =  "outputs/"
  
  # Print useful messages
  message(paste0 ("Running ", method))
  Sys.time()
  
  # Make formula out of string
  modelformula_formatted <- formula(modelformula)
  
  # Obtain the response var, urb of agex
  response_var <- as.character(terms(modelformula_formatted, simplify = T)[[2]])
  pred_vars <- labels(terms(modelformula_formatted))
  good_vars <- c(response_var,labels(terms(modelformula_formatted)))
  
  # Create cluster
  cl <- parallel::makeCluster(nb_cores)
  
  message("Start fiting...")
  # Fit model with MGCV
  if (method == "gam"){
    
    fit <- mgcv::bam(modelformula_formatted, 
                     data=train, family = "binomial", cluster = cl)
    
  } else if (method == "rf"){
    
    fit <- ranger::ranger(modelformula_formatted, num.trees = ntrees,
                          data=train, classification=F, importance = "impurity")
    
  } else if (method == "glm"){
    
    fit <- glm(modelformula_formatted, 
               data=train, family = "binomial")
    
  } else if (method == "nn"){
    
    fit <- train_NN_model(train = train, test = test, 
                          response = response_var, predvars = pred_vars, 
                          epochs = 10, lr=0.001, batch_size=32, 
                          validation_split=0.2, 
                          momentum = NULL)
    
  } else if (method == "brms"){
    
    # fit <- brm(modelformula_formatted, 
    #            data=train, family = "bernoulli", cores = nb_cores)
    
  }
  message("Fiting done.")
  
  # Print summary by default
  #print(fit)
  #print(summary(fit))
  
  # Save the fitted object or not
  if (save_fit){
    dir.create(paste0(outdir, method), showWarnings = FALSE)
    saveRDS(fit, paste0(outdir, method, "/", response_var, "_", method, "_", samplingmethod, ".rds"))
  }
  
  # Produce a plot or not
  if (make_plot){
    if (method == "gam"){
      #  fix bug
      # png(file.path(outdir, method, paste0(response_var, "_", method, "_plot.png")))
      # plot(fit, trans = plogis, pages=1,
      #      shift = coef(fit)[1],
      #      seWithMean = TRUE)
      # dev.off()
    }
  }
  
  # Check the model or not
  if (check_model){
    if (method == "gam"){
      message("Checking model...") ; Sys.time()
      png(paste0(outdir, method, "/", response_var, "_", method,  "_", samplingmethod, "_check.png"))
      gam.check(fit)
      dev.off()
    } else if (method == "rf"){
      message("RSquared:")
      message(fit$r.squared)
    }
  }
  
  # Now run the predict part
  if (predict){
    message(paste0("Predicting ", method))
    Sys.time()
    if (method == "gam"){
      
      prediction.train <- 
        mgcv::predict.bam(fit, type="response")
      prediction.test <- 
        mgcv::predict.bam(fit, test, cluster = cl, type="response")
      prediction.current <- 
        mgcv::predict.bam(fit, step_list$present, cluster = cl, type="response")
      prediction.future <- 
        mgcv::predict.bam(fit, step_list$future, cluster = cl, type="response")
      
    } else if (method == "rf"){
      
      prediction.train <- 
        ranger::predictions(fit, type="response")
      prediction.test <- 
        predict(fit, data = test, type="response")$predictions
      prediction.current <- 
        predict(fit, data = drop_na(step_list$present[,good_vars]), 
                type="response")$predictions
      prediction.future <- 
        predict(fit, data = drop_na(step_list$future[,good_vars]), 
                type="response")$predictions
      
    } else if (method == "glm"){
      
      prediction.train <- 
        predict(fit, type="response")
      prediction.test <- 
        predict(fit, test, type="response")
      prediction.current <- 
        predict(fit, step_list$present, type="response")
      prediction.future <- 
        predict(fit, step_list$future, type="response")
      
    } else if (method == "nn"){
      
      # Pred test not needed
      prediction.train <- fit$model %>% 
        predict(x=as.matrix(train[,pred_vars]))
      prediction.test <- fit$model %>%
        predict(x=as.matrix(test[,pred_vars]))
      prediction.current <- fit$model %>% 
        predict(x=as.matrix(step_list$present[,pred_vars]))
      prediction.future <- fit$model %>% 
        predict(x=as.matrix(step_list$future[,pred_vars]))
      
    } else if (method == "brms"){
      
      # prediction.test <- 
      #   predict(fit, test, type="response", nsamples=5, summary=T)
      # prediction.current <- 
      #   predict(fit, drop_na(step_list$present[,good_vars]), type="response", nsamples=5, summary=T)
      # prediction.future <- 
      #   predict(fit, drop_na(step_list$future[,good_vars]), type="response", nsamples=5, summary=T)
      # 
      # print(head(prediction.current))
      # print(head(prediction.future))
      
    }
  }
  
  # Save output
  if (save_rasters){
    
    message("Saving Raster...") ; Sys.time()
    
    if (method == "gam") {
      
      raster.current <- template
      raster.future <- template
      
      values(raster.current) <- NA
      values(raster.future) <- NA
      
      values(raster.current) <- prediction.current
      values(raster.future) <- prediction.future
      
      writeRaster(raster.current, 
                  paste0(outdir, method, "/", response_var, "_", 
                         method, "_", samplingmethod, "_c.tif"), 
                  overwrite = TRUE)
      writeRaster(raster.future, 
                  paste0(outdir, method, "/", response_var, "_", 
                         method, "_", samplingmethod, "_f.tif"), 
                  overwrite = TRUE)
      
      # FOR STSIM
      # Build spatial multiplier 
      message("Saving spatial multiplier...") ; Sys.time()
      
      template_buffer[IDs] <- prediction.future
      spa_mul <- template_buffer
      
      writeRaster(spa_mul, 
                  paste0("data/stsim/spatial_multipliers/",
                         method, "_", samplingmethod, "_", response_var,"_spa_mul.tif"), 
                  overwrite = TRUE)
      
    } else if (method == "rf"){
      
      raster.current <- template
      raster.future <- template
      
      # important cause rf does not work with NA values
      
      values(raster.current) <- NA
      values(raster.future) <- NA
      
      raster.current[get_NON_NA_loc(step_list$present[,good_vars])] <- 
        prediction.current
      raster.future[get_NON_NA_loc(step_list$future[,good_vars])] <- 
        prediction.future
      
      writeRaster(raster.current, 
                  paste0(outdir, method, "/", response_var, "_", 
                         method, "_", samplingmethod,"_c.tif"), 
                  overwrite = TRUE)
      writeRaster(raster.future, 
                  paste0(outdir, method, "/", response_var, "_", 
                         method, "_", samplingmethod,"_f.tif"), 
                  overwrite = TRUE)
      
      # FOR STSIM
      # can I mosaic right in there? no DIFFERENT ORIGINS
      #Build spatial multiplier
      message("Saving spatial multiplier") ; Sys.time()
      multiplier <- template_buffer
      multiplier[template_buffer==1] <- values(raster.future)
      writeRaster(multiplier,
                  paste0("data/stsim/spatial_multipliers/",
                         method, "_", samplingmethod, "_", response_var,"_spa_mul.tif"),
                  overwrite = TRUE)
      
    } else if (method == "glm"){
      
      raster.current <- template
      raster.future <- template
      
      values(raster.current) <- NA
      values(raster.future) <- NA
      
      values(raster.current) <- prediction.current
      values(raster.future) <- prediction.future
      
      writeRaster(raster.current, 
                  paste0(outdir, method, "/", response_var, "_", 
                         method, "_", samplingmethod, "_c.tif"), 
                  overwrite = TRUE)
      writeRaster(raster.future, 
                  paste0(outdir, method, "/", response_var, "_", 
                         method, "_", samplingmethod, "_f.tif"), 
                  overwrite = TRUE)
      
      # FOR STSIM
      # Build spatial multiplier 
      message("Saving spatial multiplier...") ; Sys.time()
      
      template_buffer[IDs] <- prediction.future
      spa_mul <- template_buffer
      
      writeRaster(spa_mul, 
                  paste0("data/stsim/spatial_multipliers/",
                         method, "_", samplingmethod, "_", response_var,"_spa_mul.tif"), 
                  overwrite = TRUE)
      
    } else if (method == "nn"){
      
      raster.current <- template
      raster.future <- template
      
      values(raster.current) <- NA
      values(raster.future) <- NA
      
      values(raster.current) <- prediction.current
      values(raster.future) <- prediction.future
      
      writeRaster(raster.current, 
                  paste0(outdir, method, "/", response_var, "_", 
                         method, "_", samplingmethod, "_c.tif"), 
                  overwrite = TRUE)
      writeRaster(raster.future, 
                  paste0(outdir, method, "/", response_var, "_", 
                         method, "_", samplingmethod, "_f.tif"), 
                  overwrite = TRUE)
      
      # FOR STSIM
      # Build spatial multiplier 
      message("Saving spatial multiplier") ; Sys.time()
      
      template_buffer[IDs] <- prediction.future
      spa_mul <- template_buffer
      
      writeRaster(spa_mul, 
                  paste0("data/stsim/spatial_multipliers/",
                         method, "_", samplingmethod, "_", response_var,"_spa_mul.tif"), 
                  overwrite = TRUE)
      
    } else if (method == "brms"){
      
      # raster.current <- template
      # raster.future <- template
      #         
      # # important cause rf does not work with NA values
      # 
      # values(raster.current) <- NA
      # values(raster.future) <- NA
      #         
      # raster.current[get_NON_NA_loc(step_list$present[,good_vars])] <- 
      #   prediction.current
      # raster.future[get_NON_NA_loc(step_list$future[,good_vars])] <- 
      #   prediction.future
      #         
      # writeRaster(raster.current, 
      #             paste0(outdir, method, "/", response_var, "_", 
      #                    method, "_", samplingmethod, "_c.tif"), 
      #             overwrite = TRUE)
      # writeRaster(raster.future, 
      #             paste0(outdir, method, "/", response_var, "_", 
      #                    method, "_", samplingmethod, "_f.tif"), 
      #             overwrite = TRUE)
      # 
      # # FOR STSIM
      # # can I mosaic right in there? no DIFFERENT ORIGINS
      # #Build spatial multiplier
      # print("Saving spatial multiplier") ; Sys.time()
      # multiplier <- template_buffer
      # multiplier[template_buffer==1] <- values(raster.future)
      # writeRaster(multiplier,
      #             paste0("data/stsim/spatial_multipliers/",
      #                    method, "_", samplingmethod, "_", response_var,"_spa_mul.tif"),
      #             overwrite = TRUE)
      
    }
  }
  
  message("Outputting...")
  stopCluster(cl)
  Sys.sleep(2)
  out_list <- list(fit=fit, 
                   prediction.train=prediction.train,
                   prediction.test=prediction.test, 
                   prediction.current=prediction.current, 
                   prediction.future=prediction.future, 
                   raster.current=raster.current, 
                   raster.future=raster.future,
                   response=response_var)
}
