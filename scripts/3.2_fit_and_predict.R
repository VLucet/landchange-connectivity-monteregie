#-------------------------------------------------------------------------------
## 3.2 Fit and predict model with tidymodels
## 2020
## Inputs: Dataframe etc.
## Outputs: Spatial multipliers, model fit
#-------------------------------------------------------------------------------

rm(list=ls())
set.seed(77)

#-------------------------------------------------------------------------------
# Set parameters 
OMP_NUM_THREADS <- as.numeric(Sys.getenv("OMP_NUM_THREADS", unset = 10))
options(mc.cores=OMP_NUM_THREADS)

R_METHOD <- Sys.getenv("R_METHOD", unset = "rf")
R_N_TREES <- as.numeric(Sys.getenv("R_N_TREES", unset = 500))
R_PART <- as.numeric(Sys.getenv("R_PART", unset = 0.7))
R_RATIO <- as.numeric(Sys.getenv("R_RATIO", unset = 2))

options(stringsAsFactors = FALSE)
#-------------------------------------------------------------------------------

# Set Working directory
# wd <- "~/home/vlucet/scratch/oct19_landuse_model"
# setwd(wd)

## Load required packages ##
suppressPackageStartupMessages({
  library(tidymodels)
  library(raster)
  library(dplyr)
  library(tidyr)
  library(rlist)
  library(parallel)
  library(doParallel)
  library(foreach)
  library(purrr)
  library(ranger)
  library(ROCR)
})

# Reset Raster tmp files
removeTmpFiles(0)
showTmpFiles()

# Yardstick option 
options(yardstick.event_first = FALSE)

# cl <- makeForkCluster(OMP_NUM_THREADS)
# doParallel::registerDoParallel(cl)
# foreach::getDoParWorkers()

# Register the cluster

#-------------------------------------------------------------------------------
# Raster template
template <- raster("data/temp/template.tif")
lu.buffer.template <- raster("data/temp/template_with_buffer.tif")

# test <- readRDS("data/temp/data_temp.RDS")

# Load inputs
full_data <- readRDS("data/temp/full_df.RDS") %>% 
  mutate(pop_change = (pop_01-pop_90)) %>%
  # mutate(urb_fact = as.factor(urb)) %>% 
  # mutate(agex_fact = as.factor(agex)) %>% 
  dplyr::select(row_nb, agex, urb, timestep, 
                pop_change, dis_90, in_90, for_90, elev, mun)

urb_set <- 
  full_data %>% 
  dplyr::select(-agex) %>% 
  mutate(outcome_fact = as.factor(urb)) %>% 
  drop_na() %>% 
  split(.$timestep) %>% 
  `names<-`(x=., value=c("current", "future"))

agex_set <- 
  full_data %>% 
  dplyr::select(-urb) %>% 
  mutate(outcome_fact = as.factor(agex)) %>% 
  drop_na() %>% 
  split(.$timestep) %>% 
  `names<-`(x=., value=c("current", "future"))

full_set <- list(urb = urb_set, agex = agex_set)

methics_df <- data.frame()
rs_metrics <- data.frame()
var_imp <- data.frame()

cls_metrics <- metric_set(roc_auc, sens, spec)
#-------------------------------------------------------------------------------

# response <-"urb"
# ratio <- 2

for (response in c("agex","urb")){
  
  for (ratio in R_RATIO){
    
    temp_data <- full_set[[response]]$current
    future_data <- full_set[[response]]$future
    
    data_split <- initial_split(temp_data, prop = R_PART)
    train_data <- training(data_split)
    test_data  <- testing(data_split)
    
    train_data_folds <- 
      vfold_cv(train_data, v = 10)
    
    # formula("urb ~ .")
    
    ## RECIPE ##
    rec <- 
      recipe(formula(paste0(response, " ~ .")), data = temp_data) %>% 
      update_role(timestep, row_nb, outcome_fact, 
                  new_role = "ID/group Variables") %>% 
      step_zv(all_predictors()) %>% 
      step_scale(all_numeric(), 
                 -all_outcomes(), 
                 -has_role("Other response"), 
                 -has_role("ID/group Variables")) %>% 
      step_dummy(mun) %>% 
      step_downsample(outcome_fact, skip = TRUE, under_ratio = R_RATIO)
    
    ## ENGINE ##
    if (R_METHOD == "rf"){
      mod <- 
        rand_forest(trees = R_N_TREES, mode = "regression") %>% 
        set_engine("ranger", 
                   num.threads = OMP_NUM_THREADS,
                   importance = "impurity")
    } else {
      stop("Method not implemented")
    }
    
    ## WORKFLOW ##
    wflow <- 
      workflow() %>% 
      add_model(mod) %>% 
      add_recipe(rec)
    
    ## FIT ##
    # Classic
    mod_fit <- 
      wflow %>% 
      fit(data = train_data)
    
    # Var Imp
    var_imp_temp <- 
      data.frame(var = names(mod_fit$fit$fit$fit$variable.importance[1:5]), 
                 importance = mod_fit$fit$fit$fit$variable.importance[1:5], 
                 response = response)
    var_imp <- bind_rows(var_imp, var_imp_temp)
    
    # Resample
    mod_fit_rs <- 
      wflow %>% 
      fit_resamples(train_data_folds, # metrics = cls_metrics,
                    control = control_resamples(save_pred = TRUE)) 
    mod_fit_rs_pred <- mod_fit_rs$.predictions
    mod_fit_rs_pred_with_outcome <- 
      purrr::map(mod_fit_rs_pred, ~cbind(., outcome_fact = train_data[.$.row,"outcome_fact"]))
    all_metrics <- 
      purrr::map(mod_fit_rs_pred_with_outcome, ~roc_auc(data = ., truth = outcome_fact, .pred)) %>% 
      purrr::map(~.$.estimate) %>% unlist()
    
    rs_metrics <- rbind(rs_metrics,
                        data.frame(mean_auc = mean(all_metrics), 
                                   sd_auc = sd(all_metrics), 
                                   method = R_METHOD,
                                   ratio = R_RATIO, 
                                   response = response))
    
    saveRDS(mod_fit_rs, paste0("data/temp/fit_rs_", R_METHOD, 
                               "_", response, "_", R_RATIO, ".RDS"))
    saveRDS(mod_fit_rs_pred_with_outcome, paste0("data/temp/fit_rs_outcome_", R_METHOD, 
                                                 "_", response, "_", R_RATIO, ".RDS"))
    
    ## TEST ##
    pred <- 
      mod_fit %>% 
      predict(test_data) %>% 
      bind_cols(test_data %>% dplyr::select(outcome_fact))
    av_prec <- round((pred %>% 
                        average_precision(truth = outcome_fact, .pred))$.estimate, 4)
    auc <- round((pred %>% 
                    roc_auc(truth = outcome_fact, .pred))$.estimate, 4)
    
    saveRDS(mod_fit, paste0("data/temp/fit_", R_METHOD, 
                            "_", response, "_", R_RATIO, ".RDS"))
    
    plot <- pred %>% 
      roc_curve(truth = outcome_fact, .pred) %>% 
      autoplot() + ggtitle(paste("Roc Curve for", R_METHOD, response)) +
      annotate(x = 0.75, y = 0.25, geom="label", 
               label = as.character(paste("AUC =", auc))) +
      annotate(x = 0.75, y = 0.10, geom="label", 
               label = as.character(paste("Av Prec =", av_prec)))
    ggsave(file.path("outputs", R_METHOD, paste0(R_METHOD, "_ratio_", R_RATIO,"_", response,"_roc.png")))
    
    methics_df <- rbind(methics_df, 
                        list(method = R_METHOD,
                             ratio = R_RATIO, 
                             response = response,
                             data = "test",
                             R2 = mod_fit$fit$fit$fit$r.squared,
                             auc = auc, 
                             av_prec = av_prec))
    
    ## PREDICTIONS Current ##
    full_pred <- 
      mod_fit %>% 
      predict(new_data=temp_data)
    
    spa_mul <- template
    values(spa_mul) <- NA
    spa_mul[temp_data$row_nb] <- full_pred$.pred
    plot(spa_mul)
    
    writeRaster(spa_mul, 
                file.path("data/stsim/spatial_multipliers/",
                          paste0(R_METHOD, "_ratio_", R_RATIO,"_", response,"_c_spamul.tif")), 
                overwrite = TRUE)
    
    ## PREDICTIONS FUTURE ##
    full_pred_future <- 
      mod_fit %>% 
      predict(new_data=future_data) %>% 
      bind_cols(future_data %>% dplyr::select(outcome_fact))
    
    av_prec_fut <- round((full_pred_future %>% 
                            average_precision(truth = outcome_fact, .pred))$.estimate, 4)
    auc_fut <- round((full_pred_future %>% 
                        roc_auc(truth = outcome_fact, .pred))$.estimate, 4)
    
    methics_df <- rbind(methics_df, 
                        list(method = R_METHOD,
                             ratio = R_RATIO, 
                             response = response,
                             data = "future",
                             R2 = mod_fit$fit$fit$fit$r.squared,
                             auc = auc_fut, 
                             av_prec = av_prec_fut))
    
    spa_mul_fut <- template
    values(spa_mul_fut) <- NA
    spa_mul_fut[future_data$row_nb] <- full_pred_future$.pred
    #plot(spa_mul_fut)
    
    final_spa_mul <- lu.buffer.template
    final_spa_mul[final_spa_mul==1] <- values(spa_mul_fut)
    
    writeRaster(final_spa_mul, 
                file.path("data/stsim/spatial_multipliers/",
                          paste0(R_METHOD, "_ratio_", R_RATIO,"_", response,"_f_spamul.tif")), 
                overwrite = TRUE)
    
  }
}

write.csv(methics_df, "outputs/metrics/metrics_table.csv")
write.csv(rs_metrics, "outputs/metrics/metrics_table_resample.csv")
write.csv(var_imp, "outputs/metrics/var_imp_table.csv")

#-------------------------------------------------------------------------------

# urb_rec <- 
#   recipe(urb ~ ., data = urb_set$current) %>% 
#   # step_rm(agex) %>% 
#   update_role(timestep, row_nb, urb_fact, new_role = "ID/group Variables") %>% 
#   step_zv(all_predictors()) %>% 
#   step_scale(all_numeric(), 
#              -all_outcomes(), 
#              -has_role("Other response"), 
#              -has_role("ID/group Variables")) %>% 
#   step_dummy(mun, skip) %>% 
#   step_downsample(urb_fact, skip = TRUE, under_ratio = 2)

# %>% 
#   prep()

# urb_rec_prep <- prep(urb_rec)
# urb_rec_juice <- juice(urb_rec_prep)

# boop <- full_data_current
# boop[,5:9] <- apply(full_data_current[5:9],FUN = scale,MARGIN = 2)
# 
# full_data_current_baked <- bake(urb_rec_prep, full_data_current)