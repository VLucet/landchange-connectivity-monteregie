#-------------------------------------------------------------------------------
## 3.2 
## 2020
## Inputs: Dataframe etc.
## Outputs: Spatial multipliers, model fit
#-------------------------------------------------------------------------------

rm(list=ls())
set.seed(77)

#-------------------------------------------------------------------------------
# Set parameters 
OMP_NUM_THREADS <- as.numeric(Sys.getenv("OMP_NUM_THREADS"))-1
options(mc.cores=OMP_NUM_THREADS)
R_METHOD <- Sys.getenv("R_METHOD")
# R_CROP <- as.logical(Sys.getenv("R_CROP")) => never got implemented 
R_N_TREES <- as.numeric(Sys.getenv("R_N_TREES"))
R_PART <- as.numeric(Sys.getenv("R_PART"))

options(stringsAsFactors = FALSE)

if (is.na(OMP_NUM_THREADS)) { 
  OMP_NUM_THREADS <- 6 
  options(mc.cores = OMP_NUM_THREADS)
  R_N_TREES <- 500
  R_PART <- 0.7
  
  print("Running on 6 cores only and using default input parameters") 
  setwd("~/Documents/Master/Thesis/land_con_monteregie/")
}
#-------------------------------------------------------------------------------

# Set Working directory
# wd <- "~/home/vlucet/scratch/oct19_landuse_model"
# setwd(wd)

## Load required packages ##
suppressPackageStartupMessages({
  library(tidymodels)
  library(raster)
  library(dplyr)
  library(rlist)
})

# Reset Raster tmp files
removeTmpFiles(0)
showTmpFiles()

#-------------------------------------------------------------------------------
# Raster template
template <- raster("data/temp/template.tif")
method <- "rf"
ratio <- 2

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

#-------------------------------------------------------------------------------

for (response in c("agex")){
  
  temp_data <- full_set[[response]]$current
  
  data_split <- initial_split(temp_data, prop = R_PART)
  train_data <- training(data_split)
  test_data  <- testing(data_split)
  
  # formula("urb ~ .")
  
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
    step_downsample(outcome_fact, skip = TRUE, under_ratio = ratio)
  
  rf_mod <- 
    rand_forest(trees = R_N_TREES, mode = "regression") %>% 
    set_engine("ranger", 
               num.threads = OMP_NUM_THREADS)
  
  rf_wflow <- 
    workflow() %>% 
    add_model(rf_mod) %>% 
    add_recipe(rec)
  
  # Fit
  rf_fit <- 
    rf_wflow %>% 
    fit(data = train_data)
  
  # Pred
  rf_pred <- 
    rf_fit %>% 
    predict(test_data) %>% 
    bind_cols(test_data %>% dplyr::select(outcome_fact))
  
  auc <- round((rf_pred %>% 
                  roc_auc(truth = outcome_fact, .pred))$.estimate, 4)
  plot <- rf_pred %>% 
    roc_curve(truth = outcome_fact, .pred) %>% 
    autoplot() + ggtitle(paste("Roc Curve for", method, response)) +
    annotate(x = 0.75, y = 0.25, geom="label", 
             label = as.character(paste("AUC =", auc)))
  ggsave(file.path("outputs", method, paste0(response,"_roc.png")))
  
  full_rf_pred <- 
    rf_fit %>% 
    predict(new_data=temp_data)
  
  spa_mul <- template
  values(spa_mul) <- NA
  spa_mul[temp_data$row_nb] <- full_rf_pred$.pred
  plot(spa_mul)
  
  writeRaster(spa_mul, 
              file.path("data/stsim/spatial_multipliers/",
                        paste0(method, "_ratio_", ratio,"_", response,"_spamul.tif")), 
              overwrite = TRUE)
}


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