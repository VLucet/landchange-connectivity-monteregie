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
  R_N_TREES <- 1000
  R_PART <- 0.7
  
  print("Running on 6 cores only and using default input parameters") 
  setwd("~/Documents/Master/Thesis/land_con_monteregie/")
}
#-------------------------------------------------------------------------------

test <- readRDS("data/temp/data_temp.RDS")

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

# Load inputs
full_data <- readRDS("data/temp/full_df.RDS") %>% 
  mutate(pop_change = (pop_01-pop_90)) %>%
  mutate(urb_fact = as.factor(urb)) %>% 
  mutate(agex_fact = as.factor(agex)) %>% 
  dplyr::select(row_nb, agex, urb, timestep, 
                pop_change, dis_90, in_90, for_90, elev, mun, urb_fact)

full_data_current <- full_data %>% 
  filter(timestep == 1) %>% drop_na()
full_data_future <- full_data %>% 
  filter(timestep == 2) %>%  drop_na()

data_split <- initial_split(full_data_current, prop = R_PART)
train_data <- training(data_split)
test_data  <- testing(data_split)
         
urb_rec <- 
  recipe(urb ~ . , data = full_data_current) %>% 
  # step_rm(agex) %>% 
  update_role(timestep, row_nb, urb_fact, new_role = "ID/group Variables") %>% 
  step_zv(all_predictors()) %>% 
  step_scale(all_numeric(), 
             -all_outcomes(), 
             -has_role("Other response"), 
             -has_role("ID/group Variables")) %>% 
  step_downsample(urb_fact, skip = TRUE, under_ratio = 2) %>% 
  prep()

# urb_rec_prep <- prep(urb_rec)
# urb_rec_juice <- juice(urb_rec_prep)

rf_mod <- 
  rand_forest(trees = 100, mode = "regression") %>% 
  set_engine("ranger", 
             num.threads = OMP_NUM_THREADS)

urb_rf_wflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(urb_rec)

# Fit

urb_rf_fit <- 
  urb_rf_wflow %>% 
  fit(data = train_data)

# Pred

urb_rf_pred <- 
  urb_rf_fit %>% 
  predict(test_data) %>% 
  bind_cols(test_data %>% dplyr::select(urb))

urb_rf_pred %>% 
  roc_curve(truth = as.factor(urb), .pred) %>% 
  autoplot()

urb_rf_pred %>% 
  roc_auc(truth = as.factor(urb), .pred)

# boop <- full_data_current
# boop[,5:9] <- apply(full_data_current[5:9],FUN = scale,MARGIN = 2)

full_data_current_baked <- bake(urb_rec_prep, full_data_current)

urb_rf_pred <- 
  urb_rf_fit %>% 
  predict(new_data=full_data_current) %>% 
  bind_cols(full_data_current %>% dplyr::select(urb))

spa_mul <- template
values(spa_mul) <- NA
spa_mul[full_data_current$row_nb] <- urb_rf_pred$.pred
plot(spa_mul)
