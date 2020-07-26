#-------------------------------------------------------------------------------
## 2.2 Prepare transition targets for stsim
## 2020
## Inputs: land use
## Outputs: Transition targets
#-------------------------------------------------------------------------------

# reviewed 2020 

# Remove all in environment
rm(list = ls())

# Load required packages
suppressPackageStartupMessages({
  library(raster)
  library(sf)
  library(tidyverse)
  library(fasterize)
})

# Reset Raster tmp files
removeTmpFiles(0)
showTmpFiles()

#-------------------------------------------------------------------------------

# Transition size
source("scripts/functions/extract_vals_and_trans.R")
lu.18.sub <- stack(raster("data/land_use/LandUse_mont_aafc_30by30_1990.tif"),
                   raster("data/land_use/LandUse_mont_aafc_30by30_2000.tif"),
                   raster("data/land_use/LandUse_mont_aafc_30by30_2010.tif"))

names(lu.18.sub) <- c("lu.1990.18", "lu.2000.18", "lu.2010.18")
mun.sub.18.clean <- st_read("data/mun//munic_SHP_clean.shp", quiet = TRUE)
mun.list <- as.character(unique((mun.sub.18.clean$MUS_NM_MUN)))
classes <- data.frame(Code=(0:6), Label=c("Other","Agriculture", "Urban", 
                                          "Forest", "Roads", "Water","Wetlands"))

All.Mont <- ExtractValsAndTrans(
  shape_vec = mun.list,
  shape_sf = mun.sub.18.clean,
  attribute = "MUS_NM_MUN",
  landUse_stack = lu.18.sub,
  classes = classes
)

saveRDS(All.Mont, "data/temp/vals_and_trans.RDS")
All.Mont <- readRDS("data/temp/vals_and_trans.RDS")

TransTotal <- All.Mont$Transitions

# Make dtaframe (from script 1)
TransTotal_saved.df <- data.frame()
for (mun in mun.list) {
  trans.df <- TransTotal[[mun]] %>%
    mutate(Mun = mun)
  TransTotal_saved.df <- bind_rows(TransTotal_saved.df, trans.df)
}

#-------------------------------------------------------------------------------

# select and format for SSIM

TransTotal_saved.df$Freq[is.na(TransTotal_saved.df$Freq)] <- 0

SSIM_targets <- TransTotal_saved.df %>% 
  dplyr::rename("SecondaryStratumID"=Mun, "Amount"=Freq) %>% 
  mutate("TransitionGroupID"= "") 

for (i in 1:length(SSIM_targets$From)){
  From <- SSIM_targets$From[i]
  To <- SSIM_targets$To[i]
  
  if (From == To){
    trans = "SAME"
  }
  
  if (From == 1 & To == 2){
    trans <- "Agricultural Loss [Type]"
  } else if (From == 3 & To == 1){
    trans <- "Agricultural Expansion [Type]"
  } else if (From == 3 & To == 2){
    trans <- "Deforestation [Type]"
  } else {
    trans <- "Other"
  }
  
  SSIM_targets$TransitionGroupID[i] <- trans
}

SSIM_targets_selected_h_per_y <-
  SSIM_targets %>% 
  dplyr::filter(TransitionGroupID != "SAME") %>% 
  dplyr::filter(TransitionGroupID != "Other") %>% 
  group_by(SecondaryStratumID, TransitionGroupID) %>% 
  summarise(Amount=sum(Amount)*0.09/20)

SSIM_targets_selected_h_per_10y <-
  SSIM_targets %>% 
  dplyr::filter(TransitionGroupID != "SAME") %>% 
  dplyr::filter(TransitionGroupID != "Other") %>% 
  rename(Timestep=Trans) %>% 
  group_by(SecondaryStratumID, TransitionGroupID, Timestep) %>% 
  summarise(Amount=sum(Amount)*0.09) %>%
  ungroup() %>% glimpse() 
SSIM_targets_selected_h_per_10y$Timestep <- 
  ifelse(SSIM_targets_selected_h_per_10y$Timestep == "1990to2000", 1, 2) %>% glimpse()

SSIM_targets_selected_h_per_10y_mean <- SSIM_targets_selected_h_per_10y %>% 
  group_by(SecondaryStratumID, TransitionGroupID) %>% 
  summarise(Amount=mean(Amount)) %>% 
  mutate(Timestep = 3)

SSIM_targets_selected_h_per_10y_final <- 
  bind_rows(SSIM_targets_selected_h_per_10y,SSIM_targets_selected_h_per_10y_mean)

write.csv(SSIM_targets_selected_h_per_y, "config/stsim/TransitionTarget_1y.csv", 
          row.names=F)
write.csv(SSIM_targets_selected_h_per_10y_final, "config/stsim/TransitionTarget_10y.csv", 
          row.names=F)

#-------------------------------------------------------------------------------

# reforestation

SSIM_targets_selected_h_per_y <- read_csv("config/stsim/TransitionTarget_1y.csv")
SSIM_targets_selected_h_per_10y_final <- read_csv("config/stsim/TransitionTarget_10y.csv")

## MUN LEVEL

# SSIM_targets_selected_h_per_10y_final %>% 
#   filter(TransitionGroupID %in% c("Deforestation [Type]", "Agricultural Expansion [Type]")) %>% 
#   group_by(SecondaryStratumID, Timestep) %>% 
#   summarise(Amount = sum(Amount)) %>% ungroup %>% 
#   mutate(TransitionGroupID = "Reforestation [Type]")  %>% 
#   bind_rows(SSIM_targets_selected_h_per_10y_final) -> SSIM_targets_selected_h_per_10y_final_with_ref
# 
# write.csv(SSIM_targets_selected_h_per_10y_final_with_ref, 
#           "config/stsim/TransitionTarget_10y_ref.csv", 
#           row.names=F)
# 
# SSIM_targets_selected_h_per_10y_final %>% 
#   filter(TransitionGroupID %in% c("Deforestation [Type]", "Agricultural Expansion [Type]")) %>% 
#   group_by(SecondaryStratumID, Timestep) %>% 
#   summarise(Amount = sum(Amount)) %>% ungroup %>% 
#   mutate(TransitionGroupID = "Reforestation [Type]") %>% 
#   mutate(Amount = 0) %>% 
#   bind_rows(SSIM_targets_selected_h_per_10y_final) -> SSIM_targets_selected_h_per_10y_final_with_ref_0
# 
# write.csv(SSIM_targets_selected_h_per_10y_final_with_ref_0, 
#           "config/stsim/TransitionTarget_10y_ref_0.csv", 
#           row.names=F)

# MONT LEVEL

urb_total <-  SSIM_targets_selected_h_per_10y_final %>% 
  filter(TransitionGroupID != "Agricultural Loss [Type]", Timestep == 3) %>% 
  group_by(Timestep) %>% 
  summarise(Amount = sum(Amount)) %>% pull(Amount)

SSIM_targets_selected_h_per_10y_final %>% 
  add_row(SecondaryStratumID = NA, 
          TransitionGroupID = "Reforestation [Type]", 
          Timestep = 3,
          Amount = urb_total) %>% 
  mutate(StratumID = "Monteregie") -> SSIM_targets_selected_h_per_10y_final_with_ref

write.csv(SSIM_targets_selected_h_per_10y_final_with_ref, 
          "config/stsim/TransitionTarget_10y_ref.csv", 
          row.names=F)

SSIM_targets_selected_h_per_10y_final %>% 
  add_row(SecondaryStratumID = NA, 
          TransitionGroupID = "Reforestation [Type]", 
          Timestep = 3,
          Amount = 0) %>% 
  mutate(StratumID = "Monteregie") -> SSIM_targets_selected_h_per_10y_final_with_ref_0

write.csv(SSIM_targets_selected_h_per_10y_final_with_ref_0, 
          "config/stsim/TransitionTarget_10y_ref_0.csv", 
          row.names=F)

SSIM_targets_selected_h_per_10y_final %>% 
  add_row(SecondaryStratumID = NA, 
          TransitionGroupID = "Reforestation [Type]", 
          Timestep = 0,
          Amount = 0) %>% 
  mutate(StratumID = "Monteregie") -> SSIM_targets_selected_h_per_10y_final_with_ref_0

write.csv(SSIM_targets_selected_h_per_10y_final_with_ref_0, 
          "config/stsim/TransitionTarget_10y_ref_0_historic.csv", 
          row.names=F)

# ## TEST
# non_equal <- data.frame()
# for(mun in unique(mun.sub.18.clean$MUS_NM_MUN)){
#   
#   submun <- subset(mun.sub.18.clean, MUS_NM_MUN == mun)
#   test <- mask(crop(lu.18.sub, submun), submun)
#   tab_list <- list(crosstab(test$lu.1990.18, test$lu.2000.18)*0.09,
#                    crosstab(test$lu.2000.18, test$lu.2010.18)*0.09)
#   
#   for (ts in 1:2){
#       
#       subtargets <- SSIM_targets_selected_h_per_10y_final %>% 
#         filter(Timestep == ts) %>% 
#         filter(SecondaryStratumID == mun) #%>% 
#         #filter(TransitionGroupID == trans)
#       subtab <- tab_list[[ts]]
#       
#       # print(c(subtab[4,2], subtab[2,3], subtab[4,3]))
#       # print(subtargets$Amount)
#       test_e <- (c(subtab[4,2], subtab[2,3], subtab[4,3]) == subtargets$Amount)
#       #print(test_e)
#       if (any(!test_e)){
#         print(subtargets)
#         print(subtab)
#         print(test_e)
#         print(c(subtab[4,2], subtab[2,3], subtab[4,3]))
#         print(subtargets$Amount)
#         print(mun)
#         non_equal <- rbind(non_equal, subtargets)
#         plot(test)
#         readline()
#       }
#   }
#   # 3,1  1,2  3,2
# }

#plot(test==2)
#freq(test$lu.1990.18)
#freq(test$lu.2010.18)

#-------------------------------------------------------------------------------
# ## PROCESSING OF BRONWYN'S TARGETS
# mun.sub.18.clean <- st_read("data/mun/munic_SHP_clean.shp", quiet=TRUE)
# BTSL_stc_targets <- read_csv("data/landis/config/TransitionTargetsSTconnectBTSL.csv")
# old_targets <- read_csv("config/stsim/TransitionTarget_10y.csv") %>%
#   pull(TransitionGroupID) %>% unique()
# head(BTSL_stc_targets)
# 
# # simple function to assess overlap between two vectors
# source("scripts/functions/get_overlap.R")
# 
# overlap <- get_overlap(x = mun.sub.18.clean$MUS_NM_MRC,
#                        y = BTSL_stc_targets$SecondaryStratumID, TRUE, "fuzzy")
# 
# get_overlap(mun.sub.18.clean$MUS_NM_MRC,
#             BTSL_stc_targets[overlap$match_ind$y,]$SecondaryStratumID, TRUE, "fuzzy")
# 
# # write out only monteregie
# BTSL_stc_targets_mont <- BTSL_stc_targets[overlap$match_ind$y,] %>%
#   rename(old_TransitionGroupID = TransitionGroupID)
# 
# # Make looup table
# new_TransitionGroupID = c("Agricultural Loss [Type]", "Agricultural Expansion [Type]",
#                           "Deforestation [Type]", "Treed Wetland Loss to Ag [Type]",
#                           "Treed Wetland Loss to Urb [Type]","Open Wetland Loss to Ag [Type]",
#                           "Open Wetland Loss to Urb [Type]")
# lookup <- tibble(old_TransitionGroupID = sort(unique(BTSL_stc_targets_mont$old_TransitionGroupID)),
#                  TransitionGroupID = new_TransitionGroupID)
# BTSL_stc_targets_mont_mod <- BTSL_stc_targets_mont %>%
#   left_join(lookup, by = "old_TransitionGroupID") %>% 
#   select(-old_TransitionGroupID)

#-------------------------------------------------------------------------------
  
