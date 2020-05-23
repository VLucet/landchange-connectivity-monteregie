#-------------------------------------------------------------------------------
## 1.7 Prep Landis multipliers
## 2020
## Inputs: Landis outputs
## Outputs: Updates multiplier base
#-------------------------------------------------------------------------------

# Remove all
rm(list=ls())
set.seed(77)

suppressPackageStartupMessages({
  library(tidyverse)
  library(assertthat)
})

#-------------------------------------------------------------------------------
forest_classes <- read_csv("config/rcl_tables/land_use/recode_table_forest.csv")
all_cases <- expand.grid((forest_classes$Name), (forest_classes$Name)) %>% 
  filter(Var1 != Var2) %>% 
  mutate(TransitionGroupID = paste0(Var1, "_to_", Var2, " [Type]"))

transMultRaw <- read_csv("data/landis/config/landis_baseline_age_transitions.csv")
# transMultRaw %>% mutate(before=paste0(from_cover,age_before), after=paste0(to_cover,age_after)) %>%  
#   filter(!(before==after)) %>% dim()
initYear <- 0

transMult_baseline <- 
  mutate(transMultRaw, srcClassID = (from_cover*10 + age_before), 
         destClassID = (to_cover * 10 + age_after)) %>% #glimpse()
  left_join(forest_classes, by=c("srcClassID"="ID")) %>% #glimpse()
  left_join(forest_classes, by=c("destClassID"="ID")) %>% #glimpse()
  filter(Name.x != Name.y) %>%
  mutate(TransitionGroupID = paste0(Name.x, "_to_", Name.y, " [Type]"),
         Timestep =  (from_time + initYear)/10, # 10 years timestep but integer
         TertiaryStratumID = land_type, 
         probability_mod = ifelse(probability == 1, 0.9999, probability),
         numTimesteps = to_time - from_time, 
         Amount = 1 - (1-probability_mod)^(1/(numTimesteps/10))) %>% # 10 years timestep
  dplyr::select(-c(names(transMultRaw), probability_mod, numTimesteps, Name.y, Name.x))


all_cases_expanded <- transMult_baseline %>% 
  dplyr::select(Timestep, TertiaryStratumID, TransitionGroupID) %>% #glimpse() %>% 
  mutate(TransitionGroupID = factor(TransitionGroupID, 
                                    levels=as.factor(all_cases$TransitionGroupID))) %>% #glimpse() %>% 
  expand(Timestep, TertiaryStratumID, TransitionGroupID) #%>% glimpse()

### -----
all_cases_expanded %>% dplyr::select(TransitionGroupID) %>% 
  unique() %>% 
  mutate(splitted = str_split(TransitionGroupID, "_to_" )) %>% 
  mutate(left = unlist(map(splitted, ~.x[1])), 
         right = unlist(map(splitted, ~.x[2]))) %>% 
  mutate(right = unlist(map(right, ~str_replace(.x, pattern = " \\[Type\\]", 
                                                replacement = "")))) %>% 
  left_join(forest_classes, by = c("left"="Name")) %>% rename(srcClassID=ID) %>% 
  left_join(forest_classes, by = c('right'='Name')) %>% rename(destClassID=ID) %>% 
  dplyr::select(-splitted, -left, -right) -> the_key
### -----

transMult_baseline_all <- transMult_baseline %>% 
  right_join(all_cases_expanded, by = c("TransitionGroupID", "Timestep", "TertiaryStratumID")) %>%
  dplyr::select(-c(srcClassID,destClassID)) %>% left_join(the_key) %>% 
  mutate(Amount=replace_na(Amount,0))

write_csv(transMult_baseline_all, 
          "config/stsim/temp_TransitionMultiplierValue_baseline.csv")
#-------------------------------------------------------------------------------

transMultRaw <- read_csv("data/landis/config/landis_rcp45_age_transitions.csv")
# transMultRaw %>% mutate(before=paste0(from_cover,age_before), after=paste0(to_cover,age_after)) %>%  
#   filter(!(before==after)) %>% dim()
initYear <- 0

transMult_4.5 <- 
  mutate(transMultRaw, srcClassID = (from_cover*10 + age_before), 
         destClassID = (to_cover * 10 + age_after)) %>% #glimpse()
  left_join(forest_classes, by=c("srcClassID"="ID")) %>% #glimpse()
  left_join(forest_classes, by=c("destClassID"="ID")) %>% #glimpse()
  filter(Name.x != Name.y) %>%
  mutate(TransitionGroupID = paste0(Name.x, "_to_", Name.y, " [Type]"),
         Timestep =  (from_time + initYear)/10, # 10 years timestep but integer
         TertiaryStratumID = land_type, 
         probability_mod = ifelse(probability == 1, 0.9999, probability),
         numTimesteps = to_time - from_time, 
         Amount = 1 - (1-probability_mod)^(1/(numTimesteps/10))) %>% # 10 years timestep
  dplyr::select(-c(names(transMultRaw), probability_mod, numTimesteps, Name.y, Name.x))

all_cases_expanded <- transMult_4.5 %>% 
  dplyr::select(Timestep, TertiaryStratumID, TransitionGroupID) %>% #glimpse() %>% 
  mutate(TransitionGroupID = factor(TransitionGroupID, 
                                    levels=as.factor(all_cases$TransitionGroupID))) %>% #glimpse() %>% 
  expand(Timestep, TertiaryStratumID, TransitionGroupID) #%>% glimpse()

transMult_4.5_all <- transMult_4.5 %>% 
  right_join(all_cases_expanded, by = c("TransitionGroupID", "Timestep", "TertiaryStratumID")) %>% 
  dplyr::select(-c(srcClassID,destClassID)) %>% left_join(the_key) %>%
  mutate(Amount=replace_na(Amount,0))

write_csv(transMult_baseline_all, 
          "config/stsim/temp_TransitionMultiplierValue_4.5.csv")

#-------------------------------------------------------------------------------

transMultRaw <- read_csv("data/landis/config/landis_rcp85_age_transitions.csv")
# transMultRaw %>% mutate(before=paste0(from_cover,age_before), after=paste0(to_cover,age_after)) %>%  
#   filter(!(before==after)) %>% dim()
initYear <- 0

transMult_8.5 <- 
  mutate(transMultRaw, srcClassID = (from_cover*10 + age_before), 
         destClassID = (to_cover * 10 + age_after)) %>% #glimpse()
  left_join(forest_classes, by=c("srcClassID"="ID")) %>% #glimpse()
  left_join(forest_classes, by=c("destClassID"="ID")) %>% #glimpse()
  filter(Name.x != Name.y) %>%
  mutate(TransitionGroupID = paste0(Name.x, "_to_", Name.y, " [Type]"),
         Timestep =  (from_time + initYear)/10, # 10 years timestep but integer
         TertiaryStratumID = land_type, 
         probability_mod = ifelse(probability == 1, 0.9999, probability),
         numTimesteps = to_time - from_time, 
         Amount = 1 - (1-probability_mod)^(1/(numTimesteps/10))) %>% # 10 years timestep
  dplyr::select(-c(names(transMultRaw), probability_mod, numTimesteps, Name.y, Name.x))


all_cases_expanded <- transMult_8.5 %>% 
  dplyr::select(Timestep, TertiaryStratumID, TransitionGroupID) %>% #glimpse() %>% 
  mutate(TransitionGroupID = factor(TransitionGroupID, 
                                    levels=as.factor(all_cases$TransitionGroupID))) %>% #glimpse() %>% 
  expand(Timestep, TertiaryStratumID, TransitionGroupID) #%>% glimpse()

transMult_8.5_all <- transMult_8.5 %>% 
  right_join(all_cases_expanded, by = c("TransitionGroupID", "Timestep", "TertiaryStratumID")) %>% 
  dplyr::select(-c(srcClassID,destClassID)) %>% left_join(the_key) %>%
  mutate(Amount=replace_na(Amount,0))
  
write_csv(transMult_8.5_all, 
          "config/stsim/temp_TransitionMultiplierValue_8.5.csv")
