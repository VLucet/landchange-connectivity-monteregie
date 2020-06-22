#-------------------------------------------------------------------------------
## 2.5 Prepare datasheets
## 2020
## Inputs: parameters
## Outputs: stsim datasheets
#-------------------------------------------------------------------------------

# Remove all
rm(list=ls())
set.seed(77)

# Load important libraries
suppressPackageStartupMessages({
  library(raster)
  library(tidyverse)
  library(assertthat)
})

options(stringsAsFactors = FALSE)
#-------------------------------------------------------------------------------
classes <- read_csv("config/rcl_tables/land_use/recode_table.csv")
forest_classes <- read_csv("config/rcl_tables/land_use/recode_table_forest.csv")

# RANDOM RGB GENERATOR
random_rgb <- function(n){
  vec <- c()
  for (i in 1:n){
    col <- paste(as.character(sample(1:255, 4)), collapse = ", ")
    vec <- c(vec, col)
  }
  vec
}

#-------------------------------------------------------------------------------

### TERNINOLOGY

# Terminology 
term <- data.frame(
  AmountLabel = "Area", 
  AmountUnits = "Hectares", 
  StateLabelX = "Classes", 
  StateLabelY = "Subclasses", 
  PrimaryStratumLabel = "Monteregie",
  SecondaryStratumLabel = "Municipality",
  TertiaryStratumLabel = "Zoning", 
  TimestepUnits = "Timestep"
)
write_csv(term, "config/stsim/Terminology.csv")

#-------------------------------------------------------------------------------
### THEN PROCESS BR's INTERNAL FOREST DYNAMICS
# Analyse land types
landtypes <- raster("data/stsim/aggregated/tertiary_stratum_land_types.tif")
mont_landtypes <- as.data.frame(freq(landtypes))
mont_landtypes <- subset(mont_landtypes, !(value %in% c(0,99,NA)))
unique_mont_landtypes <- unique(mont_landtypes$value)

# Load prepocessed scenarios
temp_baseline <- read_csv("config/stsim/temp_TransitionMultiplierValue_baseline.csv") %>% 
  filter(TertiaryStratumID %in% unique_mont_landtypes) %>% 
  mutate(TertiaryStratumID = paste0("lty_",TertiaryStratumID)) %>% 
  dplyr::select(-c(srcClassID, destClassID)) %>% 
  mutate(Timestep = Timestep+2)
assert_that(length(unique(temp_baseline$TransitionGroupID)) == 72)

new_levels_df <- temp_baseline %>% 
  mutate(TertiaryStratumID=factor(TertiaryStratumID)) %>% 
  mutate(TertiaryStratumID=factor(TertiaryStratumID, levels = c(levels(TertiaryStratumID), "Not_Forest", "Not_Monteregie"))) %>% 
  expand(TransitionGroupID,Timestep,TertiaryStratumID) %>% filter( TertiaryStratumID %in% c("Not_Forest", "Not_Monteregie")) %>% 
  mutate(Amount=0)

temp_baseline <- bind_rows(temp_baseline, new_levels_df)

temp_4.5 <- read_csv("config/stsim/temp_TransitionMultiplierValue_4.5.csv") %>% 
  filter(TertiaryStratumID %in% unique_mont_landtypes) %>% 
  mutate(TertiaryStratumID = paste0("lty_",TertiaryStratumID)) %>% 
  dplyr::select(-c(srcClassID, destClassID)) %>% 
  mutate(Timestep = Timestep+2) %>% 
  bind_rows(new_levels_df) 
assert_that(length(unique(temp_4.5$TransitionGroupID)) == 72)
temp_8.5 <- read_csv("config/stsim/temp_TransitionMultiplierValue_8.5.csv") %>% 
  filter(TertiaryStratumID %in% unique_mont_landtypes)%>% 
  mutate(TertiaryStratumID = paste0("lty_",TertiaryStratumID)) %>% 
  dplyr::select(-c(srcClassID, destClassID)) %>% 
  mutate(Timestep = Timestep+2) %>%
  bind_rows(new_levels_df)
assert_that(length(unique(temp_8.5$TransitionGroupID)) == 72)

#-------------------------------------------------------------------------------

# TransitionMultiplierValue
trans_mul_val <- data.frame(
  StratumID = c("Not_Monteregie", "Not_Monteregie", "Not_Monteregie", "Not_Monteregie",
                "PA", "PA", "PA", "PA"),
  TransitionGroupID = c("Deforestation [Type]", "Agricultural Loss [Type]", "Agricultural Expansion [Type]", "Reforestation [Type]", 
                        "Agricultural Expansion [Type]", "Agricultural Loss [Type]", "Deforestation [Type]", "Reforestation [Type]"),
  Amount = c(0, 0, 0, 0, 
             0, 0, 0, 0)
) 

trans_mul_val_baseline <- bind_rows(trans_mul_val, temp_baseline)
trans_mul_val_4.5 <- bind_rows(trans_mul_val, temp_4.5)
trans_mul_val_8.5 <- bind_rows(trans_mul_val, temp_8.5)

trans_mul_val_historic <- trans_mul_val_baseline
trans_mul_val_historic$Amount <- 0 
trans_mul_val_historic_sub <- subset(trans_mul_val_historic, Timestep==2)
trans_mul_val_historic_sub$Timestep <- 0
trans_mul_val_historic <- bind_rows(trans_mul_val_historic, trans_mul_val_historic_sub)

# TODO problems:
# Does not prevent forest change outside of the monteregie (should only allow aging?)

write_csv(trans_mul_val_baseline, "config/stsim/TransitionMultiplierValue_baseline.csv")
write_csv(trans_mul_val_4.5, "config/stsim/TransitionMultiplierValue_4.5.csv")
write_csv(trans_mul_val_8.5, "config/stsim/TransitionMultiplierValue_8.5.csv")
write_csv(trans_mul_val_historic, "config/stsim/TransitionMultiplierValue_historic.csv")

#-------------------------------------------------------------------------------
# FROM

# Transition

# Internal transitions reductions
trans_unique <- unique(temp_baseline$TransitionGroupID)
trans_unique_no_type <- gsub(trans_unique, pattern = " \\[Type\\]", replacement = "")
internal_states_unique <- unique(forest_classes$Name)

# Assembling all transitions
trans <- data.frame(
  StateClassIDSource = c("Agriculture:Cultivated", "Agriculture:Cultivated"),
  StateClassIDDest = c("Urban:Nonlinear", "Forest:decid_2"),
  TransitionTypeID = c("Agricultural Loss", "Reforestation"),
  TransitionGroupID = c("Urbanisation", "Reforestation Gr"),
  Probability = c(1, 1)
) %>% bind_rows(
  
  tibble(StateClassIDSource = rep(paste0("Forest:",internal_states_unique), 2),
         StateClassIDDest = c(rep("Agriculture:Cultivated", length(internal_states_unique)), 
                              rep("Urban:Nonlinear", length(internal_states_unique))),
         TransitionTypeID = c(rep("Agricultural Expansion", length(internal_states_unique)), 
                              rep("Deforestation", length(internal_states_unique))),
         TransitionGroupID = c(rep("Agricultural Expansion Gr", length(internal_states_unique)), 
                               rep("Urbanisation", length(internal_states_unique))), 
         Probability =1
  )
  
) %>% bind_rows(
  
  tibble(StateClassIDSource = paste0("Forest:", sapply(strsplit(trans_unique_no_type, "_to_"), `[[`, 1)), 
         StateClassIDDest = paste0("Forest:", sapply(strsplit(trans_unique_no_type, "_to_"), `[[`, 2)),
         TransitionTypeID = trans_unique_no_type,
         TransitionGroupID = "Forest Internals Gr", 
         Probability =1)
)

trans_to_write <- trans %>% 
  dplyr::select(-TransitionGroupID)

write_csv(trans_to_write, "config/stsim/Transition.csv")

unchanging_states <- c("Roads:Linear", "Water:All", "Wetlands:Both")

## AUTOMATE THE PROCESS

all_state_classes <- c(unique(c(trans$StateClassIDSource, trans$StateClassIDDest)), 
                       unchanging_states)
state_label_x <- unlist(lapply(str_split(all_state_classes, ":"), first))
state_label_y <- unlist(lapply(str_split(all_state_classes, ":"), nth, n = 2))

classes_sub <- classes %>% 
  filter(new_class %in% state_label_x) %>% 
  filter(new_code != 3) %>% 
  group_by(new_class) %>% 
  summarise(ID=modal(new_code)) %>% 
  rename(StateLabelXID=new_class)

forest_classes_sub <- forest_classes %>% 
  rename(StateLabelYID=Name)

# State classes
state_classes <- data.frame(
  Name = all_state_classes, 
  StateLabelXID = state_label_x,
  StateLabelYID = state_label_y,
  Color = random_rgb(length(all_state_classes)),
  Legend = letters[1:length(all_state_classes)]
) %>% 
  left_join(classes_sub, by = "StateLabelXID") %>% 
  left_join(forest_classes_sub, by=c("StateLabelYID")) %>% 
  rename(ID="ID.x")
state_classes$ID[!is.na(state_classes$ID.y)] <- state_classes$ID.y[!is.na(state_classes$ID.y)]
state_classes <- dplyr::select(state_classes, -ID.y) 

write_csv(state_classes, "config/stsim/StateClass.csv")

# State X AND Y 
state_x <- data.frame(
  Name = state_label_x,
  Description = state_label_x
) %>% 
  unique()
write_csv(state_x, "config/stsim/StateLabelX.csv")
state_y <- data.frame(
  Name = state_label_y,
  Description = state_label_y
) %>% 
  unique()
write_csv(state_y, "config/stsim/StateLabelY.csv")

# DeterministicTransition # Location does not matter
deter <- data.frame(
  StateClassIDSource = unique(state_classes$Name),
  Location = paste0("A", c(1:length(unique(state_classes$Name))))
)
write_csv(deter, "config/stsim/DeterministicTransition.csv")

# TransitionType
all_trans <- unique(trans$TransitionTypeID)
trans_type <- data.frame(
  Name = all_trans,
  ID = 1:length(all_trans), # we dont care for this
  Color = random_rgb(length(all_trans))
)
write_csv(trans_type, "config/stsim/TransitionType.csv")

# TransitionGroup
all_groups <- unique(trans$TransitionGroupID)
trans_group <- data.frame(
  Name = all_groups,
  IsAuto = rep("", length(all_groups))
)
write_csv(trans_group, "config/stsim/TransitionGroup.csv")

# TransitionTypeGroup
trans_sub_type <- trans %>% 
  dplyr::select(TransitionTypeID, TransitionGroupID) %>% 
  mutate(TransitionGroupID = paste(TransitionTypeID, "[Type]")) %>% 
  mutate(IsAuto = TRUE) %>% 
  unique()
trans_sub_group <- trans %>% 
  dplyr::select(TransitionTypeID, TransitionGroupID) %>% 
  filter(!is.na(TransitionGroupID)) %>% 
  mutate(IsAuto = NA) %>% 
  unique()
trans_type_group <- bind_rows(trans_sub_type, trans_sub_group) %>% 
  arrange(TransitionTypeID)
write_csv(trans_type_group, "config/stsim/TransitionTypeGroup.csv")

#-------------------------------------------------------------------------------
# CANT BE AUTOMATIZED but downstream
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# ADJACENCY

# StateAttributeType
atr_type <- data.frame(
  Name = c("Agriculture", "Urban", "Forest", internal_states_unique)
)
write_csv(atr_type, "config/stsim/StateAttributeType.csv")

# StateAttributeValue
atr_val <- data.frame(
  StateClassID = c("Agriculture:Cultivated", "Urban:Nonlinear"),
  StateAttributeTypeID = c("Agriculture", "Urban"),
  Value = c(1, 1)
) %>% bind_rows(
  
  data.frame(StateClassID=rep(paste0("Forest:", internal_states_unique),2), 
             StateAttributeTypeID=c(rep("Forest", length(internal_states_unique)), internal_states_unique), 
             Value=1)
  
)
write_csv(atr_val, "config/stsim/StateAttributeValue.csv")

# TransitionAdjacencySetting

right_side <- sapply(strsplit(trans_unique_no_type, "_to_"), `[[`, 2)

adj_settings <- data.frame(
  TransitionGroupID = c("Urbanisation", "Agricultural Expansion Gr", "Reforestation Gr") ,
  StateAttributeTypeID = c("Urban", "Agriculture", "Forest"),
  NeighborhoodRadius = c(500, 250, 250), # 1500 #200
  UpdateFrequency = c(1)
  
) %>% bind_rows(
  
  data.frame(TransitionGroupID = trans_unique,
             StateAttributeTypeID = right_side,
             NeighborhoodRadius = 225, # 1500
             UpdateFrequency = c(1))
  
)
write_csv(adj_settings, "config/stsim/TransitionAdjacencySetting.csv")

# TransitionAdjacencyMultiplier
adj_mul <- data.frame(
  
  TransitionGroupID = c("Urbanisation", "Urbanisation",
                        "Agricultural Expansion Gr", "Agricultural Expansion Gr",
                        "Reforestation Gr", "Reforestation Gr"),
  AttributeValue = c(0.000, 0.7, 
                     0.000, 0.9, 
                     0.000, 0.9),
  Amount = c(0,1,
             0,1,
             0,1)
  
) %>% bind_rows(
  
  data.frame(TransitionGroupID = rep(trans_unique, each=2), 
             AttributeValue = c(0.000,0.15), 
             Amount = c(0,1))
  
)
write_csv(adj_mul, "config/stsim/TransitionAdjacencyMultiplier.csv")

#-------------------------------------------------------------------------------

## Time since Transition
TST_group <- data.frame(
  TransitionTypeID = trans_unique_no_type,
  TransitionGroupID = "Forest Internals Gr"
)
write_csv(TST_group, "config/stsim/TimeSinceTransitionGroup.csv")

TST_random <- data.frame(
  TransitionGroupID = "Forest Internals Gr",
  MinInitialTST = 2 
)
write_csv(TST_random, "config/stsim/TimeSinceTransitionRandomize.csv")

## Pathway autocorelation
TPA <- data.frame(
  TransitionGroupID = "Forest Internals Gr",
  AutoCorrelation ="Yes", 
  SpreadTo = "Same Tertiary Stratum"
)
write_csv(TPA, "config/stsim/TransitionPathwayAutoCorrelation.csv")

#-------------------------------------------------------------------------------

# trans <- data.frame(
#   StateClassIDSource = c("Agriculture:Cultivated", "Forest:Deciduous", "Forest:Deciduous", "Agriculture:Cultivated"),
#   StateClassIDDest = c("Urban:Nonlinear", "Agriculture:Cultivated", "Urban:Nonlinear", "Forest:Deciduous"),
#   TransitionTypeID = c("Agricultural Loss", "Agricultural Expansion", "Deforestation", "Reforestation"),
#   TransitionGroupID = c("Urbanisation", "Agricultural Expansion Gr", "Urbanisation", "Reforestation"),
#   Probability = c(1, 1, 1, 1)
# )
# state_classes <- data.frame(
#   Name = c("Agriculture:Cultivated", "Forest:Deciduous", 
#            "Urban:Nonlinear", "Roads:Linear"),
#   StateLabelXID = c("Agriculture", "Forest", "Urban", "Roads"),
#   StateLabelYID = c("Cultivated", "Deciduous", "Nonlinear", "Linear"),
#   ID = c(1,3,2,4),
#   Color = c("255,255,255,0", "255,0,255,0", "255,255,0,0", "0,255,255,0"),
#   Legend = c("a", "b", "c", "d")
# )
# state_x <- data.frame(
#   Name = c("Forest", "Urban", "Agriculture", "Roads"),
#   Description = c("Forest", "Urban", "Agriculture", "Roads")
# )
# state_y <- data_frame(
#   Name = c("Deciduous", "Cultivated", "Nonlinear", "Linear"),
#   Description = c("Deciduous", "Cultivated", "Nonlinear", "Linear")
# )
# trans_type <- data.frame(
#   Name = c("Deforestation", "Agricultural Loss", "Agricultural Expansion"),
#   ID = c(1, 2, 3),
#   Color = c("255,255,255,0", "255,0,255,0", "255,255,0,0")
# )
# trans_type_group <- data.frame(
#   TransitionTypeID = c("Agricultural Expansion", "Agricultural Loss", 
#                        "Agricultural Loss", "Deforestation", "Deforestation"),
#   TransitionGroupID = c("Agricultural Expansion [Type]", "Agricultural Loss [Type]", 
#                         "Urbanisation", "Deforestation [Type]", "Urbanisation"),
#   IsAuto = c(TRUE, TRUE, NA, TRUE, NA)
# )