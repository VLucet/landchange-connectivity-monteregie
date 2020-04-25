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
})

options(stringsAsFactors = FALSE)
#-------------------------------------------------------------------------------
classes <- read_csv("config/rcl_tables/land_use/recode_table.csv")

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
# CANT BE AUTOMATATIZED

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

# StateAttributeType
atr_type <- data.frame(
  Name = c("Agriculture", "Urban")
)
write_csv(atr_type, "config/stsim/StateAttributeType.csv")

#-------------------------------------------------------------------------------
# FROM:
# Transition
trans <- data.frame(
  StateClassIDSource = c("Agriculture:Cultivated", "Forest:Deciduous", "Forest:Deciduous"),
  StateClassIDDest = c("Urban:Nonlinear", "Agriculture:Cultivated", "Urban:Nonlinear"),
  TransitionTypeID = c("Agricultural Loss", "Agricultural Expansion", "Deforestation"),
  Probability = c(1, 1, 1)
)
write_csv(trans, "config/stsim/Transition.csv")

unchanging_states <- c("Roads:Linear")
all_state_classes <- c(unique(c(trans$StateClassIDSource, trans$StateClassIDDest)), 
                       unchanging_states)
state_label_x <- unlist(lapply(str_split(all_state_classes, ":"), first))
state_label_y <- unlist(lapply(str_split(all_state_classes, ":"), nth, n = 2))

classes_sub <- classes %>% 
  filter(new_class %in% state_label_x) %>% 
  group_by(new_class) %>% 
  summarise(ID=modal(new_code)) %>% 
  rename(StateLabelXID=new_class)

# State classes
state_classes <- data.frame(
  Name = all_state_classes, 
  StateLabelXID = state_label_x,
  StateLabelYID = state_label_y,
  Color = random_rgb(length(all_state_classes)),
  Legend = letters[1:length(all_state_classes)]
) %>% 
  left_join(classes_sub, by = "StateLabelXID")

write_csv(state_classes, "config/stsim/StateClass.csv")

# State X
state_x <- data.frame(
  Name = state_label_x,
  Description = state_label_x
)
write_csv(state_x, "config/stsim/StateLabelX.csv")

# State Y 
state_y <- data.frame(
  Name = state_label_y,
  Description = state_label_y
)
write_csv(state_y, "config/stsim/StateLabelY")

# DeterministicTransition
deter <- data.frame(
  StateClassIDSource = unique(state_classes$Name),
  Location = paste0("A", c(1,2,3,4))
)
write_csv(deter, "config/stsim/DeterministicTransition.csv")

# TransitionType
all_trans <- trans$TransitionTypeID
trans_type <- data.frame(
  Name = all_trans,
  ID = 1:length(all_trans),
  Color = random_rgb(3)
)
write_csv(trans_type, "config/stsim/TransitionType.csv")

# TransitionGroup
trans_group <- data.frame(
  Name = c("Urbanisation"),
  IsAuto = c("")
)
write_csv(trans_group, "config/stsim/TransitionGroup.csv")

# TransitionTypeGroup
trans_type_group <- data.frame(
  TransitionTypeID = c("Agricultural Expansion", "Agricultural Loss", 
                       "Agricultural Loss", "Deforestation", "Deforestation"),
  TransitionGroupID = c("Agricultural Expansion [Type]", "Agricultural Loss [Type]", 
                        "Urbanisation", "Deforestation [Type]", "Urbanisation"),
  IsAuto = c(TRUE, TRUE, NA, TRUE, NA)
  
)
write_csv(trans_type_group, "config/stsim/TransitionTypeGroup.csv")

# TransitionMultiplierValue
trans_mul_val <- data.frame(
  StratumID = c("Not_Monteregie", "Not_Monteregie", "Not_Monteregie", "", "", ""),
  TertiaryStratumID = c("", "", "", "PA", "PA", "PA"), 
  TransitionGroupID = c("Deforestation [Type]", "Agricultural Loss [Type]", 
                        "Agricultural Expansion [Type]", "Agricultural Expansion [Type]", 
                        "Agricultural Loss [Type]", "Deforestation [Type]"),
  Amount = c(0, 0, 0, 0, 0, 0)
)
write_csv(trans_mul_val, "config/stsim/TransitionMultiplierValue.csv")

#-------------------------------------------------------------------------------

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