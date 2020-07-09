library(tidyverse)
surf <- read_csv("surf/surf_output.csv")
print(head(surf))

surf %>% 
  mutate(scenario = as.factor(scenario)) %>% 
  ggplot(aes(x = timestep, y = kp_nb, color=scenario)) +
  geom_smooth(aes(group = scenario), alpha=0.1, method="loess")+
  geom_point(aes(group=scenario))+
  facet_wrap(~species, scales = "free")
