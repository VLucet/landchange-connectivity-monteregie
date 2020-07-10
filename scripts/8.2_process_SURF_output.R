library(tidyverse)
surf <- read_csv("surf/surf_output.csv")
print(head(surf))

surf %>% 
  mutate(scenario = as.factor(scenario)) %>% 
  ggplot(aes(x = timestep, y = kp_nb, color=scenario)) +
  geom_smooth(aes(group = scenario), alpha=0.5, method="loess")+
  geom_point(aes(group=scenario))+
  scale_y_log10()+
  facet_wrap(~species, scales = "free")

surf %>% 
  mutate(scenario = as.factor(scenario)) %>% 
  group_by(scenario, timestep, species) %>% 
  summarise(kp_nb = mean(kp_nb)) %>% ungroup %>% 
  ggplot(aes(x = timestep, y = kp_nb, color=scenario)) +
  geom_smooth(aes(group = scenario), se = F, method="loess")+
  geom_point(aes(group=scenario))+
  facet_wrap(~species, scales = "free")

