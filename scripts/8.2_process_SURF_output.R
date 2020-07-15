library(tidyverse)
library(ggradar)

surf <- read_csv("surf/surf_output.csv") %>% 
  mutate(timestep = timestep*10+1990)
print(head(surf))

sce_code_vec <- 
  as.vector(t(outer(c("BAU-", "Ref-", "CorrProtec-", "Ref-CorrProtec-", "Ref(Targ)-CorrProtecc-"), 
                    c("Hist", "Base", "RCP8"), paste0)))

results <- read_rds("test/july/10itercloud1/stsim_run_results.RDS") %>% 
  dplyr::select(scenarioId, name)
results$name <- gsub(results$name, pattern = " \\(.+\\)", replacement = "")
results$sce <- paste0("sce_",results$scenarioId)
results$code <- c("Control", sce_code_vec)

results_clean <- results %>% tibble() %>% 
  mutate(splitted = str_split(name, " \\| ")) %>% 
  mutate(climate = unlist(map(splitted, ~unlist(.x[2]))), 
         run = unlist(map(splitted, ~unlist(.x[1])))) %>% 
  dplyr::select(-splitted) 

joined <- left_join(surf, results_clean, by = c("scenario"="scenarioId")) %>% 
  replace_na(list(climate = "none", run = "historic run")) %>% 
  mutate(sce = as.factor(sce), 
         run = as.factor(run),
         climate = factor(climate, levels = c("none", 
                                              "historic",
                                              "baseline", "RCP 8.5")),
         run = factor(run, levels = c("historic run",
                                      "BAU run", 
                                      "BAU run + corrs protection",
                                      "BAU run + ref", 
                                      "BAU run + corrs protection + ref",
                                      "BAU run + corrs protection + ref TARGETED"
         ))
  )

theme_set(theme_minimal())
theme_update(legend.position = c(0.85, 0.22),
             #legend.justification = c(1, -0.2), # for inside plot
             #legend.position = "none",
             legend.box = "vertical",
             legend.background = element_blank(),
             legend.box.background = element_rect(colour = "black"),
             panel.border = element_rect(fill = NA),
             legend.title = element_text(size = 15),
             legend.text = element_text(size = 12),
             plot.title = element_text(size=22),
             plot.subtitle = element_text(size=22),
             axis.text.x = element_text(angle=65, vjust=0.6, size =15),
             axis.text.y = element_text(size =15),
             strip.text.x = element_text(size = 15),
             strip.text.y = element_text(size = 15),
             axis.title=element_text(size=18))

joined %>% 
  mutate(scenario = as.factor(scenario)) %>% 
  #filter(scenario == "52") %>% 
  filter(climate != "none") %>% 
  
  group_by(scenario, species, iter) %>% 
  group_modify(~mutate(.x , baseline = subset(.x, timestep == 2010)$kp_nb, 
                       the_diff = ((kp_nb-baseline)/baseline)*100 )) %>% ungroup %>% 
  
  ggplot(aes(x = timestep, y = the_diff, color=climate)) +
  geom_smooth(aes(group = scenario, linetype=run), alpha=0.2, method="loess")+
  geom_point(aes(group=scenario))+
  #scale_y_log10()+
  facet_wrap(~species, scales = "free")+
  
  scale_color_manual(values=c('#ffe300', 
                              '#ff7a14', 
                              "#b31400"), 
                     labels = c("Historic", 
                                "Baseline", 
                                "RCP 8.5"))

joined %>% 
  filter(sce != "sce_0", name != "historic run") %>% 
  group_by(timestep, species, code) %>% 
  summarise(kp_nb = mean(kp_nb)) %>% ungroup %>% 
  pivot_wider(names_from = timestep, values_from = kp_nb) %>% 
  mutate(diff = ((`2100`-`2010`)/`2010`)*100) %>% 
  select(-c(`2010`:`2100`)) %>%  
  pivot_wider(names_from = code, values_from = diff)-> radar_data

radar_data_2 <- radar_data %>% rename(group = species)
radar_2 <- ggradar(radar_data_2, centre.y = -60, legend.position = "right",
                   grid.min = -40, grid.max = 5, grid.mid = 0, 
                   values.radar = c("-40 %", "0 %", ""), 
                   gridline.label.offset	=5, legend.text.size= 12,
                   group.line.width =0.8, 
                   background.circle.colour	= "#ffffff", group.point.size	=5)
radar_2


radar <- ggRadar(data=radar_data_2,
                 aes(group=group),
                 interactive=FALSE,
                 rescale=FALSE, 
                 size	= 1, 
                 alpha =0.05, 
                 legend.position="right") +
  # Wrap the label text to fit plot area
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5)) +
  # Make x axis labels smaller to fit area and blank out y axis as the circles are intuitive enough
  theme(axis.text.x = element_text(size = 10, angle=0),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10), 
        axis.title.x=element_text(size = 10), 
        axis.title.y=element_text(size = 10), 
        axis.ticks=element_line(size = 0.3),
        panel.spacing = unit(10, "lines"))+
  labs(y = "Connectivity Loss")
ggiraph(code = print(radar))
#axis.text.y=element_blank(),
#axis.ticks.y = element_blank()
#panel.border = element_rect(colour = "white"), 