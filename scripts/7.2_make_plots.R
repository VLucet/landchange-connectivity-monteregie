#-------------------------------------------------------------------------------
## 7.2 Make plots
## 2020
## Inputs: post-processed data
## Outputs: plots
#-------------------------------------------------------------------------------

rm(list = ls())
set.seed(77)

#-------------------------------------------------------------------------------

##-- for shorter tests
#STSIM_ITER=2
#STSIM_TS_END=2
##--

# Load important libraries
suppressPackageStartupMessages({
  library(raster)
  library(sf)
  library(tidyverse)
  library(ggplot2)
  library(gganimate)
  library(transformr)
  library(animation)
  library(rasterVis)
  library(ggmap)
  library(gifski)
  library(gtools)
  library(fmsb)
  library(RColorBrewer)
  library(scales)
  library(ggradar)
})

#-------------------------------------------------------------------------------
# Get result scenario directory 
sce_dir_vec <- list.files("libraries/stsim/monteregie-conncons-scripted.ssim.output", 
                          full.names = T)
sce_nb_vec <- paste0("sce_", as.numeric(unlist(lapply(str_split(sce_dir_vec, "-"), 
                                                      FUN = last))))
mun <- st_read("data/mun/munic_SHP_clean.shp", quiet = TRUE)

results <- read_rds("data/temp/stsim_run_results.RDS") %>% 
  dplyr::select(scenarioId, name)
results$name <- gsub(results$name, pattern = " \\(.+\\)", replacement = "")
results$sce <- paste0("sce_",results$scenarioId)

results_clean <- results %>% tibble() %>% 
  mutate(splitted = str_split(name, " \\| ")) %>% 
  mutate(climate = unlist(map(splitted, ~unlist(.x[2]))), 
         run = unlist(map(splitted, ~unlist(.x[1])))) %>% 
  dplyr::select(-splitted) 
#-------------------------------------------------------------------------------

# Data prep
# listofiles <- list.files("it1_lasterun/it1_MAAM_Current/", full.names = T)
df_final <- readRDS("outputs/final/final_df_current_density.RDS") %>%
  mutate(timestep = (timestep*10)+1990, source = "model")
df_final_origin <- readRDS("outputs/final/final_df_origin_current_density.RDS") %>%
  mutate(timestep = timestep*10+1990, source = "model")

# df_final <- readRDS("test/final_df_current_density.RDS") %>%
#   mutate(timestep = (timestep*10)+1990, source = "model")
# df_final_origin <- readRDS("test/final_df_origin_current_density.RDS") %>%
#   mutate(timestep = timestep*10+1980, source = "model")

# Summarised => drops the zone
df_summarised <- df_final %>%
  group_by(sce, timestep, species, iteration) %>% 
  summarise(sum_cur = sum(current)) %>% ungroup %>%
  mutate(source = "model")

df_origin_summarised <- df_final_origin %>%
  group_by(timestep, species) %>% 
  summarise(sum_cur = sum(mean)) %>% ungroup %>% 
  mutate(sce = "sce_0", source = "observation")

joined <- full_join(df_summarised, 
                    df_origin_summarised, 
                    by=c("sce","source", "species", 
                         "timestep", "sum_cur")) %>% 
  left_join(results_clean, by = "sce") %>% 
  replace_na(list(climate = "none", run = "historic run")) %>% 
  mutate(sce = as.factor(sce), 
         run = as.factor(run), 
         sum_cur = 10*(sum_cur),
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

# Set theme
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

#-------------------------------------------------------------------------------

## FIGURE 1 => smoothed trajectories

fig_1_historic <- joined %>% 
  
  #filter(source=="observation") %>% 
  
  filter(climate == "none") %>% 
  
  ggplot(aes(x=timestep, y=sum_cur, col=source)) +
  scale_color_manual(values=c('#d8b365','#5ab4ac'), 
                     labels = c("Model", "Observation")) +
  
  geom_smooth(aes(group = sce), method = "glm", se=FALSE) +
  scale_linetype_manual(values = c(1,3,2,5))+
  
  geom_point(aes(group = sce)) +
  facet_wrap(~species, scales = "free") +
  
  labs(title = "Connectivity change for species through time",
       subtitle = "1990-2010",
       y = "Cummulative Connectivity",
       x = "Year",
       col = "Source") +
  NULL
fig_1_historic
ggsave(fig_1_historic, 
       filename = "outputs/figures/connectivity_decrease_x5species_historic.png", 
       width = 17, height = 12)

fig_1_static <- joined %>% 
  
  #filter(species %in% c("BLBR","URAM")) %>% 
  #filter(sce %in% c("sce_15", "sce_16", "sce_0")) %>%
  #filter(iteration =="it_1_") %>% 
  #filter(climate %in% c("historic", "baseline")) %>% 
  #filter(run %in% c("BAU run", "BAU run + ref")) %>% 
  #filter(species == "MAAM") %>% 
  #filter(run == "BAU run") %>% 
  
  filter(climate != "none") %>% 
  
  ggplot(aes(x=timestep, y=sum_cur, col=climate)) +
  scale_color_manual(values=c('#ffe300', 
                              '#ff7a14', 
                              "#b31400"), 
                     labels = c("Historic", 
                                "Baseline", 
                                "RCP 8.5"
                     )) +
  
  geom_smooth(aes(group = sce, linetype=run), alpha=0.2, method="loess") +
  scale_linetype_discrete() +
  #geom_point(aes(group = sce, pch = run)) +
  facet_wrap(~species, scales = "free") +
  
  #facet_grid(sce~species, scales = "fixed") +
  #add_phylopic(bear, alpha = 1, x=2010, y =0.11, ysize = 10) +
  
  labs(title = "Connectivity change for species through time",
       #subtitle = "1990-2100",
       #subtitle = "Year:{frame_along}",
       y = "Cummulative Connectivity",
       x = "Year",
       col = "Climate",
       linetype = "Run") +
  NULL
fig_1_static

ggsave(fig_1_static, 
       filename = "outputs/figures/connectivity_decrease_x5species.png", 
       width = 17, height = 12)

#-------------------------------------------------------------------------------

## FIGURE 2 => maps, needs better color scheme 

key <- read_csv("config/stsim/SecondaryStratum.csv") %>%
  mutate(ID = as.factor(ID)) %>%
  rename(zone=ID, MUS_NM_MUN=Name) %>%
  left_join(df_final, by="zone") %>%
  filter(MUS_NM_MUN!="Not Monteregie")

#mun_joined <- left_join(mun, key, by="MUS_NM_MUN")

df_final_fordiff_pivoted <- df_final %>% 
  group_by(zone, sce, species, timestep, source) %>% 
  summarise(current = mean(current)) %>% ungroup %>% 
  filter(timestep %in% c(2010, last(unique(key$timestep)))) %>% 
  pivot_wider(names_from=timestep, values_from=current) %>% drop_na() %>% 
  rename(before=last_col(offset = 1), after=last_col()) %>% 
  left_join(key, by=c("zone", "sce", "species", "source")) %>% 
  filter(MUS_NM_MUN!="Not Monteregie") %>% 
  mutate(change=((after-before)/before)*100) %>% 
  left_join(mun, by="MUS_NM_MUN") %>% 
  st_as_sf()

df_final_fordiff_pivoted <- 
  st_transform(df_final_fordiff_pivoted, crs=4326)

change <- ggplot() +
  geom_sf(data=df_final_fordiff_pivoted,
          aes(fill=change),
          show.legend=T, lwd = 0)  + 
  facet_wrap(~sce) + 
  ggtitle("Connectivity change in %", 
          subtitle = "2010-2100") +
  scale_fill_binned(low='#d13e11', high='#fff7bc', 
                    breaks=c(-50, -40, -30, -20, -10, 0, 10))
change
ggsave("outputs/figures/connectivit_change_mun.png", change)

#-------------------------------------------------------------------------------

## FUGURE 3 => radar charts

joined %>% 
  filter(sce != "sce_0", name != "historic run") %>% 
  group_by(timestep, species, sce) %>% 
  summarise(sum_cur = mean(sum_cur)) %>% ungroup %>% 
  pivot_wider(names_from = timestep, values_from = sum_cur) %>% 
  mutate(diff = ((`2100`-`2010`)/`2010`)*100) %>% 
  select(-c(`2010`:`2100`)) %>%  
  pivot_wider(names_from = sce, values_from = diff)-> radar_data

radar_data_nosp <- radar_data[,2:16]
rownames(radar_data_nosp) <- radar_data$species

min <- as.data.frame(t(rep(-20, ncol(radar_data_nosp))))
colnames(min) <- colnames(radar_data_nosp)

max <- as.data.frame(t(rep(1, ncol(radar_data_nosp))))
colnames(max) <- colnames(radar_data_nosp)

coul <- brewer.pal(5, "Dark2")
colors_border <- coul
colors_in <- alpha(coul,0.05)

radar <- bind_rows(max, min, radar_data_nosp)
radar_sorted <- radar %>% 
  select(gtools::mixedsort(names(radar), decreasing = T))
fmsb::radarchart(radar_sorted, pcol=colors_border , pfcol=colors_in,
                 axistype=0, plwd=2, plty=1, maxmin = T, centerzero=T, 
                 cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, 
                 vlcex=0.8, caxislabels = c("m", "b"))

legend(x=1.3, y=1.2, legend = radar_data$species, bty = "n", pch=20 , 
       col=colors_border, cex=1.1, pt.cex=2)

radar_data_2 <- radar_data %>% rename(group = species)
ggradar(radar_data_2, centre.y = -20, legend.position = "right",
        grid.min = -20, grid.max = 2, grid.mid = 0, 
        values.radar = c("-20 %", "0 %", "+2 %"), 
        gridline.label.offset	=5, legend.text.size= 12,
        background.circle.colour	= "#ffffff", group.point.size	=3)



#-------------------------------------------------------------------------------
stop("Reviewed so far")
#-------------------------------------------------------------------------------

## FIGURE 3
it_1 <- lapply(X = file.path(sce_dir_vec, "stsim_OutputSpatialState"), 
               FUN = list.files,  pattern="it1", full.names=TRUE)
# it_1 <- list.files("test/june/cloud/",
#                    pattern="it1", full.names = T)
it_1 <- lapply(X = it_1, FUN = mixedsort)
#it_1 <- mixedsort(unlist(it_1))

list_lu <-  map(.x = map_depth(.x = it_1, .f = raster, .depth = 2), 
                .f = stack)
#list_lu <- list_lu[2:length(list_lu)]
#names(list_lu) <- sce_nb_vec[2:length(sce_nb_vec)]

# for (x in 2:6) {print(freq((list_lu[[x]]$sc.it1.ts11==3) - (list_lu[[x]]$sc.it1.ts2==3)))}
list_lu_1_cropped <- map(map(list_lu, crop, mun), mask, mun)
extent_zoom <- extent(c(554687.5, 568838.3, 5024170, 5035009))
#extent_zoom <- drawExtent()
list_lu_1_cropped_2 <- map(list_lu, crop, extent_zoom)

# cs_cropped <- crop(it_1_CS, extent_zoom)

ts_template <- seq(from =0, by=10, length.out = nlayers(list_lu[[1]]))
ts_template_year <- ts_template+2010

for (sce in seq_len(length(list_lu))){
  
  #sce <- 1
  the_stack <- list_lu_1_cropped[[sce]]
  #the_stack <- crop(the_stack, extent_zoom)
  
  df_list <- list()
  for (ts in 1:nlayers(the_stack)){
    df <- as.data.frame(freq(the_stack[[ts]])) %>%
      filter(!is.na(value))
    df$count <- df$count/(ncell(the_stack[[1]])-2111516) # number of NA 2111516
    df$timestep <- ts_template[ts]
    df_list[[ts]] <- df
  }
  
  df_final <- bind_rows(df_list)
  
  names(df_final)[2] <- "proportion"
  df_final$timestep <- df_final$timestep+2010
  #df_final$value <- as.character(df_final$value)
  #df_final$value[as.numeric(df_final$value)>10] <- "3"
  # TODO joining?
  #df_final$value[df_final$value==1] <- "Agriculture"
  #df_final$value[df_final$value==2] <- "Urban"
  #df_final$value[df_final$value==3] <- "Forest"
  
  plot <- df_final %>% 
    #filter((value > 10)) %>% 
    mutate(value = as.factor(value)) %>% 
    group_by(value, timestep) %>% 
    summarise(proportion=sum(proportion)) %>% 
    ggplot()+
    aes(x=timestep, y=proportion, col=value, show.legend=F) +
    geom_line() +
    geom_point(size = 2) +
    # geom_segment(aes(xend = 2060, yend = proportion, col=value),
    #              linetype = 2, colour = 'grey') +
    #geom_segment(aes(xend = 2103, yend = proportion), linetype = 2)+
    geom_text(aes(x = 2105, label = value), hjust = 0, size=5, fontface="bold") +
    #scale_color_manual(values = c('#dfc27d', '#339933','#a6611a'))+
    geom_point(aes(group = seq_along(timestep))) +
    coord_cartesian(clip = 'off') +
    #transition_reveal(as.integer(timestep)) +
    labs(y="Proportion",
         x = "Year")+
    theme_minimal()+
    theme(legend.position = "none") +
    theme(plot.margin = margin(5.5, 60, 5.5, 5.5),
          axis.title=element_text(size=20, face="bold"),
          axis.text.x =element_text(size=15),
          axis.text.y =element_text(size=15))+
    NULL
  #ggsave(paste0("outputs/figures/",sce_nb_vec[sce],"_lu_change_animated.png"))
  #options(gganimate.dev_args = list(width = 900, height = 700))
  #plot_anim <- animate(plot, renderer = gifski_renderer())
  #anim_save(paste0("outputs/figures/sce_",sce_nb_vec[sce],"_lu_change_animated.gif"))
}
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

## Figure 4
# Raster gifs

# first declare function
make_plots_cs <- function(rasters, ts){
  
  the_max <- max(unlist(lapply(as.list(rasters), cellStats, max)))
  the_min <- min(unlist(lapply(as.list(rasters), cellStats, min)))
  
  for (idx in c(1:length(rasters))){
    plot<- rasterVis::levelplot(rasters[[idx]],
                                main=list(paste0('Year: ', ts[idx]), fontsize=25),
                                maxpixels= 10000000,
                                margin=list(draw=FALSE), 
                                at=seq(the_min, the_max, length.out=100))
    print(plot)
  }
}

for(sce in seq_len(length(list_lu))){
  it_CS <- stack(readRDS(paste0("outputs/final/final_cur_sum_sce_",sce+1,"_per_ts.RDS")))
  oldwd <- getwd()
  setwd("outputs/figures/")
  saveGIF(expr = make_plots_cs(as.list(it_CS), ts_template_year),
          interval=2, movie.name=paste0("sce_",sce,"_cs_animated.gif"),
          ani.width=800, ani.height=800, ani.res=100)
  setwd(oldwd)
}

#-------------------------------------------------------------------------------

## Figure 5

all_facetted <- ggplot(df_final) +
  aes(x = timestep, y = mean, group = zone, color=zone) +
  geom_line(show.legend = FALSE, alpha=0.2) +
  scale_color_viridis_d(option = "plasma") +
  facet_grid(sce ~ species)+
  theme(legend.position = "none")
ggsave("outputs/figures/final_graph.png", all_facetted)

#-------------------------------------------------------------------------------
library(tidymodels)
library(patchwork)

urb <- readRDS("test/fit/fit_rs_outcome_rf_urb_2")
agex <- readRDS("test/fit/fit_rs_outcome_rf_agex_2")

# urb_2 <- readRDS("test/fit/fit_rs_rf_urb_2")
# roc_curve(data = urb[[1]], .pred, truth = outcome_fact) %>% 
#   autoplot()
# roc_curve(data = urb[[2]], .pred, truth = outcome_fact) %>% 
#   autoplot(add=T)

p1 <- bind_rows(urb, .id = "fold") %>% 
  mutate(fold = factor(fold, levels = as.character(1:10))) %>% 
  group_by(fold) %>% 
  roc_curve(.pred, truth = outcome_fact) %>% 
  autoplot(add=T) +
  ggtitle(paste("Roc Curve for Urbanisation")) +
  annotate(x = 0.75, y = 0.25, geom="label", 
           label = as.character(paste("Av AUC = 0.938 +/- 0.002"))) +
  theme(legend.position = "none") +
  NULL

p2 <- bind_rows(agex, .id = "fold") %>% 
  mutate(fold = factor(fold, levels = as.character(1:10))) %>% 
  group_by(fold) %>% 
  roc_curve(.pred, truth = outcome_fact) %>% 
  autoplot(add=T) +
  ggtitle(paste("Roc Curve for Agricultural Expansion")) +
  annotate(x = 0.75, y = 0.25, geom="label", 
           label = as.character(paste("Av AUC = 0.929 +/- 0.002"))) +
  NULL

p1 + p2

#-------------------------------------------------------------------------------
# 
# ## FIGURE 3 (same than 2 but gif)
# mun_joined <- left_join(mun, key, by="MUS_NM_MUN")
# mun_joined$sum_cur <- (1/log(mun_joined$mean)*-1)*10
# plot <- ggplot(data=mun_joined) +
#   geom_sf(aes(fill=mean), show.legend=F) +
#   facet_grid(~sce) +
#   scale_fill_viridis_c(option = "inferno") +
#   labs(title= "Connectivity for all species - Year: {closest_state}")+
#   #transition_states(timestep) +
#   theme(plot.title = element_text(size = 35))+
#   theme(plot.caption = element_text(size=80, family="AvantGarde"))
# ggsave("outputs/figures/connectivit_change_mun.gif", plot)
# #plot_anim <- animate(plot, renderer = gifski_renderer())
# # throws error
# # anim_save(plot_anim, "outputs/figures/connectivit_change_mun_gif.gif")

#-------------------------------------------------------------------------------

### CALCULATION
# joined %>%
#   group_by(species, timestep, sce) %>%
#   filter(sce %in% c("sce_15", "sce_16")) %>% 
#   summarise(sum_cur = sum(sum_cur)) %>% ungroup() %>% 
#   pivot_wider(names_from=timestep, values_from=sum_cur) %>%
#   rename(before=last_col(offset = 9), after=last_col()) %>% 
#   select(-(`2020`:`2090`)) %>% 
#   mutate(change = (after-before)/before*100) %>% 
#   group_by(sce) %>% 
#   summarise (change = mean(change))

# fig_1_animated <- joined %>% 
#   #filter(species %in% c("BLBR","URAM")) %>% 
#   #filter(sce %in% c("sce_15", "sce_16", "sce_0")) %>% 
#   mutate(sce = as.factor(sce)) %>% 
#   ggplot(aes(x=timestep, y=sum_cur, col=source)) +
#   geom_line(aes(linetype = sce)) +
#   scale_color_manual(values=c('#d8b365','#5ab4ac'), 
#                      labels = c("Model", "Observation")) +
#   scale_linetype_manual(values = c(1:7), 
#                         #labels = c("none (observations)", "BAU", "Conservation")
#   )+
#   geom_point(aes(group = seq_along(timestep), pch = sce), show.legend = FALSE) +
#   #add_phylopic(bear, alpha = 1, x=2010, y =0.11, ysize = 10) +
#   facet_grid(~species, scales = "fixed") +
#   #facet_grid(sce~species, scales = "fixed") +
#   labs(title = "Cumulative Connectivity change for two species through time",
#        subtitle = "1990-2100",
#        #subtitle = "Year:{frame_along}",
#        y = "Cummulative Connectivity",
#        x = "Year",
#        col = "Source", 
#        #pch = "Scenario",
#        linetype = "Scenario") +
#   theme(legend.position = c(0.10, 0.27),
#         #legend.justification = c(1, -0.2),
#         legend.box = "vertical",
#         legend.background = element_blank(),
#         legend.box.background = element_rect(colour = "black"),
#         panel.border = element_rect(fill = NA),
#         legend.title = element_text(size = 15),
#         legend.text = element_text(size = 12),
#         plot.title = element_text(size=22),
#         plot.subtitle = element_text(size=22),
#         axis.text.x = element_text(angle=65, vjust=0.6, size =15),
#         axis.text.y = element_text(size =15),
#         #strip.text.x = element_blank(),
#         #strip.text.y = element_blank(),
#         axis.title=element_text(size=18)) +
#   transition_reveal(as.integer(timestep)) +
#   NULL
# fig_1_animated_plot <- animate(fig_1_animated, renderer = gifski_renderer())
# anim_save(animation = fig_1_animated_plot,
#           filename = "outputs/figures/connectivity_decrease_x5species.gif")

# fig_1_static <- joined %>% 
#   
#   #filter(species %in% c("BLBR","URAM")) %>% 
#   #filter(sce %in% c("sce_15", "sce_16", "sce_0")) %>%
#   #filter(iteration =="it_1_") %>% 
#   #filter(!(climate %in% c("RCP 8.5"))) %>% 
#   #filter(run %in% c("BAU run", "BAU run + ref")) %>% 
#   #filter(species == "BLBR") %>% 
#   #filter(run == "BAU run") %>% 
#   
#   filter(climate != "none") %>% 
#   ggplot(aes(x=timestep, y=sum_cur, col=run)) +
#   scale_color_brewer(palette="Greens") +
#   #                    labels = c("Historic", "Baseline", "RCP 8.5")) +
# 
#   geom_smooth(aes(group = sce, linetype=climate), alpha=0.2, method="loess") +
#   scale_linetype_manual(values = c(1,2,3))+
#   #geom_point(aes(group = sce, pch = run)) +
#   facet_wrap(~species, scales = "free") +
#   
#   #facet_grid(sce~species, scales = "fixed") +
#   #add_phylopic(bear, alpha = 1, x=2010, y =0.11, ysize = 10) +
#   
#   labs(title = "Cumulative Connectivity change for species through time",
#        #subtitle = "1990-2100",
#        #subtitle = "Year:{frame_along}",
#        y = "Cummulative Connectivity",
#        x = "Year",
#        col = "Run",
#        linetype = "Climate") +
#   NULL
# fig_1_static


# list_lu_1_cropped_rat <- lapply(as.list(list_lu_1_cropped), ratify)
# rat <- as.data.frame(levels(list_lu_1_cropped_rat[[1]])) ; names(rat) <- "ID"
# rat$landcover <- c('Agriculture', 'Urban', 'Forest', "Roads", "Water", "Wetlands")
# rat$class <- c('A', 'B', 'C', 'D',"E","F")
# for (idx in 1:length(list_lu_1_cropped_rat)){
#   levels(list_lu_1_cropped_rat[[idx]]) <-rat
# }
# 
# # #855C75,#D9AF6B,#AF6458,#736F4C,#526A83,#625377,
# # #68855C,#9C9C5E,#A06177,#8C785D,#467378,#7C7C7C
# 
# make_plots <- function(rasters, ts){
#   for (idx in c(1:length(rasters))){
#     plot<- levelplot(rasters[[idx]],
#                      colorkey=list(space="bottom", height=0.8,
#                                    labels = list(cex=1)),
#                      col.regions=c('#D9AF6B', '#7C7C7C',
#                                    '#68855C', '#AF6458', 
#                                    '#467378', '#625377'),
#                      main=list(paste0('Year: ', ts[idx]), fontsize=25),
#                      scales=list(draw=FALSE))
#     print(plot)
#   }
# }

# saveGIF(expr = make_plots(list_lu_1_cropped_rat, ts_template_year),
#         interval=2, movie.name="lu_animated.gif",
#         ani.width=800, ani.height=800, ani.res=100)
