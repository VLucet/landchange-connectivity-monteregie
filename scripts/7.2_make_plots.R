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
  #library(caret)
})

#-------------------------------------------------------------------------------
# Get result secenario directory 
sce_dir_vec <- list.files("libraries/stsim/monteregie-conncons-scripted.ssim.output", 
                          full.names = T)
sce_nb_vec <- paste0("sce_", as.numeric(unlist(lapply(str_split(sce_dir_vec, "-"), FUN = last))))

# Data prep
# listofiles <- list.files("it1_lasterun/it1_MAAM_Current/", full.names = T)
df_final <- readRDS("outputs/final/final_df_current_density.RDS") %>% 
  mutate(timestep = (timestep*10)+1990, source = "model")
df_final_origin <- readRDS("outputs/final/final_df_origin_current_density.RDS") %>% 
  mutate(timestep = timestep*10+1980, source = "model")
mun <- st_read("data/mun/munic_SHP_clean.shp", quiet = TRUE)

# Summarised
df_summarised <- df_final %>%
  group_by(sce, timestep, species) %>% 
  summarise(sum_cur = sum(mean)) %>% ungroup %>%
  mutate(source = "model")
df_origin_summarised <- df_final_origin %>%
  group_by(timestep, species) %>% 
  summarise(sum_cur = sum(mean)) %>% ungroup %>% 
  mutate(sce = "sce_0", source = "observation")

joined <- full_join(df_summarised, 
                    df_origin_summarised, 
                    by=c("sce","source", "species", 
                         "timestep", "sum_cur"))

# Set theme
theme_set(theme_minimal())

#-------------------------------------------------------------------------------

# FIGURE 0 
all_facetted <- ggplot(df_final) + 
  aes(x = timestep, y = mean, group = zone, color=zone) + 
  geom_line(show.legend = FALSE, alpha=0.2) + 
  scale_color_viridis_d(option = "plasma") + 
  facet_grid(sce ~ species)+
  theme(legend.position = "none") 
ggsave("outputs/figures/final_graph.png", all_facetted)

## FIGURE 1
animated <- joined %>% 
  mutate(sce = as.factor(sce)) %>% 
  ggplot() +
  aes(x=timestep, y=sum_cur, col=source) +
  geom_line(aes(linetype = sce)) +
  scale_color_manual(values=c('#d8b365','#5ab4ac')) +
  geom_point(aes(group = seq_along(timestep), pch = sce)) +
  facet_grid(~species, scales = "fixed") +
  #facet_grid(sce~species, scales = "fixed") +
  labs(title = "Cumulative Connectivity for each species through time",
       subtitle = "Year:{frame_along}",
       y = "Cummulative Connectivity",
       x = "Year",
       col = "Source", 
       linetype = "Scenario") +
  theme(#legend.position = c(0.95, 0),
    #legend.justification = c(1, -0.2),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    plot.title = element_text(size=22),
    plot.subtitle = element_text(size=22),
    axis.text.x = element_text(angle=65, vjust=0.6),
    #strip.text.x = element_blank(),
    #strip.text.y = element_blank(),
    axis.title=element_text(size=18)) +
  transition_reveal(as.integer(timestep))
options(gganimate.dev_args = list(width = 1200, height = 800))
plot_anim <- animate(animated, renderer = gifski_renderer())
anim_save(animation = plot_anim, 
          filename = "outputs/figures/connectivity_decrease_x5species.gif")

## FIGURE 2 => will break
key <- read_csv("config/stsim/SecondaryStratum.csv") %>%
  mutate(ID = as.factor(ID)) %>%
  rename(zone=ID, MUS_NM_MUN=Name) %>%
  left_join(df_final, by="zone") %>%
  filter(MUS_NM_MUN!="Not Monteregie")

#mun_joined <- left_join(mun, key, by="MUS_NM_MUN")

df_final_fordiff_pivoted <- df_final %>% 
  filter(timestep %in% c(first(unique(key$timestep)),
                         last(unique(key$timestep)))) %>% 
  pivot_wider(names_from=timestep, values_from=mean) %>%
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
  facet_grid(~sce) +
  ggtitle("Connectivity change in %", 
          subtitle = "1990-2100") +
  scale_fill_binned(low='#d13e11', high='#fff7bc', 
                    breaks=c(-50, -40, -30, -20, -10, 0, 10))
ggsave("outputs/figures/connectivit_change_mun.png", change)

## FIGURE 3
it_1 <- list.files(file.path(sce_dir_vec[1],"stsim_OutputSpatialState"),                    
                   pattern = "it1",
                   full.names = T)
it_1 <- list.files("test/it/",
                   full.names = T)

it_1_CS <- stack(readRDS("test/final_cur_sum_sce_1_per_ts.RDS"))

list_lu <-  stack(lapply(mixedsort(it_1),FUN=raster))
# list_lu_masked <- crop(mask(list_lu,mun),mun)
extent_zoom <- extent(c(621300, 621300+20000, 5023000, 5023000+20000))
list_lu_1_cropped <- (crop(list_lu, extent_zoom))

cs_cropped <- crop(it_1_CS, extent_zoom)

ts_template <- seq(from =0, by=10, length.out = nlayers(list_lu))
ts_template_year <- ts_template+1990

df_list <- list()
for (ts in 1:nlayers(list_lu_1_cropped)){
  df <- as.data.frame(freq(list_lu_1_cropped[[ts]])) %>%
    filter(!is.na(value)) %>%
    filter(!(value %in% 4:6))
  df$count <- df$count/ncell(list_lu_1_cropped[[1]])
  df$timestep <- ts_template[ts]
  df_list[[ts]] <- df
}

df_final <- df_list[[1]]
for (df in df_list[2:length(df_list)]){
  df_final <- full_join(df_final, df,
                        by = c("value", "count", "timestep"))
}

names(df_final)[2] <- "proportion"
df_final$timestep <- df_final$timestep+1990
df_final$value <- as.character(df_final$value)
df_final$value[df_final$value==1] <- "Agriculture"
df_final$value[df_final$value==2] <- "Urban"
df_final$value[df_final$value==3] <- "Forest"

plot <- ggplot(df_final)+
  aes(x=timestep, y=proportion, col=value, show.legend=F) +
  geom_line() +
  geom_point(size = 2) +
  # geom_segment(aes(xend = 2060, yend = proportion, col=value),
  #              linetype = 2, colour = 'grey') +
  geom_segment(aes(xend = 2103, yend = proportion), linetype = 2)+
  geom_text(aes(x = 2105, label = value), hjust = 0, size=5, fontface="bold") +
  scale_color_manual(values = c('#dfc27d', '#339933','#a6611a'))+
  geom_point(aes(group = seq_along(timestep))) +
  coord_cartesian(clip = 'off') +
  transition_reveal(as.integer(timestep)) +
  labs(y="Proportion",
       x = "Year")+
  theme_minimal()+
  theme(legend.position = "none") +
  theme(plot.margin = margin(5.5, 60, 5.5, 5.5),
        axis.title=element_text(size=20, face="bold"),
        axis.text.x =element_text(size=15),
        axis.text.y =element_text(size=15))
options(gganimate.dev_args = list(width = 900, height = 700))
plot_anim <- animate(plot, renderer = gifski_renderer())
anim_save("outputs/figures/lu_change_animated.gif")

# Raster gifs
list_lu_1_cropped_rat <- lapply(as.list(list_lu_1_cropped), ratify)

rat <- as.data.frame(levels(list_lu_1_cropped_rat[[1]])) ; names(rat) <- "ID"
rat$landcover <- c('Agriculture', 'Urban', 'Forest', "Roads", "Water", "Wetlands")
rat$class <- c('A', 'B', 'C', 'D',"E","F")
for (idx in 1:length(list_lu_1_cropped_rat)){
  levels(list_lu_1_cropped_rat[[idx]]) <-rat
}

# #855C75,#D9AF6B,#AF6458,#736F4C,#526A83,#625377,
# #68855C,#9C9C5E,#A06177,#8C785D,#467378,#7C7C7C

make_plots <- function(rasters, ts){
  for (idx in c(1:length(rasters))){
    plot<- levelplot(rasters[[idx]],
                     colorkey=list(space="bottom", height=0.8,
                                   labels = list(cex=1)),
                     col.regions=c('#D9AF6B', '#7C7C7C',
                                   '#68855C', '#AF6458', 
                                   '#467378', '#625377'),
                     main=list(paste0('Year: ', ts[idx]), fontsize=25),
                     scales=list(draw=FALSE))
    print(plot)
  }
}

make_plots_cs <- function(rasters, ts){
  
  the_max <- max(unlist(lapply(as.list(cs_cropped), cellStats, max)))
  the_min <- min(unlist(lapply(as.list(cs_cropped), cellStats, min)))
  
  for (idx in c(1:length(rasters))){
    plot<- rasterVis::levelplot(rasters[[idx]],
                     main=list(paste0('Year: ', ts[idx]), fontsize=25),
                     maxpixels= 10000000,
                     margin=list(draw=FALSE), 
                     at=seq(the_min, the_max, length.out=100))
    print(plot)
  }
}

oldwd <- getwd()
setwd("outputs/figures/")
saveGIF(expr = make_plots(list_lu_1_cropped_rat, ts_template_year),
        interval=2, movie.name="lu_animated.gif",
        ani.width=800, ani.height=800, ani.res=100)
saveGIF(expr = make_plots_cs(as.list(cs_cropped), ts_template_year),
        interval=2, movie.name="cs_animated.gif",
        ani.width=800, ani.height=800, ani.res=100)
setwd(oldwd)

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
# # TODO throws error
# # anim_save(plot_anim, "outputs/figures/connectivit_change_mun_gif.gif")
