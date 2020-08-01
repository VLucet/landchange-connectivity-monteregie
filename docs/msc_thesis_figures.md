Msc Thesis Figures
================
Valentin Lucet
17/07/2020

### Data prep

#### Cleaning and formatting

We start by preparing the data from the outputs of scripts `7.1` and
`8.1`. The results table shown as output is the key to read the figures.

``` r
# Results dir + mun shapefile
mun <- st_read("data/mun/munic_SHP_clean.shp", quiet = TRUE)

# results key
sce_code_vec <- 
  as.vector(t(outer(c("BAU-", "R-", "CorPr-", "R-CorPr-", "R(T)-CoPr-"), 
                    c("Hist", "Base", "RCP8"), paste0)))

results_clean <- read_rds("data/temp/stsim_run_results.RDS") %>% 
  dplyr::select(scenarioId, name) %>% 
  mutate(name =  gsub(.$name, pattern = " \\(.+\\)", replacement = "")) %>% 
  mutate(sce =  paste0("sce_", .$scenarioId)) %>% 
  mutate(chapter = c("none", rep("both", 3), rep("chap_1", 3), rep("chap_2", 9))) %>% 
  mutate(splitted = str_split(name, " \\| ")) %>% 
  mutate(climate = unlist(map(splitted, ~unlist(.x[2])))) %>% 
  mutate(run = unlist(map(splitted, ~unlist(.x[1])))) %>% 
  dplyr::select(-splitted) %>% 
  replace_na(list(climate = "none")) %>% 
  mutate(code = c("Control", sce_code_vec))

# Final extracted datasets
df_final <- readRDS("outputs/final/final_df_current_density.RDS") %>%
  mutate(timestep = (timestep*10)+1990, source = "model")
df_final_origin <- readRDS("outputs/final/final_df_origin_current_density.RDS") %>%
  mutate(timestep = timestep*10+1990, source = "model")

# Stratum key
stratum_key <- read_csv("config/stsim/SecondaryStratum.csv") %>%
  mutate(ID = as.factor(ID)) %>%
  rename(zone=ID, MUS_NM_MUN=Name) %>%
  left_join(df_final, by="zone") %>%
  filter(MUS_NM_MUN!="Not Monteregie")

# Sum all municipalities
df_summarised <- df_final %>%
  group_by(sce, timestep, species, iteration) %>% 
  summarise(sum_cur = sum(current)) %>% ungroup %>%
  mutate(source = "model")
df_origin_summarised <- df_final_origin %>%
  group_by(timestep, species) %>% 
  summarise(sum_cur = sum(mean)) %>% ungroup %>% 
  mutate(sce = "sce_0", source = "observation")

# Joined dataset
joined <- full_join(df_summarised, df_origin_summarised, 
                    by=c("sce", "source", "species", "timestep", "sum_cur")) %>% 
  left_join(results_clean, by = "sce")  %>% 
  replace_na(list(climate = "none", run = "historic run")) %>% 
  # Make factors
  mutate(sce = as.factor(sce), run = as.factor(run), sum_cur = 10*(sum_cur),
         climate = factor(climate, levels = c("none", "historic",
                                              "baseline", "RCP 8.5")),
         run = factor(run, levels = c("historic run", "BAU run", 
                                      "BAU run + corrs protection", "BAU run + ref", 
                                      "BAU run + corrs protection + ref",
                                      "BAU run + corrs protection + ref TARGETED")))

# Roc curves data
urb <- readRDS("data/temp/fit_rs_outcome_rf_urb_2.RDS")
agex <- readRDS("data/temp/fit_rs_outcome_rf_agex_2.RDS")

# Histogram data 
histograms <- read_csv("outputs/final/final_values_output.csv") %>% 
  left_join(results_clean, by="sce")

# SURF Data
surf <- read_csv("surf/surf_output.csv") %>% 
  mutate(timestep = timestep*10+1990) %>% 
  left_join(results_clean, by=c("scenario"="scenarioId"))

results_clean
```

    ##    scenarioId                                                 name    sce
    ## 1          37                                         historic run sce_37
    ## 2          38                                   BAU run | historic sce_38
    ## 3          39                                   BAU run | baseline sce_39
    ## 4          40                                    BAU run | RCP 8.5 sce_40
    ## 5          41                             BAU run + ref | historic sce_41
    ## 6          42                             BAU run + ref | baseline sce_42
    ## 7          43                              BAU run + ref | RCP 8.5 sce_43
    ## 8          44                BAU run + corrs protection | historic sce_44
    ## 9          45                BAU run + corrs protection | baseline sce_45
    ## 10         46                 BAU run + corrs protection | RCP 8.5 sce_46
    ## 11         47          BAU run + corrs protection + ref | historic sce_47
    ## 12         48          BAU run + corrs protection + ref | baseline sce_48
    ## 13         49           BAU run + corrs protection + ref | RCP 8.5 sce_49
    ## 14         50 BAU run + corrs protection + ref TARGETED | historic sce_50
    ## 15         51 BAU run + corrs protection + ref TARGETED | baseline sce_51
    ## 16         52  BAU run + corrs protection + ref TARGETED | RCP 8.5 sce_52
    ##    chapter  climate                                       run           code
    ## 1     none     none                              historic run        Control
    ## 2     both historic                                   BAU run       BAU-Hist
    ## 3     both baseline                                   BAU run       BAU-Base
    ## 4     both  RCP 8.5                                   BAU run       BAU-RCP8
    ## 5   chap_1 historic                             BAU run + ref         R-Hist
    ## 6   chap_1 baseline                             BAU run + ref         R-Base
    ## 7   chap_1  RCP 8.5                             BAU run + ref         R-RCP8
    ## 8   chap_2 historic                BAU run + corrs protection     CorPr-Hist
    ## 9   chap_2 baseline                BAU run + corrs protection     CorPr-Base
    ## 10  chap_2  RCP 8.5                BAU run + corrs protection     CorPr-RCP8
    ## 11  chap_2 historic          BAU run + corrs protection + ref   R-CorPr-Hist
    ## 12  chap_2 baseline          BAU run + corrs protection + ref   R-CorPr-Base
    ## 13  chap_2  RCP 8.5          BAU run + corrs protection + ref   R-CorPr-RCP8
    ## 14  chap_2 historic BAU run + corrs protection + ref TARGETED R(T)-CoPr-Hist
    ## 15  chap_2 baseline BAU run + corrs protection + ref TARGETED R(T)-CoPr-Base
    ## 16  chap_2  RCP 8.5 BAU run + corrs protection + ref TARGETED R(T)-CoPr-RCP8

Preparing for spatial plotting (time consuming).

``` r
# Pivoted dataset for mapping
# df_final_fordiff_pivoted <- df_final %>% 
#   left_join(results_clean, by = "sce")  %>% 
#   group_by(zone, sce, species, timestep, source, chapter) %>% 
#   summarise(current = mean(current)) %>% ungroup %>% 
#   filter(timestep %in% c(2010, last(unique(stratum_key$timestep)))) %>% 
#   pivot_wider(names_from=timestep, values_from=current) %>% drop_na() %>% 
#   rename(before=last_col(offset = 1), after=last_col()) %>% 
#   left_join(stratum_key, by=c("zone", "sce", "species", "source")) %>% 
#   filter(MUS_NM_MUN!="Not Monteregie") %>% 
#   mutate(change=((after-before)/before)*100) %>% 
#   left_join(mun, by="MUS_NM_MUN") %>% 
#   st_as_sf()
# 
# df_final_fordiff_pivoted <- 
#   st_transform(df_final_fordiff_pivoted, crs=4326)
```

#### Set the ggplot theme

We see the ggplot theme to `theme_minimal()` and change the size of some
plot elements.

``` r
theme_set(theme_minimal())
theme_update(legend.position = c(0.85, 0.20),
             #legend.justification = c(1, -0.2), # for inside plot
             #legend.position = "none",
             legend.box = "vertical",
             legend.background = element_blank(),
             legend.box.background = element_rect(colour = "black"),
             panel.border = element_rect(fill = NA),
             legend.title = element_text(size = 10),
             legend.text = element_text(size = 8),
             plot.title = element_text(size=20),
             plot.subtitle = element_text(size=20),
             axis.text.x = element_text(angle=65, vjust=0.6, size =13),
             axis.text.y = element_text(size =10),
             strip.text.x = element_text(size = 10),
             strip.text.y = element_text(size = 10),
             axis.title=element_text(size=15))
```

### Figures

### Mean current flow change

#### Historic

``` r
fig_1_historic <- joined %>% 
  filter(climate == "none") %>% 
  group_by(scenarioId, species, iteration) %>% 
  group_modify(~mutate(.x , baseline = subset(.x, timestep == 1990)$sum_cur, 
                       the_diff = ((sum_cur-baseline)/baseline)*100 )) %>% 
  ggplot(aes(x=timestep, y=the_diff, col=source)) +
  scale_color_manual(values=c('#d8b365','#5ab4ac'), 
                     labels = c("Model", "Observation")) +
  geom_smooth(aes(group = sce), method = "glm", se=FALSE) +
  scale_linetype_manual(values = c(1,3,2,5))+
  geom_point(aes(group = sce)) +
  facet_wrap(~species, scales = "free") +
  labs(y = "Current flow (% Change)",
       x = "Year",
       col = "Source") +
  NULL
fig_1_historic
```

<div class="figure">

<img src="msc_thesis_figures_files/figure-gfm/fig_1_historic-1.png" alt="Connectivity change for species through time, 1990-2010" width="100%" />

<p class="caption">

Connectivity change for species through time, 1990-2010

</p>

</div>

``` r
ggsave(fig_1_historic,
       filename = "outputs/figures/connectivity_decrease_x5species_historic.png",
       width = 15, height = 10)
```

#### Chap 1

##### Line graph

``` r
fig_1_static_chap_1 <- joined %>% 
  filter(climate != "none", chapter %in% c("none", "both", "chap_1")) %>% 
  group_by(scenarioId, species, iteration) %>% 
  group_modify(~mutate(.x , baseline = subset(.x, timestep == 2010)$sum_cur, 
                       the_diff = ((sum_cur-baseline)/baseline)*100 )) %>% 
  ggplot(aes(x=timestep, y=the_diff, col=climate)) +
  scale_color_manual(values=c('#ffe300', '#ff7a14', "#b31400"), 
                     labels = c("Historic", "Baseline", "RCP 8.5")) +
  geom_smooth(aes(group = sce, linetype=run), alpha=0.5, method="loess") +
  scale_linetype_manual(values=c("solid", "twodash")) +
  facet_wrap(~species, scales = "fixed") +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "darkGreen", alpha = 0.5) +
  labs(y = "Current flow (% Change)",
       x = "Year",
       col = "Climate",
       linetype = "Run") +
  guides(col = guide_legend(ncol = 2, nrow=2), 
         linetype = guide_legend(nrow=1, override.aes = list(colour = 'black'))) +
  theme(legend.position = c(0.85, 0.15))+
  NULL
fig_1_static_chap_1
```

<div class="figure">

<img src="msc_thesis_figures_files/figure-gfm/figure_1_model_1-1.png" alt="Connectivity change for species through time, 2010-2100" width="100%" />

<p class="caption">

Connectivity change for species through time, 2010-2100

</p>

</div>

``` r
ggsave(fig_1_static_chap_1, 
       filename = "outputs/figures/connectivity_decrease_x5species_chap1.png", 
       width = 15, height = 10)
```

##### Radar Graph

``` r
radar_data_chap1 <- joined %>% 
  filter(climate != "none", chapter %in% c("none", "both", "chap_1")) %>% 
  group_by(timestep, species, code) %>% 
  summarise(sum_cur = mean(sum_cur)) %>% ungroup %>% 
  pivot_wider(names_from = timestep, values_from = sum_cur) %>% 
  mutate(diff = ((`2100`-`2010`)/`2010`)*100) %>% 
  select(-c(`2010`:`2100`)) %>%  
  pivot_wider(names_from = code, values_from = diff) %>% 
  rename(group = species)

radar_chap1 <-  
  ggradar(radar_data_chap1, centre.y = -20, legend.position = "right",
          grid.min = -20, grid.max = 3, grid.mid = 0, 
          values.radar = c("-20 %", "0 %", ""), 
          gridline.label.offset =5, legend.text.size= 12,
          group.line.width =0.8,
          background.circle.colour  = "#ffffff", group.point.size   = 2, 
          axis.label.size   =3)
radar_chap1
```

<img src="msc_thesis_figures_files/figure-gfm/figure_1_model_1_radar-1.png" width="100%" />

``` r
ggsave("outputs/figures/radar_ggradar_chap1.png", radar_chap1, width = 15, height = 10)
```

#### Chap 2

``` r
fig_1_static_chap_2 <- joined %>% 
  filter(climate != "none", chapter %in% c("none", "both", "chap_2")) %>% 
  group_by(scenarioId, species, iteration) %>% 
  group_modify(~mutate(.x , baseline = subset(.x, timestep == 2010)$sum_cur, 
                       the_diff = ((sum_cur-baseline)/baseline)*100 )) %>% 
  ggplot(aes(x=timestep, y=the_diff, col=climate)) +
  scale_color_manual(values=c('#ffe300', '#ff7a14', "#b31400"), 
                     labels = c("Historic", "Baseline", "RCP 8.5")) +
  geom_smooth(aes(group = sce, linetype=run), alpha=0.5, method="loess") +
  scale_linetype_manual(values=c("solid", "dashed", "longdash", "dotted")) +
  facet_wrap(~species, scales = "fixed") +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "darkGreen", alpha = 0.5) +
  labs(y = "Current flow (% Change)",
       x = "Year",
       col = "Climate",
       linetype = "Run") +
  guides(col = guide_legend(ncol = 2, nrow=2), 
         linetype = guide_legend(ncol=1, 
                                 override.aes = list(colour = 'black'))) +
  theme(legend.position = c(0.85, 0.15))+
  NULL
fig_1_static_chap_2
```

<div class="figure">

<img src="msc_thesis_figures_files/figure-gfm/figure_1_model_2-1.png" alt="Connectivity change for species through time, 2010-2100" width="100%" />

<p class="caption">

Connectivity change for species through time, 2010-2100

</p>

</div>

``` r
ggsave(fig_1_static_chap_2, 
       filename = "outputs/figures/connectivity_decrease_x5species_chap2.png", 
       width = 15, height = 10)
```

##### Radar Graph

``` r
radar_data_chap2 <- joined %>% 
  filter(climate != "none", chapter %in% c("none", "both","chap_2")) %>% 
  group_by(timestep, species, code) %>% 
  summarise(sum_cur = mean(sum_cur)) %>% ungroup %>% 
  pivot_wider(names_from = timestep, values_from = sum_cur) %>% 
  mutate(diff = ((`2100`-`2010`)/`2010`)*100) %>% 
  select(-c(`2010`:`2100`)) %>%  
  pivot_wider(names_from = code, values_from = diff) %>% 
  rename(group = species)

radar_chap2 <-  
  ggradar(radar_data_chap2, centre.y = -20, legend.position = "right",
          grid.min = -20, grid.max = 3, grid.mid = 0, 
          values.radar = c("-20 %", "0 %", ""), 
          gridline.label.offset =5, legend.text.size= 12,
          group.line.width =0.8,
          background.circle.colour  = "#ffffff", group.point.size   = 2, 
          axis.label.size   =3)
radar_chap2
```

<img src="msc_thesis_figures_files/figure-gfm/figure_1_model_2_radar-1.png" width="100%" />

``` r
ggsave("outputs/figures/radar_ggradar_chap2.png", radar_chap2, width = 15, height = 10)
```

#### Both

##### Radar Graph

``` r
radar_data_all <- joined %>% 
  filter(climate != "none") %>% 
  group_by(timestep, species, code) %>% 
  summarise(sum_cur = mean(sum_cur)) %>% ungroup %>% 
  pivot_wider(names_from = timestep, values_from = sum_cur) %>% 
  mutate(diff = ((`2100`-`2010`)/`2010`)*100) %>% 
  select(-c(`2010`:`2100`)) %>%  
  pivot_wider(names_from = code, values_from = diff) %>% 
  rename(group = species)

radar_all <-  
  ggradar(radar_data_all, centre.y = -20, legend.position = "right",
          grid.min = -20, grid.max = 3, grid.mid = 0, 
          values.radar = c("-20 %", "0 %", ""), 
          gridline.label.offset =5, legend.text.size= 12,
          group.line.width =0.8,
          background.circle.colour  = "#ffffff", group.point.size   = 2, 
          axis.label.size   =3)
radar_all
```

<img src="msc_thesis_figures_files/figure-gfm/figure_1_model_both_radar-1.png" width="100%" />

``` r
ggsave("outputs/figures/radar_ggradar_both.png", radar_all, width = 15, height = 10)
```

### Map of change

Mapping takes a lot of time, not knitted by default

#### Chap

``` r
# change_1 <- df_final_fordiff_pivoted %>% 
#   filter(chapter %in% c("none", "both", "chap_1")) %>%
#   filter(sce %in% c('sce_38', 'sce_39')) %>% 
#   ggplot(aes(fill=change)) +
#   geom_sf(show.legend=T, lwd = 0)  + 
#   facet_wrap(~sce) + 
#   ggtitle("Connectivity change in %", 
#           subtitle = "2010-2100") +
#   scale_fill_binned(low='#d13e11', high='#fff7bc', 
#                     breaks=c(-50, -40, -30, -20, -10, 0, 10))
# change_1
#ggsave("outputs/figures/connectivit_change_mun_chap1.png", change_1)
```

#### Chap 2

``` r
# change_2 <- df_final_fordiff_pivoted %>% 
#   filter(chapter %in% c("none", "both", "chap_2")) %>%
#   ggplot(aes(fill=change)) +
#   geom_sf(show.legend=T, lwd = 0)  + 
#   facet_wrap(~sce) + 
#   ggtitle("Connectivity change in %", 
#           subtitle = "2010-2100") +
#   scale_fill_binned(low='#d13e11', high='#fff7bc', 
#                     breaks=c(-50, -40, -30, -20, -10, 0, 10))
# change_2
#ggsave("outputs/figures/connectivit_change_mun_chap2.png", change_2)
```

#### Both

``` r
# change_both <- df_final_fordiff_pivoted %>% 
#   ggplot(aes(fill=change)) +
#   geom_sf(show.legend=T, lwd = 0)  + 
#   facet_wrap(~sce) + 
#   ggtitle("Connectivity change in %", 
#           subtitle = "2010-2100") +
#   scale_fill_binned(low='#d13e11', high='#fff7bc', 
#                     breaks=c(-50, -40, -30, -20, -10, 0, 10))
# change_both
#ggsave("outputs/figures/connectivit_change_mun_both.png", change_both)
```

### ROC Curves

ROC curves from fitting both models, plotting with `patchwork` package.

``` r
p1 <- bind_rows(urb, .id = "fold") %>% 
  mutate(Fold = factor(fold, levels = as.character(1:10))) %>% 
  group_by(Fold) %>% 
  roc_curve(.pred, truth = outcome_fact) %>% 
  autoplot(add=T) +
  ggtitle(paste("Urbanisation")) +
  annotate(x = 0.75, y = 0.25, geom="label", 
           label = as.character(paste("Av AUC = 0.938 +/- 0.002"))) +
  theme(legend.position = "none") +
  NULL

p2 <- bind_rows(agex, .id = "fold") %>% 
  mutate(Fold = factor(fold, levels = as.character(1:10))) %>% 
  group_by(Fold) %>% 
  roc_curve(.pred, truth = outcome_fact) %>% 
  autoplot(add=T) +
  ggtitle(paste("Agricultural Expansion")) +
  annotate(x = 0.75, y = 0.25, geom="label", 
           label = as.character(paste("Av AUC = 0.929 +/- 0.002"))) +
  NULL

full = p1 + p2

full
```

<img src="msc_thesis_figures_files/figure-gfm/roc_curves-1.png" width="100%" />

``` r
ggsave("outputs/figures/double_roc_resample.png", full)
```

### Histograms

#### Chap 1

``` r
hist_plot_1 <- histograms %>% 
  mutate(ts = as.factor(ts)) %>% 
  filter(chapter %in% c('both', 'chap_1')) %>%  
  ggplot(aes(x=bins, y=n, group=ts, colour=ts)) + 
  geom_density(stat='identity', show.legend=F, aes(fill=factor(ts)), alpha=0.3)+
  facet_grid(code~species) +
  labs(x = "Flow intensity distribution (log)", 
       y = "")+
  theme(strip.text.y = element_text(angle=360, size=10, hjust = 0), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.text.y.left = element_blank(),
        axis.ticks.y.left = element_blank()) +
  NULL
hist_plot_1
```

<img src="msc_thesis_figures_files/figure-gfm/hist_plot_1-1.png" width="100%" height="150%" />

#### Chap 2

``` r
hist_plot_2 <- histograms %>% 
  mutate(ts = as.factor(ts)) %>% 
  filter(chapter %in% c('both', 'chap_2')) %>%  
  ggplot(aes(x=bins, y=n, group=ts, colour=ts)) + 
  geom_density(stat='identity', show.legend=F, aes(fill=factor(ts)), alpha=0.3)+
  facet_grid(code~species, labeller = label_wrap_gen(width = 5, multi_line = TRUE)) +
  labs(x = "Flow intensity distribution (log)", 
       y = "") +
  theme(strip.text.y = element_text(angle=360, size=10, hjust = 0), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.text.y.left = element_blank(),
        axis.ticks.y.left = element_blank()) +
  NULL
hist_plot_2
```

<img src="msc_thesis_figures_files/figure-gfm/hist_plot_2-1.png" width="100%" />
\#\#\# SURF Analysis

#### Chap1

##### Linear Graph

##### Radar Graph

#### Chap 2

##### Linear Graph

##### Radar Graph
