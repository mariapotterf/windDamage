

# Put together final Figures for teh manuscript

# Includes final Methods and Results for Wind risk by management paper


# Results and methods for paper 

# Read libraries ----
rm(list = ls())


library(data.table)
library(dplyr)
library(raster)
library(ggplot2)
library(sf)
library(stringr)
library(gridExtra)
library(tidyr)
library(ggpubr)
library(broom)
#library(RColorBrewer)


# Set themes ----
theme_set(theme_classic())
theme_update(panel.grid.major = element_line(colour = "grey95",
                                             size = 0.1,
                                             linetype = 2),
             strip.background = element_rect(color="grey95", 
                                             fill="grey95",
                                             size=0.1, 
                                             linetype="solid"))

source("C:/MyTemp/myGitLab/windDamage/myFunctions.R")


# Set wd ------------------------------------------------------------------

setwd("C:/MyTemp/myGitLab/windDamage/output/even_flow")

# Read input data ----------------------------------------------------

# includes optimal scenario, raster data, windRisk
df <- fread(paste(getwd(), "finalFoPlotting.csv", sep = "/"))


# change windRisk to probabilities: multiply by 100%
df$windRisk = df$windRisk*100




#  Understand the sheltering effect ----------------------------------------
df.nbrs.risk <- data.table::fread(paste(getwd(), "df_nbrs_diff_risk.csv", sep = "/"))

df.nbrs2 <-
  df.nbrs.risk %>% 
  mutate(abs_diff_rsk = abs(windRisk- windRisk_nbrs))  %>%
  group_by(id, landscape) %>% 
  summarise(abs_diff_rsk = mean(abs_diff_rsk, na.rm = T))
  
# Join calculated values of risk differences to simulated data
df <-
  df %>% 
  left_join(df.nbrs2, by = c("id", "landscape"))

# increase absolute difference wiind risk by 100%
df$abs_diff_rsk =  df$abs_diff_rsk*100





# PLot difference in wind risk between neighbors --------------------------

p.mean.risk.nbrs.line.npi <-
  df %>% 
  group_by(scenSimpl2, 
           NPI, 
           Management) %>% 
  summarize(my_y = mean(abs_diff_rsk, na.rm =T ))  %>% # there are NA if V and V_strat_max = 0
  ggplot(aes(y = my_y, 
             x = NPI, 
             shape = scenSimpl2,     
             color = scenSimpl2,     
             linetype = scenSimpl2,  
             group = scenSimpl2,     
             fill = scenSimpl2 )) +  
  #ylim(0,350) +
  xlab("NPI (k€/ha)") + #
  ylab("Wind damage risk difference\nbetween neighbors (%)") +
  plot_line_details()



p.mean.risk.nbrs.time <-
  df %>% 
  group_by(scenSimpl2, 
           year, 
           Management) %>% 
  summarize(my_y = mean(abs_diff_rsk, na.rm =T ))  %>% # there are NA if V and V_strat_max = 0
  ggplot(aes(y = my_y, 
             x = year, 
             shape = scenSimpl2,     
             color = scenSimpl2,     
             linetype = scenSimpl2,  
             group = scenSimpl2,     
             fill = scenSimpl2 )) +  
 # ylim(0,350) +
  xlab("Time") + #
  ylab("Wind damage risk difference\nbetween neighbors (%)") +
  plot_line_details()



ggarrange(p.mean.risk.nbrs.line.npi, p.mean.risk.nbrs.time, 
          ncol = 2, nrow = 1,
          #widths = c(1, 1),
          common.legend = TRUE,
          align = c("hv"),
          #font.label = list(size = 10, color = "black", face = "plain", family = NULL),
          legend="bottom",
          labels= "AUTO",
          hjust = -5,
          vjust = 3,
          font.label = list(size = 10, 
                            face = "bold", 
                            color ="black"))

















# does CCF thinning affect tree height after? -----------------------------

# subset one stand with thinning under CCF
# see if tree  height lowers after thinning
unique(df$branching_new)

# Calculate back when thinning has happened
df$thin_year = df$year - df$difference

# Check the development:
df %>% 
  filter(branching_new == "Selection cut_1") %>% 
  filter(id == 19104636) %>% 
  dplyr::select(year, THIN, H_dom, difference, thin_year) #%>% 

# Make plots
# thinning time is indicated by red  line
p.H<- 
  df %>% 
  filter(branching_new == "Selection cut_1") %>% 
  filter(id == 19104636) %>% 
  dplyr::select(year, THIN, H_dom, thin_year) %>% 
  ggplot(aes(x = year,
             y = H_dom)) + 
  geom_line() + 
  geom_vline(aes(xintercept = thin_year), colour = "red", linetype = "dashed") +
  ylim(150,280)

p.n<- df %>% 
  filter(branching_new == "Selection cut_1") %>% 
  filter(id == 19104636) %>% 
  dplyr::select(year, THIN, H_dom, N,  thin_year) %>% 
  ggplot(aes(x = year,
             y = N)) + 
  geom_line() +
  geom_vline(aes(xintercept = thin_year), colour = "red", linetype = "dashed") 

p.V_strat<- df %>% 
  filter(branching_new == "Selection cut_1") %>% 
  filter(id == 19104636) %>% 
  dplyr::select(year, THIN, H_dom, V_strat_max,thin_year ) %>% 
  ggplot(aes(x = year,
             y = V_strat_max)) + 
  geom_vline(aes(xintercept = thin_year), colour = "red", linetype = "dashed") +
  geom_line()# + 
  #ylim(150,280)

p.BA<- df %>% 
  filter(branching_new == "Selection cut_1") %>% 
  filter(id == 19104636) %>% 
  dplyr::select(year, THIN, H_dom, BA,thin_year) %>% 
  ggplot(aes(x = year,
             y = BA)) + 
  geom_vline(aes(xintercept = thin_year), colour = "red", linetype = "dashed") +
  geom_line()# + 

  
ggarrange(p.H, p.n, p.V_strat, p.BA, nrow = 2, ncol = 2)


# Define the plotting ------------------------------------------------------

# Define own palette, color blind
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# Make plots with means  -----------------------------------------------------------

# try a plot where they will all be in a same plot: different color lines
# maybe exclude SA?


## Wind risk plot -----

p.mean.windRisk.line.npi2 <-
  df %>% 
  group_by(scenSimpl2, 
           NPI, 
           Management) %>% 
  summarize(sum.risk = mean(windRisk))  %>%
  ggplot(aes(y = sum.risk, 
             x = NPI, 
             shape = scenSimpl2,     
             color = scenSimpl2,     
             linetype = scenSimpl2,  
             group = scenSimpl2,     
             fill = scenSimpl2 )) +  
  ylim(0, 6) +
  facet_wrap(.~Management)  + # scenSimpl2
  xlab("NPI (k€/ha)") + #
  ylab("Wind damage probability\n(mean, %)") +
  plot_line_details()



# Wind risk over time
p.mean.windRisk.line.time2 <-
  df %>% 
  group_by(scenSimpl2, 
           year, 
           Management) %>% 
  summarize(my.y = mean(windRisk)) %>%
  ggplot(aes(y = my.y, 
             x = year, 
             shape = scenSimpl2,     
             color = scenSimpl2,     
             linetype = scenSimpl2,  
             group = scenSimpl2,     
             fill = scenSimpl2 )) +  
  ylim(0,6) +
  xlab("Time") + #
  ylab("Wind damage probability\n(mean, %)") +
  plot_line_details()






#### Investigate timber volume proportions  --------------
# How does the % of the volume change between SA and regimes, and among scenarios??

# Make plots with proportions, total volum, total top volume,
# can dgo to supplementary material




library(ggtext)

#### V at top layer  -----------------

p.mean.V_stratum.line.npi <-
  df %>% 
  group_by(scenSimpl2, 
           NPI, 
           Management) %>% 
  summarize(my_y = mean(V_strat_max, na.rm =T ))  %>% # there are NA if V and V_strat_max = 0
  ggplot(aes(y = my_y, 
             x = NPI, 
             shape = scenSimpl2,     
             color = scenSimpl2,     
             linetype = scenSimpl2,  
             group = scenSimpl2,     
             fill = scenSimpl2 )) +  
  ylim(0,350) +
  xlab("NPI (k€/ha)") + #
  ylab("Top stratum volume\n(mean m^3/ha)") +
  #ylab(expression(paste("Top stratum volume ", "\n(mean ", m^3, "/ha)")))  +
  #ylab(expression('Top stratum volume\n'^2'moment'^2'moment')) +
  #ylab(expression(paste("Top stratum volume\n(mean, m^3/ha)"))) +  
  #ylab(expression(paste("y axis ", ^2))) +
  plot_line_details()



p.mean.V_stratum.line.time <-
  df %>% 
  group_by(scenSimpl2, 
           year, 
           Management) %>% 
  summarize(my_y = mean(V_strat_max, na.rm =T ))  %>% # there are NA if V and V_strat_max = 0
  ggplot(aes(y = my_y, 
             x = year, 
             shape = scenSimpl2,     
             color = scenSimpl2,     
             linetype = scenSimpl2,  
             group = scenSimpl2,     
             fill = scenSimpl2 )) +  
  ylim(0,350) +
  xlab("Time") + #
  ylab("Top stratum volume\n(mean, m^3/ha)") +
  plot_line_details()

  
  
#### Plot proportions ---------------------
p.mean.V_prop.line.npi <-
  df %>% 
   group_by(scenSimpl2, 
           NPI, 
           Management) %>% 
  summarize(my_y = mean(V_prop, na.rm =T ))  %>% # there are NA if V and V_strat_max = 0
  ggplot(aes(y = my_y, 
             x = NPI, 
             shape = scenSimpl2,     
             color = scenSimpl2,     
             linetype = scenSimpl2,  
             group = scenSimpl2,     
             fill = scenSimpl2 )) +  
  ylim(0,100) +
  xlab("NPI (k€/ha)") + #
  ylab("Ratio top:total\nvolume (mean, %)") +
  plot_line_details()


p.mean.V_prop.line.time <-
  df %>% 
  group_by(scenSimpl2, 
           year, 
           Management) %>% 
  summarize(my_y = mean(V_prop, na.rm =T ))  %>% # there are NA if V and V_strat_max = 0
  ggplot(aes(y = my_y, 
             x = year, 
             shape = scenSimpl2,     
             color = scenSimpl2,     
             linetype = scenSimpl2,  
             group = scenSimpl2,     
             fill = scenSimpl2 )) +  
  ylim(0,100) +
  xlab("Time") + #
  ylab("Ratio top:total\nvolume(mean, %)") +
  plot_line_details()



#### Plot the Volume proportion top vs total over NPI and time  -------------------------------------------
windows(width = 7, height = 7.5)
ggarrange(p.mean.windRisk.line.npi2, p.mean.windRisk.line.time2, 
          p.mean.V_stratum.line.npi, p.mean.V_stratum.line.time, 
          p.mean.V_prop.line.npi,    p.mean.V_prop.line.time, 
          ncol = 2, nrow = 3,
          #widths = c(1, 1),
          common.legend = TRUE,
          align = c("hv"),
          #font.label = list(size = 10, color = "black", face = "plain", family = NULL),
          legend="bottom",
          labels= "AUTO",
          hjust = -5,
          vjust = 3,
          font.label = list(size = 10, 
                            face = "bold", 
                            color ="black"))













# Plot explanatory variables # ---------------------------------------------



plot_line_details_act <- function() {
  list(
    geom_line(    size = 0.9),
   # facet_wrap(.~Management), 
    ggtitle(""),
    scale_linetype_manual(values = c( "dotted", 
                                      "solid",  
                                      'dashed')),
    scale_color_manual(values = cbp1),
    scale_fill_manual(values = cbp1),
    labs(shape = "Scenario",
         color = "Scenario",
         linetype = "Scenario",
         fill = "Scenario"),
    theme(axis.title  = element_text(size = 10, face="plain", family = "sans"),
          axis.text.x = element_text(angle = 90, vjust = 0.5, face="plain", size = 9, family = "sans"),
          axis.text.y = element_text(face="plain", size = 9, family = "sans"),
          legend.position = "right",
          strip.background =element_rect(fill="white", 
                                         color = NA))
  )
}

# Tree species
# Tree height
# Time since thinning
# open-neighbour


## Tree species -------------------------

# Spruce is the most vulnerable: frequency of spruce

# Calculate the mean # of spruces!!
# over time and npi

# Calculate the mean # of spruces!! Why mean???
# How many stand I have for each simple scen vs NPI?
# what I want to show? how does the share of teh spruce changes over 
# harvest and time gradient??


df %>% 
  group_by(scenSimpl2, 
           NPI) %>% 
  tally() %>%
  print(n = 70)

# different number of stands by combination
# ranges from 29400 to 29280 - caulculate the % from the 29400 stands
tot.stand.n = 1470*20


# Make plots for npi and time

p.spruce.ratio.npi <-
  df %>% 
  filter(Management == "Active") %>% 
  group_by(scenSimpl2, 
           NPI, 
           Management) %>% 
  filter(species == "spruce") %>% 
  tally() %>%
  mutate(spruce_prop = n/tot.stand.n*100)  %>%  # get the proportion of spruce from all stands over 20 years 
  ggplot(aes(y = spruce_prop, 
             x = NPI, 
             shape = scenSimpl2,
             color = scenSimpl2,
             linetype = scenSimpl2,
             group = scenSimpl2,
             fill = scenSimpl2)) +
  xlab("NPI (k€/ha)") + #
  ylab("Spruce proportion\n(%)") +
  ylim(0,50) +
  plot_line_details_act()

# to calulate how many landscapes I have over time:
p.spruce.ratio.time <-
  df %>% 
  filter(Management == "Active") %>% 
  group_by(scenSimpl2, 
           year, 
           Management) %>% #, species
  filter(species == "spruce") %>% 
  tally() %>%
  mutate(spruce_prop = n/tot.stand.n*100)  %>%  # get the proportion of spruce from all stands over 20 years 
  ggplot(aes(y = spruce_prop, 
             x = year, 
             shape = scenSimpl2,
             color = scenSimpl2,
             linetype = scenSimpl2,
             group = scenSimpl2,
             fill = scenSimpl2)) +
  ylim(0,50) +
  xlab("Time") + #
  ylab("Spruce proportion\n(%)") +
  plot_line_details_act()
  
 

## H Dom ---------------------
p.mean.H_dom.npi <-
  df %>% 
  filter(Management == "Active") %>% 
  group_by(scenSimpl2, 
           NPI, 
           Management) %>% 
  #summarize(s_area = sum(area)) %>% 
  summarize(my_y = weighted.mean(H_dom,  area, na.rm = T))  %>% 
 # summarize(my_y = mean(H_dom))  %>%
  ggplot(aes(y = my_y, 
             x = NPI, 
             shape = scenSimpl2,
             color = scenSimpl2,
             linetype = scenSimpl2,
             group = scenSimpl2,
             fill = scenSimpl2)) +
  ylim(0,300) +
  ggtitle("") +
  xlab("NPI (k€/ha)") + #
  ylab("Tree height\n(w.mean, dm)") +
  plot_line_details_act()



p.mean.H_dom.time <-
  df %>% 
  filter(Management == "Active") %>% 
  group_by(scenSimpl2, 
           year, 
           Management) %>% 
  #summarize(my_y = mean(H_dom ))  %>%
  summarize(my_y = weighted.mean(H_dom,  area, na.rm = T))  %>% 
  ggplot(aes(y = my_y, 
             x = year, 
             shape = scenSimpl2,
             color = scenSimpl2,
             linetype = scenSimpl2,
             group = scenSimpl2,
             fill = scenSimpl2)) +
  ylim(0,300) +
  ggtitle("") +
  xlab("Time ") + #
  ylab("Tree height\n(w. mean, dm)") +
  plot_line_details_act()



## Time since thinning ------------------------
# get the difference by landscape???
# replace NA by 0?
# get mean landscape differences by NPI, by time
# how to calculate the time since thinning? for NPi, for time??
# calculate weighted mean??
# get the sum of area and then calculate? yes, seems working and has the same results are using n


p.mean.thin.npi <-
  df %>% 
  filter(Management == "Active") %>% 
  group_by(scenSimpl2, 
           NPI, 
           Management,
           difference) %>% 
  summarize(s_area = sum(area)) %>% 
  summarize(my_y = weighted.mean(difference,  s_area, na.rm = T))  %>% 
 # tally() %>%
 # summarize(my_y = weighted.mean(difference, n, na.rm = T)) %>% 
  ggplot(aes(y = my_y, 
             x = NPI, 
             shape = scenSimpl2,
             color = scenSimpl2,
             linetype = scenSimpl2,
             group = scenSimpl2,
             fill = scenSimpl2)) +
  ylim(0,30) +
  xlab("NPI (k€/ha)") + #
  ylab("Years since thinning\n(weigh. mean)") +
  plot_line_details_act()


# Mean thinning over time
p.mean.thin.time <-
  df %>% 
  filter(Management == "Active") %>% 
  group_by(scenSimpl2, 
           year, 
           Management,
           difference) %>% 
  summarize(s_area = sum(area)) %>% 
  summarize(my_y = weighted.mean(difference,  s_area, na.rm = T))  %>% 
  ggplot(aes(y = my_y, 
             x = year, 
             shape = scenSimpl2,
             color = scenSimpl2,
             linetype = scenSimpl2,
             group = scenSimpl2,
             fill = scenSimpl2)) +
  ylim(0,30) +
  xlab("Time ") + #
  ylab("Years since thinning\n(weigh. mean)") +
  plot_line_details_act()



# Plot neighboring stand difference ---------------------------------------


p.mean.H_diff.line.npi <-
  df %>% 
 # filter(Management == "Active") %>% 
  group_by(scenSimpl2, 
           NPI, 
           Management) %>% 
  #summarize(my_y = mean(mean_H_diff, na.rm =T ))  %>% # there are NA if V and V_strat_max = 0
  summarize(my_y = weighted.mean(mean_H_diff,  area, na.rm = T))  %>% 
  ggplot(aes(y = my_y, 
             x = NPI, 
             shape = scenSimpl2,     
             color = scenSimpl2,     
             linetype = scenSimpl2,  
             group = scenSimpl2,     
             fill = scenSimpl2 )) +  
  ylim(0,10) +
  xlab("NPI (k€/ha)") + #
  ylab("Height difference\nneighbors (mean, m)") +
  plot_line_details()



p.mean.H_diff.line.time <-
  df %>% 
 # filter(Management == "Active") %>% 
  group_by(scenSimpl2, 
           year, 
           Management) %>% 
  #summarize(my_y = mean(mean_H_diff, na.rm =T ))  %>% 
  summarize(my_y = weighted.mean(mean_H_diff,  area, na.rm = T))  %>% 
  ggplot(aes(y = my_y, 
             x = year, 
             shape = scenSimpl2,     
             color = scenSimpl2,     
             linetype = scenSimpl2,  
             group = scenSimpl2,     
             fill = scenSimpl2 )) +  
  ylim(0,10) +
  xlab("Time") + #
  ylab("Height difference\nneighbors(mean, m)") +
  plot_line_details()



# get table mean +- sd
df %>% 
  filter(Management == "Active" & scenSimpl2 != "RF") %>% 
  group_by(scenSimpl2, 
           NPI, 
           Management) %>% 
  summarize(mean = weighted.mean(mean_H_diff,  area, na.rm = T),
            sd = sd(mean_H_diff,  na.rm = T))  %>%
  print(n = 40)
  
# `summarise()` regrouping output by 'scenSimpl2' (override with `.groups` argument)
# # A tibble: 6 x 4
# # Groups:   scenSimpl2 [3]
# scenSimpl2 Management  mean    sd
# <chr>      <chr>      <dbl> <dbl>
# 1 ALL        Active      4.10  3.37
# 2 ALL        Set Aside   4.26  3.32
# 3 CCF        Active      3.47  2.20
# 4 CCF        Set Aside   3.80  2.71
# 5 RF         Active      7.05  4.35
# 6 RF         Set Aside   5.45  3.77
# over time
df %>% 
  filter(Management == "Active" & scenSimpl2 != "RF") %>% 
  group_by(scenSimpl2, 
           year, 
           Management) %>% 
  summarize(mean = weighted.mean(mean_H_diff,  area, na.rm = T),
            sd = sd(mean_H_diff,  na.rm = T))  %>%
  print(n = 40)




### Plot spruce prop, H_dom, thinning frequency ffor actively managed stands
# (SA goes to Supplementary material)
#  --------------------------------------------

windows(width = 7, height = 5)
ggarrange(p.spruce.ratio.npi, p.mean.H_dom.npi, p.mean.thin.npi, 
          p.spruce.ratio.time, p.mean.H_dom.time,p.mean.thin.time,
          ncol = 3, nrow = 2,
          common.legend = TRUE,
          align = c("hv"),
          legend="bottom",
          labels= "AUTO",
          hjust = -2,
          vjust = 2,
          font.label = list(size = 10, 
                            face = "bold", color ="black"))


# Plot  height difference ------------------------ 
windows(width = 7.4, height = 2.7)
ggarrange(p.mean.H_diff.line.npi, p.mean.H_diff.line.time, 
          ncol = 2, nrow = 1,
          #widths = c(1, 1),
          common.legend = TRUE,
          align = c("hv"),
          legend="bottom",
          labels= "AUTO",
          hjust = -5,
          vjust = 3,
          font.label = list(size = 10, 
                            face = "bold", color ="black"))







### Get summary statistics volumes & risk, make table ---------------------------
# Make a nice table including wind risk and total timber volume
# how to show how much volume is in the top and total straum? does this change by scenario???

sum_tab<-
  df %>% 
  group_by(scenSimpl2,  
           Management) %>% 
  summarize(mean_risk    = round(mean(windRisk,    na.rm = T),  3),
            sd_risk      = round(sd(  windRisk,    na.rm = T),  3),
            mean_V       = round(mean(V,           na.rm = T),  1),
            sd_V         = round(sd(  V,           na.rm = T),  1),
            mean_V_strat = round(mean(V_strat_max, na.rm = T),  1),
            sd_V_strat   = round(sd(  V_strat_max, na.rm = T),  1),
            mean_V_prop  = round(mean(V_prop,      na.rm = T),  1),
            sd_V_prop    = round(sd(  V_prop,      na.rm = T),  1),
            mean_log     = round(mean(V_strat_max_log, na.rm = T), 1),
            sd_log       = round(sd(  V_strat_max_log, na.rm = T), 1), 
            mean_pulp    = round(mean(V_strat_max_pulp,na.rm = T), 1),
            sd_pulp      = round(sd(  V_strat_max_pulp,na.rm = T), 1))




# Format table nicely
formated_sum_tab <- 
  sum_tab %>% 
  mutate(windRisk       = stringr::str_glue("{mean_risk}±{sd_risk}"),
         Volume         = stringr::str_glue("{mean_V}±{sd_V}"),
         Volume_top     = stringr::str_glue("{mean_V_strat}±{sd_V_strat} ({mean_V_prop}±{sd_V_prop})"),
         Volume_log     = stringr::str_glue("{mean_log}±{sd_log}"),
         Volume_pulp    = stringr::str_glue("{mean_pulp}±{sd_pulp}")) %>%  #,  {scales::percent(sd_height)}
  tidyr::complete(scenSimpl2, Management)  %>%
  dplyr::select(scenSimpl2, Management, windRisk, 
                Volume, Volume_top, Volume_log, Volume_pulp)











  
# Make a plot of volume development by NPI and Years -----------------------------------

#p.mean.V.pulp.line.time <-
  df %>% 
  group_by(scenSimpl2, 
           year,
           NPI, 
           Management) %>% 
  filter(Management != "Set Aside") %>% 
  summarize(my_y = mean(V_strat_max_log)) %>%
  ggplot(aes(y = my_y, 
             x = NPI, 
             shape = interaction(scenSimpl2, Management),     
             color = interaction(scenSimpl2, Management),   
             linetype = interaction(scenSimpl2, Management),  
             group = interaction(scenSimpl2, Management),     
             fill = interaction(scenSimpl2, Management) )) +  
  geom_line(    size = 0.9) +
  facet_wrap(.~year)  + # scenSimpl2
  ylim(0,250) +
  ggtitle("") #+
  xlab("Time") + #
  ylab("Standing pulp volume\n(mean, m^3)") +
  scale_linetype_manual(values = c( "dotted", "solid",  'dashed')) +
  scale_color_manual(values = cbp1) +
  scale_fill_manual(values = cbp1) +
  labs(shape = "Management",
       color = "Management",
       linetype = "Management",
       fill = "Management") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "right",
        strip.background =element_rect(fill="white", color = NA))
  
  
  
# CHeck wind risk over time  
  df %>% 
    group_by(scenSimpl2, 
             year,
             NPI, 
             Management) %>% 
    filter(Management != "Set Aside") %>% 
    summarize(my_y = mean(windRisk)) %>%
    ggplot(aes(y = my_y, 
               x = NPI, 
               shape = interaction(scenSimpl2, Management),     
               color = interaction(scenSimpl2, Management),   
               linetype = interaction(scenSimpl2, Management),  
               group = interaction(scenSimpl2, Management),     
               fill = interaction(scenSimpl2, Management) )) +  
    geom_line(    size = 0.9) +
    facet_wrap(.~year)  + # scenSimpl2
    #ylim(0,250) +
    ggtitle("") #+
  xlab("Time") + #
    ylab("Standing pulp volume\n(mean, m^3)") +
    scale_linetype_manual(values = c( "dotted", "solid",  'dashed')) +
    scale_color_manual(values = cbp1) +
    scale_fill_manual(values = cbp1) +
    labs(shape = "Management",
         color = "Management",
         linetype = "Management",
         fill = "Management") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
          legend.position = "right",
          strip.background =element_rect(fill="white", color = NA))
  
  
 

  
  
  

# does standing timber volume predict wind risk??? ---------------------------------------
  
  # sample the data
  # Sample  random rows:
  #df1 <- filter(df,  scenSimpl2 != "ALL")
  
  set.seed(1)
  # create the vector index of the rows to select from original large dataset
  sample_row <- sample(1:nrow(df), 50000, replace=F)  # 100000 is number of rows to subset
  # subset the selected rows from the original large.df
  df.sample <- df[sample_row,]
  
  
  
  # Make correlation plot: how does timber volume predict wind risk??
  #ggplot(df.sample, aes(x = V,
  #                     y = windRisk,
  #                    group = scenNumb,
  #                   color = scenNumb)) + 
  #geom_point() +
  #geom_smooth(method=lm, se=FALSE, formula = y ~ poly(x, 3)) + 
  #facet_grid(scenSimpl2~Management)
  
  
  
  # Risk based on pulp volume
  df.sample %>% 
    filter(Management == "Active") %>% 
    ggplot(aes(x =  V_strat_max_pulp, #V,
               y = windRisk,
               # group = scenSimpl2,
               #color = scenSimpl2,
               fill = scenSimpl2
    )) #+ 
  #geom_jitter(size = 0.3, alpha = 0.5) #+
  #geom_smooth(method=lm, se=FALSE, formula = y ~ poly(x, 2)) %>% #+ 
  #facet_grid(scenSimpl2 ~ scenNumb)
  
  

  # Make models by groups:
# does  the volume correlate with risk?# ------------------------
  # https://stackoverflow.com/questions/1169539/linear-regression-and-group-by-in-r
  
  fitted_models <- 
    df.sample %>% 
    group_by(NPI) %>% 
    do(model = lm(windRisk ~ year, data = .))
  
  
  # Check models: 
  fitted_models$model
  
  
  # Retrieve the coefficients
  fitted_models %>% broom::augment(model)
  
  
  # Get summary statistics
  fitted_models %>% broom::glance(model)
  
  
  # plot regression lines by group
  df.sample %>% 
    #filter(Management == "Active") %>% 
    ggplot(aes(x =  V, #V,
               y = windRisk, #,
               group = Management,
               color = Management  )) + 
    # geom_jitter(size = 0.1, alpha = 0.9) +
    geom_smooth(method=glm, formula = y ~ x^2, se=FALSE)  + 
    facet_grid(. ~ scenSimpl2 )
  
  
  # plot regression lines by group
  # for pulp
  p.lm.pulp <- 
    df.sample %>% 
    #filter(Management == "Active") %>% 
    ggplot(aes(x =  V_strat_max_pulp, #V,
               y = windRisk, #,
               group = scenSimpl2,
               color = scenSimpl2  )) + 
    ylim(0,0.09) +
    # geom_jitter(size = 0.1, alpha = 0.9) +
    geom_smooth(method=glm, formula = y ~ poly(x,2), se=FALSE)  + 
    scale_linetype_manual(values = c( "dotted", "solid",  'dashed')) +
    scale_color_manual(values = cbp1) +
    facet_grid(. ~ Management )
  
  
  
  
  p.lm.log <- 
    df.sample %>% 
    #filter(Management == "Active") %>% 
    ggplot(aes(x =  V_strat_max_log, #V,
               y = windRisk, #,
               group = scenSimpl2,
               color = scenSimpl2  )) + 
    ylim(0,0.09) +
    # geom_jitter(size = 0.1, alpha = 0.9) +
    geom_smooth(method=glm, formula = y ~ poly(x,2), se=FALSE)  + 
    scale_linetype_manual(values = c( "dotted", "solid",  'dashed')) +
    scale_color_manual(values = cbp1) +
    facet_grid(. ~ Management )
  
  
  

  
  windows(width = 7, height = 2.5)
  ggarrange(p.lm.pulp, p.lm.log, 
            #ncol = 2, nrow = 2,
            #widths = c(1, 1),
            common.legend = TRUE,
            align = c("hv"),
            legend="bottom",
            labels= "AUTO",
            hjust = -5,
            vjust = 3,
            font.label = list(size = 10, 
                              face = "bold", color ="black"))
  
  
  # Risk based on  log volume
  df %>%  # .sample
    filter(Management == "Active"  & scenSimpl2 != "ALL") %>% 
    ggplot(aes(x = V_strat_max_log,  #V,
               y = windRisk,
               group = scenSimpl2,
               color = scenSimpl2)) + 
    #geom_jitter(size = 0.3, alpha = 0.5) +
    #geom_smooth(method=lm, se=FALSE, formula = y ~ poly(x, 2)) #+ 
    geom_smooth() #+  # se=FALSE
  #facet_wrap(scenSimpl2 ~ .)
  
  
  df %>%  # .sample
    filter(Management == "Active"  & scenSimpl2 != "ALL") %>% 
    ggplot(aes(x = V_strat_max_pulp,  #V,
               y = windRisk,
               group = scenSimpl2,
               color = scenSimpl2)) + 
    #geom_jitter(size = 0.3, alpha = 0.5) +
    geom_smooth(method=lm, se=FALSE, formula = y ~ poly(x, 3)) #+ 
  # geom_smooth() #+  # se=FALSE
  #facet_wrap(scenSimpl2 ~ .)
  
