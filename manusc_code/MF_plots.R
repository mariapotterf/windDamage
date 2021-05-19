

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

#source("C:/MyTemp/myGitLab/windDamage/myFunctions.R")


# Set wd ------------------------------------------------------------------

setwd("C:/MyTemp/myGitLab/windDamage/output/even_flow")

# Read input data ----------------------------------------------------

# includes optimal scenario, raster data, windRisk
df <- fread(paste(getwd(), "finalFoPlotting.csv", sep = "/"))


# change windRisk to probabilities: multiply by 100%
df$windRisk = df$windRisk*100



# Classify NPI values into categories -------------------------------------

# get a classified NPI values: create from a DF
dd_class <- data.frame(NPI = sort(unique(df$NPI)),
                       SA_share = c(100, rep(seq(from = 95, to = 0, by = -5), each = 3)))

# Merge classified df NPI values into the all dataframe by NPI
df2 <- df %>%
  left_join(dd_class, by = c("NPI"))


# Reorder the factors: have ALL on top
df2 <- df2 %>% 
  mutate(scenSimpl2 = factor(scenSimpl2, levels = c("RF", "CCF", "ALL")))


# Include Multifunctionnality: Get points plot: MF by wind risk by scenario -------------------------------------

# Colr blind palette
# Define own palette, color blind
cbp1 <- c("#999999", # grey
          "#E69F00", # gold
          "#56B4E9", #lightblue
          "#009E73", # green
          "#F0E442", #bright yeallow
          "#0072B2", # darkblue
          "#D55E00", # warm orange
          "#CC79A7") # old rose

# Define my cols: from Protected areas
cols = c('#0072B2', # dark blue RF,
         "#999999", # grey , CCF
         #'#E69F00', # buffer 500, gold
         "red"
         #'#F0E442' # buffer 2000; yellow
         #'#000000' # control, black
)


# Create labels
npi_label  = "Net present income (k€/ha)"
risk_label = "Wind damage probability\n(mean, %)"
vol_label  = "Top stratum volume\n(mean, m3/ha)"
spruce_label = "Spruce proportion\n(%)"



# Make function to plot point plot ----------------------------------
my_pt_plot <- function() {
  list(
    geom_point(aes(size = SA_share,
                   color = scenSimpl2),
               alpha = 0.65,  
               shape = 16),
      ylim(0,3),
      ylab("Multifunctionnality"),
      scale_color_manual(name = "Scenario",
                         values = cols), 
      scale_size(range = c(0.1, 5), #_continuous
                            breaks = c(0,100),
                            name = "Net present income (k€/ha)", #npi_label, #"Net present\nincome (k€/ha)",
                            labels = c("9", "0"),
                 trans = 'reverse'),
      ggtitle(""),
      guides(size = guide_legend(reverse = TRUE, # reverse point size
                                 title.position = "top", 
                                 title.vjust = 0),
             color = guide_legend(title.position = "top",
                                  title.vjust = 0,
                                  override.aes = list(size=4, 
                                                      alpha = 1))), # increase the size of legend items
      theme(panel.border = element_rect(fill=NA,
                                        color="black", 
                                        size=0.5, 
                                        linetype="solid"),
            panel.grid.major = element_line(colour = "grey90"),
           # axis.title.x = element_blank(),
            axis.title  = element_text(size = 10, face="plain", family = "sans"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, face="plain", size = 9, family = "sans"),
            axis.text.y = element_text(face="plain", size = 9, family = "sans"))
    
  )
}


# NPI vs multifunctionnality (from Eyvindson 2021) ---------------------
p.MF.npi <- 
  df2 %>% 
  dplyr::select(scenSimpl2, 
                NPI, 
                MF, 
                SA_share) %>%  # filter the columsn as has many repetitive rows but only  some values for NPI and MF
  distinct() %>% 
 # mutate(scenSimpl2 = factor(scenSimpl2, levels = c("RF", "ALL", "CCF"))) %>% 
  ggplot(aes(y = MF,
             x =  NPI )) +  
  xlab(npi_label) +
  xlim(0,10) +
  theme(legend.position = "none") +
  my_pt_plot() 



# MF and wind risk
p.MF.risk <- 
  df2 %>% 
  group_by(scenSimpl2, 
           NPI,
           SA_share,
           MF) %>% 
  summarize(mean.risk = mean(windRisk, rm.na = T))  %>%
  ggplot(aes(y = MF, #
             x =  mean.risk           )) + 
  xlim(1,4) +
  xlab(risk_label) +
  my_pt_plot() 



# MF and top stratum volume
#windows()
p.MF_V <- 
  df2 %>% 
  group_by(scenSimpl2, 
           SA_share,
           NPI, 
           MF) %>% 
  summarize(mean.V = mean(V_strat_max, na.rm = T))  %>%
  ggplot(aes(y = MF, 
             x =  mean.V)) +  
  xlim(80,250) +
  #xlab("Top stratum volume\n(mean, m3/ha)") +
  xlab(vol_label) +
  my_pt_plot() 


# Merge MF plots together --------------------------
windows(height = 3.2, width = 7)
ggarrange(p.MF.npi, p.MF.risk, p.MF_V, 
          nrow = 1, ncol = 3,  
          common.legend = TRUE,
          legend="bottom",
          labels=list("a) ", #Net present income (k€/ha)
                      "b) ", # Wind damage probability (%)
                      "c) "), # Top stratum volume (m3/ha)"
          align = c("hv"),
          hjust = -2.5, #-5,
          vjust = 2,
          font.label = list(size = 10, 
                            face = "plain", 
                            color ="black"))





# Get overall plots with scenarios ----------------------------- 
my_ln_plot <- function() {
    list(
      geom_line(aes(color    = Management,     
                    linetype = Management),
                lwd = 1),
      ggtitle(""),
      facet_grid(.~ scenSimpl2),
      scale_linetype_manual(values = c("solid",
                                       'dashed')),
      scale_color_manual(values = c("red",
                                    "black" #'#0072B2', 
                                    )),      # "red"
      labs(color = "Management",
            linetype = "Management"),
      xlab(npi_label),
     # xlim(0,10),
      theme(panel.border = element_rect(fill=NA,
                                        color="black", 
                                        size=0.5, 
                                        linetype="solid"),
            panel.grid.major = element_line(colour = "grey95"),
            axis.title   = element_text(size = 10, face="plain", family = "sans"),
            axis.title.y = element_blank(),
            axis.text.x  = element_text(angle = 90, vjust = 0.5, face="plain", size = 9, family = "sans"),
            axis.text.y  = element_text(face="plain", size = 9, family = "sans"),
            legend.position = "right",
            strip.background =element_rect(fill="white", 
                                           color = NA))
  )
}

# Wind risk by scenario ----------------------------------------
p.risk <- df2 %>% 
  group_by(scenSimpl2, 
           Management,
           NPI, SA_share) %>%   # ,MF
  summarize(my_y = weighted.mean(windRisk, area, na.rm = T))  %>%
  ggplot(aes(y = my_y,
             x =  NPI)) + 
  ylab("") + #risk_label
  ylim(0,5)  +
  my_ln_plot()


# Exposed timber volume - remove SA? --------------------------------------

# Wind risk by scenario
p.vol <- df2 %>% 
  group_by(scenSimpl2, 
           Management,
           NPI) %>%   # ,MF
 # summarize(my_y = mean(V_strat_max, na.rm = T))  %>%
  summarize(my_y = weighted.mean(V_strat_max, area, na.rm = T)) %>% 
  ggplot(aes(y = my_y,
             x =  NPI)) +  
  my_ln_plot()+
  ylab("") + #vol_label
  ylim(0,240) # +
#xlim(0,3)
  


# Plot wind risk and timber volume ----------------------------------------


windows(height = 6, width = 6.5)
ggarrange(p.risk, p.vol, 
          nrow = 2, ncol = 1,  
          common.legend = TRUE,
          legend="bottom",
          labels=list(paste("a) ", "Wind damage probability (%)"),
                      paste("b) ", "Top stratum volume (m3/ha)")), #"auto",
          align = c("hv"),
          hjust = -0.2,
         # vjust = 2,
          font.label = list(size = 10, 
                            face = "plain", 
                            color ="black"))



# Make facet plot for Tree height, spruce proportion, thinning frequency, open_edge
# mean for SA and actively managed
# facets by scenarios
# ----------------------------------------------------------


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
# or multiply the spruce counts by area????
tot.stand.n = 1470*20


# Make plots for npi and time
p.spruce <-
  df2 %>% 
  group_by(scenSimpl2, 
           NPI, 
           Management) %>% 
  filter(species == "spruce") %>% 
  tally() %>%
  mutate(spruce_prop = n/tot.stand.n*100)  %>%  # get the proportion of spruce from all stands over 20 years 
  ggplot(aes(y = spruce_prop, 
             x = NPI)) +
  xlab(npi_label) + #
  ylab("") +
  ylim(0,50) +
  my_ln_plot()


 

## H Dom ---------------------
p.H_dom <-
  df2 %>% 
  group_by(scenSimpl2, 
           NPI, 
           Management) %>% 
  summarize(my_y = mean(H_dom,  na.rm = T)) %>%   
  ggplot(aes(y = my_y/10, 
             x = NPI)) +
  ylim(0,30) +
  xlab(npi_label) + #
  ylab("") +
  my_ln_plot()


## Time since thinning ------------------------
p.thin <-
  df2 %>% 
  filter(Management == "Active") %>% 
  group_by(scenSimpl2, 
           NPI, 
           Management) %>%  # ,
    summarize(my_y = mean(difference, na.rm = T))  %>% 
  ggplot(aes(y = my_y, 
             x = NPI)) +
  ylim(0,30) +
  xlab(npi_label) + #
  ylab("") +
 # ylab("Years since thinning\n(mean)") +
    my_ln_plot()




# Plot neighboring stand difference ---------------------------------------
p.nbrs.diff <-
  df2 %>% 
  group_by(scenSimpl2, 
           NPI, 
           Management) %>% 
  summarize(my_y = mean(mean_H_diff, na.rm = T))  %>%
  ggplot(aes(y = my_y, 
             x = NPI)) +  
  ylim(0,10) +
  ylab("") +
  #ylab("Height difference\nneighbors (mean, m)") +
  my_ln_plot()

  # !!! Get rid of all 'weighted mean values, not needed. keep only means
  
  

# Plot all predictors together --------------------------------------------
windows(height = 4.3, width = 7)
ggarrange(p.spruce, p.H_dom, p.thin, p.nbrs.diff,
          nrow = 2, ncol = 2,  
          common.legend = TRUE,
          legend="bottom",
          labels=list(paste("a) ", "Spruce proportion (%)"),
                      paste("b) ", "Tree height (m)"),
                      paste("c) ", "Years since thinning"),
                      paste("d) ", "Height difference between neighbors (m)")),
          align = c("hv"),
          hjust = -0.2,
          # vjust = 2,
          font.label = list(size = 10, 
                            face = "plain", 
                            color ="black"))



  

# Make summary tables -----------------------------------------------------



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






