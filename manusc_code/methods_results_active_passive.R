
# ----------------------------
# Results and methods for paper
# -----------------------------
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


theme_set(theme_classic())
theme_update(panel.grid.major = element_line(colour = "grey95",
                                             size = 0.1,
                                             linetype = 2),
             strip.background = element_rect(color="grey95", 
                                             fill="grey95",
                                             size=0.1, 
                                             linetype="solid"))



# Read input data
# =========================
#df <- fread("/projappl/project_2003256/windDamage/output/final_df_solution8.csv")
df <- fread(paste(getwd(), "output/even_flow/final_df_solution8_3.csv", sep = "/"))



# stands geometry
df.geom <- st_read(paste0(getwd(),"/14.534/14.534/mvj_14.534.shp"))
df.geom <- subset(df.geom, select = c("KUVIO_ID"))
names(df.geom) <- c("id", "geometry")
df.geom$area <- st_area(df.geom)
df.geom <- subset(df.geom, id %in% unique(df$id))



# replace all NA in volume by 0 - because not volume is available there
df<- 
  df %>% 
  dplyr::mutate(V_stand_log      = replace_na(V_stand_log, 0)) %>% 
  dplyr::mutate(V_stand_pulp     = replace_na(V_stand_pulp, 0)) %>% 
  dplyr::mutate(V_strat_max      = replace_na(V_strat_max, 0)) %>% 
  dplyr::mutate(V_strat_max_log  = replace_na(V_strat_max_log, 0)) %>% 
  dplyr::mutate(V_strat_max_pulp = replace_na(V_strat_max_pulp, 0)) %>% 
  dplyr::mutate(Harvested_V_log_under_bark = replace_na(Harvested_V_log_under_bark, 0)) %>% 
  dplyr::mutate(Harvested_V_pulp_under_bark = replace_na(Harvested_V_pulp_under_bark, 0)) 

# DEfine SA and no_SA regimes:
# Create two regimes: SA and non-SA"
df <- 
  df %>% 
  mutate(Management = case_when(avohaakut == "SA" ~ "Set Aside",
                                avohaakut != "SA" ~ "Active"))


# Calculate the proportion % between V total and V at teh top stratum 
# -----------------------------
df <- df %>% 
  mutate(V_prop = V_strat_max / V *100)  




# --------------------------
# Define the plotting 
# --------------------------

# DEfine own palette
# color blind
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


plot_line_details <- function() {
  list(
    geom_line(    size = 0.9),
    facet_wrap(.~Management), 
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
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
          legend.position = "right",
          strip.background =element_rect(fill="white", 
                                         color = NA))
  )
}






# -------------------------
# make plots with means 
# -------------------------
# ---------------------------------
# try a plot where they will all be in a same plot: different color lines
# maybe exclude SA?
# --------------------------------

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
  ylim(0,0.06) +
  facet_wrap(.~Management)  + # scenSimpl2
  xlab("Net present income\n(k€/ha)") + #
  ylab("Wind damage probability\n(mean, %)") +
  plot_line_details()

 # ggtitle("") +
#  scale_linetype_manual(values = c( "dotted", "solid",  'dashed')) +
 # scale_color_manual(values = cbp1) +
  #scale_fill_manual(values = cbp1) +
  #labs(shape = "Management",
   #    color = "Management",
    #   linetype = "Management",
    #   fill = "Management") +
#  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
 #       legend.position = "right",
  #      strip.background =element_rect(fill="white", color = NA))



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
  ylim(0,0.06) +
  xlab("\nTime") + #
  ylab("Wind damage probability\n(mean, %)") +
  plot_line_details()



# -----------------------------
# Plot by managed/unmanaged stands!!!
# ===============================
windows(width = 7, height=3)
ggarrange(p.mean.windRisk.line.npi2, 
          p.mean.windRisk.line.time2,  
          ncol = 2, nrow = 1,
          widths = c(1, 1),
          common.legend = TRUE,
          align = c("hv"),
          legend="bottom",
          labels= "AUTO",
          hjust = -5,
          vjust = 3,
          font.label = list(size = 10, 
                            face = "bold", color ="black"))




# Top layer standing timber at risk??
# Plot log volume:
# --------------------------
p.mean.V.log.line.npi <-
  df %>% 
  group_by(scenSimpl2, 
           NPI, 
           Management) %>% 
  summarize(my_y = mean(V_strat_max_log ))  %>%
  ggplot(aes(y = my_y, 
             x = NPI, 
             shape = scenSimpl2,     
             color = scenSimpl2,     
             linetype = scenSimpl2,  
             group = scenSimpl2,     
             fill = scenSimpl2 )) +  
  ylim(0,210) +
  xlab("Net present income\n(k€/ha)") + #
  ylab("Standing log volume\n(mean, m^3)") +
  plot_line_details()


# mean V log over time
# --------------------------
p.mean.V.log.line.time <-
  df %>% 
  group_by(scenSimpl2, 
           year, 
           Management) %>% 
  summarize(my_y = mean(V_strat_max_log))  %>%
  ggplot(aes(y = my_y, 
             x = year, 
             shape = scenSimpl2,     
             color = scenSimpl2,     
             linetype = scenSimpl2,  
             group = scenSimpl2,     
             fill = scenSimpl2 )) +  
  ylim(0,210) +
  xlab("Time\n ") + #
  ylab("Standing log volume\n(mean, m^3)") +
  plot_line_details()



# Pulp volume
# ===================

p.mean.V.pulp.line.npi <-
  df %>% 
  group_by(scenSimpl2, 
           NPI, 
           Management) %>% 
  summarize(my_y = mean(V_strat_max_pulp ))  %>%
  ggplot(aes(y = my_y, 
             x = NPI, 
             shape = scenSimpl2,     
             color = scenSimpl2,     
             linetype = scenSimpl2,  
             group = scenSimpl2,     
             fill = scenSimpl2 )) +  
  ylim(0,210) +
  xlab("Net present income\n(k€/ha)") + #
  ylab("Standing pulp volume\n(mean, m^3)") +
  plot_line_details()


# mean V pulp over time
# --------------------------
p.mean.V.pulp.line.time <-
  df %>% 
  group_by(scenSimpl2, 
           year, 
           Management) %>% 
  summarize(my_y = mean(V_strat_max_pulp ))  %>%
  ggplot(aes(y = my_y, 
             x = year, 
             shape = scenSimpl2,     
             color = scenSimpl2,     
             linetype = scenSimpl2,  
             group = scenSimpl2,     
             fill = scenSimpl2 )) +  
  ylim(0,210) +
  xlab("Time\n") + #
  ylab("Standing pulp volume\n(mean, m^3)") +
  plot_line_details()

# The log and pulp timber volume over NPI and time at one pot 
windows(width = 7, height = 6)
ggarrange(p.mean.V.pulp.line.npi, p.mean.V.pulp.line.time, 
          p.mean.V.log.line.npi, p.mean.V.log.line.time, 
          ncol = 2, nrow = 2,
          #widths = c(1, 1),
          common.legend = TRUE,
          align = c("hv"),
          legend="bottom",
          labels= "AUTO",
          hjust = -5,
          vjust = 3,
          font.label = list(size = 10, 
                            face = "bold", color ="black"))




# How does the % of the volume change between SA and regimes, and among scenarios??
# -----------------------------------
  

# Make plots with proportions, total volum, total top volume,
# can dgo to supplementary material
# -----------------------------




# Total volume
# --------------------
p.mean.V.line.npi <-
  df %>% 
  group_by(scenSimpl2, 
           NPI, 
           Management) %>% 
  summarize(my_y = mean(V, na.rm =T ))  %>% # there are NA if V and V_strat_max = 0
  ggplot(aes(y = my_y, 
             x = NPI, 
             shape = scenSimpl2,     
             color = scenSimpl2,     
             linetype = scenSimpl2,  
             group = scenSimpl2,     
             fill = scenSimpl2 )) +  
  ylim(0,350) +
  xlab("Net present income\n(k€/ha)") + #
  ylab("Total stand volume\n(mean, m^3)") +
  plot_line_details()

p.mean.V.line.time <-
  df %>% 
  group_by(scenSimpl2, 
           year, 
           Management) %>% 
  summarize(my_y = mean(V, na.rm =T ))  %>% # there are NA if V and V_strat_max = 0
  ggplot(aes(y = my_y, 
             x = year, 
             shape = scenSimpl2,     
             color = scenSimpl2,     
             linetype = scenSimpl2,  
             group = scenSimpl2,     
             fill = scenSimpl2 )) +  
  ylim(0,350) +
  xlab("Time\n") + #
  ylab("Total stand volume\n(mean, m^3)") +
  plot_line_details()


# -----------------
# V at top layer
# -----------------

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
  xlab("Net present income\n(k€/ha)") + #
  ylab("Top stratum volume\n(mean, m^3)") +
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
  xlab("Time\n") + #
  ylab("Top stratum volume\n(mean, m^3)") +
  plot_line_details()

  
  
# Plot proportions
# ----------------------
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
  xlab("Net present income\n(k€/ha)") + #
  ylab("Rate top:total volume\n(mean, %)") +
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
  xlab("Time\n") + #
  ylab("Rate top:total volume\n(mean, %)") +
  plot_line_details()



# The Volume proportion top vs total over NPI and time 
# --------------------------------------------
windows(width = 7, height = 5.5)
ggarrange(#p.mean.V.line.npi, p.mean.V.line.time,
          p.mean.V_stratum.line.npi, p.mean.V_stratum.line.time, 
          p.mean.V_prop.line.npi, p.mean.V_prop.line.time, 
          ncol = 2, nrow = 2,
          #widths = c(1, 1),
          common.legend = TRUE,
          align = c("hv"),
          legend="bottom",
          labels= "AUTO",
          hjust = -5,
          vjust = 3,
          font.label = list(size = 10, 
                            face = "bold", color ="black"))




# ----------------------------
#  Get summary statistics
# ---------------------------
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




# -------------------------------------
# Explain why???
# ----------------------------------

# Tree species
# tree height
# Time since thinning
# open-neighbour


# Tree species:
# -------------------------
# Spruce is the most vulnerable: frequency of spruce

# Calculate the mean # of spruces!!
# over time and npi

# Calculate the mean # of spruces!! Why mean???
# How many stand I have for each simple scen vs NPI?


df %>% 
  group_by(scenSimpl2, 
           NPI) %>% 
  tally() %>%
  print(n = 70)

# different number of stands by combination
# ranges from 29400 to 29280 - caulculate the % from the 29400 stands
tot.stand.n = 1470*20


# Make plots for npi and time
# -----------------------
p.spruce.ratio.npi <-
  df %>% 
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
  xlab("Net present income\n(k€/ha)") + #
  ylab("Tree height\n(mean, dm)") +
  ylim(0,40) +
  xlab("Net present income\n(k€/ha)") + #
  ylab("Spruce proportion\n(%)") +
  plot_line_details()


p.spruce.ratio.time <-
  df %>% 
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
  ylim(0,40) +
  xlab("Time\n") + #
  ylab("Spruce proportion\n(%)") +
  plot_line_details()
  
 

# H Dom
# ---------------------
p.mean.H_dom.npi <-
  df %>% 
  group_by(scenSimpl2, 
           NPI, 
           Management) %>% 
  summarize(my_y = mean(H_dom))  %>%
  ggplot(aes(y = my_y, 
             x = NPI, 
             shape = scenSimpl2,
             color = scenSimpl2,
             linetype = scenSimpl2,
             group = scenSimpl2,
             fill = scenSimpl2)) +
  ylim(0,300) +
  ggtitle("") +
  xlab("Net present income\n(k€/ha)") + #
  ylab("Tree height\n(mean, cm)") +
  plot_line_details()



p.mean.H_dom.time <-
  df %>% 
  group_by(scenSimpl2, 
           year, 
           Management) %>% 
  summarize(my_y = mean(H_dom ))  %>%
  ggplot(aes(y = my_y, 
             x = year, 
             shape = scenSimpl2,
             color = scenSimpl2,
             linetype = scenSimpl2,
             group = scenSimpl2,
             fill = scenSimpl2)) +
  ylim(0,300) +
  ggtitle("") +
  xlab("Time\n ") + #
  ylab("Tree height\n(mean, cm)") +
  plot_line_details()



# -----------------------
# Time since thinning:
# ------------------------
# get the difference by landscape???
# replace NA by 0?
# get mean landscape differences by NPI, by time
# how to calculate the time since thinning? for NPi, for time??
# calculate weighted mean??

p.mean.thin.npi <-
  df %>% 
  group_by(scenSimpl2, 
           NPI, 
           Management,
           difference) %>% 
  tally() %>%
  summarize(my_y = weighted.mean(difference, n, na.rm = T)) %>% 
  ggplot(aes(y = my_y, 
             x = NPI, 
             shape = scenSimpl2,
             color = scenSimpl2,
             linetype = scenSimpl2,
             group = scenSimpl2,
             fill = scenSimpl2)) +
  ylim(0,30) +
  xlab("Net present income\n(k€/ha)") + #
  ylab("Years since thinning\n(weigh. mean)") +
  plot_line_details()

# Mean thinning over time
# ------------------
p.mean.thin.time <-
  df %>% 
  group_by(scenSimpl2, 
           year, 
           Management,
           difference) %>% 
  tally()  %>%
  summarize(my_y = weighted.mean(difference, n, na.rm = T)) %>% 
  ggplot(aes(y = my_y, 
             x = year, 
             shape = scenSimpl2,
             color = scenSimpl2,
             linetype = scenSimpl2,
             group = scenSimpl2,
             fill = scenSimpl2)) +
  ylim(0,30) +
  xlab("Time\n ") + #
  ylab("Years since thinning\n(weigh. mean)") +
  plot_line_details()


# ???
# Or, what is the proportion of stands that get thinned???
# !!!!???


# Open_edge frequency
# ---------------------------
p.mean.open.edge.npi <-
  df %>% 
  group_by(scenSimpl2, 
           NPI, 
           Management,
           open_edge) %>%
  tally() %>%
  filter(open_edge == TRUE) %>% 
  summarize(my_y = n/1470/20) %>% 
  ggplot(aes(y = my_y, 
             x = NPI, 
             shape = scenSimpl2,
             color = scenSimpl2,
             linetype = scenSimpl2,
             group = scenSimpl2,
             fill = scenSimpl2)) +
  ylim(0,2) +
  xlab("Net present income\n(k€/ha)") + #
  ylab("Open edge stands\n(%)") +
  plot_line_details()



# Mean thinning over time
# ------------------
p.mean.open.edge.time <-
  df %>% 
  group_by(scenSimpl2, 
           year, 
           Management,
           open_edge) %>%
  tally() %>%
  filter(open_edge == TRUE) %>% 
  summarize(my_y = n/1470/20) %>% 
  ggplot(aes(y = my_y, 
             x = year, 
             shape = scenSimpl2,
             color = scenSimpl2,
             linetype = scenSimpl2,
             group = scenSimpl2,
             fill = scenSimpl2)) +
  ylim(0,2) +
  xlab("Time\n ") + #
  ylab("Open edge stands\n(%)") +
  plot_line_details()
  

# --------------------
# Plot all together
# -------------------------

windows(width = 7.4, height = 10)
ggarrange(p.spruce.ratio.npi, p.spruce.ratio.time,
          p.mean.H_dom.npi,   p.mean.H_dom.time,
          p.mean.thin.npi,    p.mean.thin.time,
          p.mean.open.edge.npi, p.mean.open.edge.time,
          ncol = 2, nrow = 4,
          #widths = c(1, 1),
          common.legend = TRUE,
          align = c("hv"),
          legend="bottom",
          labels= "AUTO",
          hjust = -5,
          vjust = 3,
          font.label = list(size = 10, 
                            face = "bold", color ="black"))




# CHeck the open_ edge counts over time??


#p.mean.open.edge.time <-
  df %>% 
    filter(scenNumb == 10) %>% 
  group_by(scenSimpl2, 
           year, 
           Management,
           open_edge) %>%
  tally() %>%
  filter(open_edge == TRUE) %>% 
  summarize(my_y = n ) %>% # /1470/20 
  ggplot(aes(y = my_y, 
             x = year, 
             shape = scenSimpl2,
             color = scenSimpl2,
             linetype = scenSimpl2,
             group = scenSimpl2,
             fill = scenSimpl2)) +
  geom_line(size = 0.9) +
  facet_wrap(.~Management) +
 # ylim(0,2) +
  ggtitle("") +
  xlab("Time\n ") + #
  ylab("Open edge stands\n(%)") +
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



# how does the range of tree height changes oevr time???
  # get range of values, does it lowers ove time???

  df %>% 
    group_by(scenSimpl2, 
             NPI, 
             Management ) %>%  #,
    #open_edge) %>%
    summarise(H_min = min(H_dom, na.rm = T),
              H_max = max(H_dom, na.rm = T)) %>% 
    mutate(H_diff = H_max - H_min) %>% 
    ggplot(aes(y = H_diff, 
               x = NPI, 
               shape = scenSimpl2,
               color = scenSimpl2,
               linetype = scenSimpl2,
               group = scenSimpl2,
               fill = scenSimpl2)) +
    geom_line(size = 0.9) +
    facet_wrap(.~Management) + #+
    ylim(0, 350)
  
  
  
  
  
  
  df %>% 
  group_by(scenSimpl2, 
           year, 
           Management ) %>%  #,
           #open_edge) %>%
  summarise(H_min = min(H_dom, na.rm = T),
            H_max = max(H_dom, na.rm = T)) %>% 
  mutate(H_diff = H_max - H_min) %>% 
  ggplot(aes(y = H_diff, 
             x = year, 
             shape = scenSimpl2,
             color = scenSimpl2,
             linetype = scenSimpl2,
             group = scenSimpl2,
             fill = scenSimpl2)) +
  geom_line(size = 0.9) +
  facet_wrap(.~Management) + #+
  ylim(0, 350)
  
  
# How to illustrate teh difference in tre heights distributions over time??
  




# -------------------------------
# Get summary statistic tables
# ------------------------------
# get summary statistics
#sum_tab_vars<-
  df %>% 
  group_by(scenSimpl2,  
           Management) %>% 
  summarize(mean_H_dom = round(mean(H_dom),1),
            sd_H_dom = round(sd(H_dom),1) ) #, 
            #mean_pulp = round(mean(V_strat_max_pulp),1),
            #sd_pulp = round(sd(V_strat_max_pulp), 1))




# ------------------------------------
# Make a plot of volume development by NPI and Years
# -----------------------------------

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
  xlab("Time\n") + #
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
  xlab("Time\n") + #
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
  
  
  
 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  
  
  
  
  
  
  
  
  
  
  
  
  # --------------------------------------
  
  
  
  # ---------------------------------------
  # does standing timber volume predict wind risk???
  # ---------------------------------------
  
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
  
  
  # --------------------------
  # Make models by groups:
  # does  the volume correlate with risk?
  # ------------------------
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
  
  
  
  # ------------------------
  
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
  
