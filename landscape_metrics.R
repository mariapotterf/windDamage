

# Calculate aggregation and landscape characteristics of teh 
# SA patches
# 

# Statistics:
# what is the % of SA by scenario?
# does o-20 Kyle's scenarios coresponds these proportions???
# does the frequency of SA differs between SA area???
# How to measure aggregation between 0-20 SA regimes????

# Investigate if:
# does % of min(H_dom) increases wind risk???

# Calculate aggregation 

# 
rm(list = ls())

# , eval = FALSE
library(data.table)
library(dplyr)
library(raster)
library(ggplot2)
library(sf)
library(stringr)
library(ggspatial)

theme_set(theme_classic())


# Read datasets:
df.all <- fread("C:/MyTemp/myGitLab/windDamage/output/df_sim_windRisk.csv")

#  -----------------------------------------
# read stand geometry data
# ------------------------------------------
# stands that are not simulated
stands.remove <-c(13243875, 
                  13243879, 
                  13243881,
                  6685176,     # # H_dom is >150 m 
                  13243960)    #  H_dom is 430



# Subset stands only for normal H_dom values
df.all <- subset(df.all, !id %in% stands.remove)


# stands geometry
# ===================================
df.geom <- st_read("C:/MyTemp/avohaakut_db/14.534/14.534/mvj_14.534.shp")
df.geom <- subset(df.geom, select = c("KUVIO_ID"))
names(df.geom) <- c("standid", "geometry")
df.geom$area <- st_area(df.geom)
#df.geom <- subset(df.geom, !standid %in% stands.remove)
df.geom <- subset(df.geom, standid %in% unique(df.all$id))
df.geom$id <- as.numeric(as.character(df.geom$standid ))


# Split regimes in two groups: SA or no_SA
df.all <- 
  df.all %>% 
  mutate(twoRegm = case_when(avohaakut == "SA" ~ "SA",
                             avohaakut != "SA" ~ "no_SA"))

# -------------------------------------
# Understand where the logging/management happened??
# -------------------------------------
# spatially, but also calculate the frequency of thinnings and 
# loggings by landscape???


# Understand how the management actipons are coded:
# for RF: THIN, cash_flow = max

# Check individual stand to understand this
# let's select stand: 6667292
df.all %>% 
  filter(id == 6667292 ) %>% 
  distinct(avohaakut)   # has all scenarios

#    avohaakut
# 1:        SA
# 2:  THwoTM20
# 3:     LRH30
# 4:     LRT30
# 5:  CCF_4_45
# 6:  CCF_3_45

df.all %>% 
  filter(id == 6667292 ) %>% 
  distinct(scenario)

# Check stand development for specific scenario and regime: RF
df.all %>% 
  filter(id == 6667292 ) %>% 
  filter(avohaakut == "LRT30") %>% 
  #distinct(scenario)
  filter(scenario == "ALL18") %>% 
  dplyr::select(year, BA, V, avohaakut, cash_flow, THIN2)

# Fror RF the year of the final cut is where V == 0


df.all %>% 
    filter(id == 6667292 ) %>% 
    filter(avohaakut == "CCF_3_45") %>% 
    filter(scenario == "CCF20") %>% 
  dplyr::select(year, BA, V, avohaakut, cash_flow, THIN2)


# Reclassify teh management actions: thin = thining, finalCut
# also THIN from above or THIN from belov? RF = below, CCF = above
# get frequency of the actions
head(df.all)

# -----------------------
# Add new values:
# ------------------------
# Category it is it RF or CCF action, or SA
# categorize thinning: above, below, finalCut
df.all<- 
  df.all %>% 
  mutate(mainRegime = case_when(
                      str_detect(avohaakut, "SA")   ~ "SA",
                      str_detect(avohaakut, "CCF_") ~ "CCF",
                      str_detect(avohaakut, "LRH")  ~ "RF",
                      str_detect(avohaakut, "LRT")  ~ "RF",
                      str_detect(avohaakut, "SR5")  ~ "RF",
                      str_detect(avohaakut, "SRT5") ~ "RF",
                      str_detect(avohaakut, "TH")   ~ "RF",
                      str_detect(avohaakut, "TT")   ~ "RF")) %>% 
  #group_by(id, scenario) %>% 
  mutate(managAction = case_when(
    mainRegime == "SA"                  ~ "SA",
    !is.na(THIN2) & mainRegime == "RF"  ~ "thinBelow",
    !is.na(THIN2) & mainRegime == "CCF" ~ "thinAbove",
    mainRegime == "RF" & V == 0         ~ "finalCut",
    TRUE ~ "no action"))# %>% 
 






# ----------------------------------
#     Prepare data for animation:
# ----------------------------------

# Example in R animate
# https://www.r-graph-gallery.com/271-ggplot2-animated-gif-chart-with-gganimate.html


# Get data:
#library(gapminder)

# Charge libraries:
library(ggplot2)
library(gganimate)
library(transformr)



# Animate harvest over selected scenario and over years:
# -------------------
# ALL8
df.sub <- 
  df.all %>%  
  filter(scenario == "ALL8") %>%   # regimes are stable over scenarios
  dplyr::select(id, year, scenario, managAction) 

# Merge with geometry data
df.sub.g <-
  df.geom %>% 
  left_join(df.sub, by = "id")


# My data:
df.sub.g %>% 
  ggplot() + 
  geom_sf(aes(fill = factor(managAction)),
              color = NA) +
  scale_fill_manual(values = c("red",           # finalCut 
                               "grey82",        # no action
                               "darkgreen",     # SA
                               "blue",          # thinAbove
                               "orange")) +  # thinBelow
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme_bw() +
  xlab("Longitude") + 
  ylab("Latitude") +
  labs(title = 'Harvest action ALL8 Year: {current_frame}') +
  transition_manual(year) +
  #transition_time(year) +
  ease_aes('linear')


# Save at gif:
anim_save("avoh_ALL8.gif")



# --------------------------------------

# Animate harvest over selected scenario and over years:
# "not_CCF8"

df.sub <- 
  df.all %>%  
  filter(scenario == "not_CCF8") %>%  
  dplyr::select(id, year, scenario, managAction) 

# Merge with geometry data
df.sub.g <-
  df.geom %>% 
  left_join(df.sub, by = "id")# %>% 


# My data:
df.sub.g %>% 
  ggplot() + 
  geom_sf(aes(fill = factor(managAction)),
          color = NA) +
  scale_fill_manual(values = c("red",           # finalCut 
                               "grey82",        # no action
                               "darkgreen",     # SA
                               "blue",          # thinAbove
                               "orange")) +  # thinBelow
  
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme_bw() +
  xlab("Longitude") + 
  ylab("Latitude") +
  # gganimate specific bits:
  labs(title = 'Harvest action not_CCF8 Year: {current_frame}') +
  transition_manual(year) +
  #transition_time(year) +
  ease_aes('linear')


# Save at gif:
anim_save("avoh_not_CCF8.gif")



# ------------------------------------------
# Animate harvest over selected scenario and over years:
# "CCF8"

df.sub <- 
  df.all %>%  
  filter(scenario == "CCF8") %>%   # regimes are stable over scenarios
  dplyr::select(id, year, scenario, managAction) 

# Merge with geometry data
df.sub.g <-
  df.geom %>% 
  left_join(df.sub, by = "id")# %>% 



# My data:
df.sub.g %>% 
  ggplot() + 
  geom_sf(aes(fill = factor(managAction)),
          color = NA) +
  scale_fill_manual(values = c(#"red",           # finalCut 
                               "grey82",        # no action
                               "darkgreen",     # SA
                               "blue" #,          # thinAbove
                               #"orange"
                               )) +   # thinBelow
  
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme_bw() +
  xlab("Longitude") + 
  ylab("Latitude") +
  # gganimate specific bits:
  labs(title = 'Harvest action CCF8 Year: {current_frame}') +
  transition_manual(year) +
  #transition_time(year) +
  ease_aes('linear')



# Save at gif:
anim_save("avoh_CCF8.gif")


 



# The question is how does the configuration of SA contributes to
# hetegorebours/homogenous H_dom among adjacent stands??? !!!!

# ===================================
#    Calculate landscape metrics
# ===================================

# Subset data to have one landscape (scenario), at one time (year)
# Merge landscape data with geometry
# convert to raster: H_dom
# calculate edge differences index:

df1 <- subset(df.all, 
              scenario == "not_CCF11" & year == 2016)

# Merge data together
df1.g<-
  df.geom %>% 
  left_join(df1, by = "id")


# Convert to raster
library(sf)
library(raster)
library(fasterize)

# Create raster with desired resolution, then fill in values
r1 <- raster::raster(df2.g, res = 16)
r1 <- fasterize(df1.g, r1, field = "H_dom" )


# Make 2nd raster (to allow camparison:)

df2 <- subset(df.all, 
              scenario == "not_CCF11" & year == 2096)

# Merge data together
df2.g<-
  df.geom %>% 
  left_join(df2, by = "id")

# Create raster with desired resolution, then fill in values
r2 <- raster(df2.g, res = 16)
r2  <- fasterize(df2.g, r2, field = "H_dom" )

# Plot both:
par(mfrow = c(2,1))
plot(r1)
plot(r2)

# Calculate landscape metrics:
# on categorical patterns: need to convert to classes?
hist(r1)
hist(r2)

# Create reclassification matrix:
reclass_m <- matrix(
              c(0, 5, 5,
                5,10, 10,
                10, 15, 15,
                15, 20, 20,
                20,25,25,
                25, Inf, 30),
                ncol = 3, 
              byrow = TRUE)

# Reclassify rasters based on matrix
r1.c <- reclassify(r1,
                   reclass_m)

r2.c <- reclassify(r2,
                   reclass_m)

windows()
par(mfrow = c(2,1))
plot(r1.c)
plot(r2.c)


# Calculate teh contrast between patches:


# Calculate edge density (landscape level)

library(landscapemetrics)

lsm_c_ed(r1.c, count_boundary = FALSE, directions = 8)
















# Merge one regime data with geometry
# ----------------------------------------------
df1 <- subset(df.sub, scenario == "not_CCF11")
# convert factor to integer

# Merge data together
df1.g<-
  df.geom %>% 
  left_join(df1, by = "id")

# Plot the variable over years:
# Plot attribute information:
windows()
ggplot(df.geom) +
  geom_sf() 

windows()
ggplot(df1.g) +
  geom_sf(aes(fill = factor(twoRegm))) +
  scale_fill_manual(values = c("darkgrey", "white"))
  #scale_fill_gradient(fill = terrain.colors(2))



plot(df1.g["twoRegm"])









# Get data:
library(gapminder)

# Charge libraries:
library(ggplot2)
library(gganimate)
library(transformr)


# My data:

ggplot(df1.g) + 
  geom_sf(aes(fill = windRisk)) +
  scale_fill_continuous(low = "lightgreen", 
                        high = "darkgreen",
                        space = "Lab", 
                        na.value = "white", guide = "colourbar")+
  
 # annotation_scale(location = "bl", width_hint = 0.4) +
 # annotation_north_arrow(location = "bl", which_north = "true", 
 #                        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
 #                        style = north_arrow_fancy_orienteering) +
  theme_bw() +
  xlab("Longitude") + 
  ylab("Latitude") +
  # theme(axis.title=element_blank(),
  #      axis.text=element_blank(),
  #     axis.ticks=element_blank()) +
  # gganimate specific bits:
  labs(title = 'WindRISK BAU Year: {current_frame}') +
  #transition_manual(scenNumb) +  #year
  transition_time(year) +
  ease_aes('linear')




































## Explore where are SA regimes over landscape??
# ------------------------------------------

# How does the % of SA per scenario affect windrisk??
# !!! ??? how to get teh % of SA by scenario???
# Count the number of stands with SA regime

prop.regimes<-
  df.all %>% 
  group_by(scenario, avohaakut) %>% 
  distinct(id) %>% 
  summarise(stands_n = n()) %>%
  mutate(freq = 100* (stands_n / sum(stands_n))) %>% 
  arrange(scenario) 

# Calculate how many differe regimes are by each scenario:
regime.n <-
  df.all %>% 
  group_by(scenario) %>% 
  distinct(avohaakut) %>% 
  summarise(regimes_n = n()) %>%
  #mutate(freq = 100* (stands_n / sum(stands_n))) %>% 
  arrange(scenario) 



# --------------------
#     Plots
# --------------------


# Plot regimes over landscape

# Subset first different scenarios (% of SA)  
# ALL0, ALL10 ALL20
# add to geom data to plot it, in year 2016 as they are constant
all0 <- df.all %>% 
  filter(scenario == "ALL0" & year == 2016)
all0.sf <- left_join(df.geom, 
                     all0, 
                     by = c("standid" = "id"))

all10 <- df.all %>% 
  filter(scenario == "ALL10" & year == 2016)
all10.sf <- left_join(df.geom, all10, by = c("standid" = "id"))
all10.sf$avokReclas <- ifelse(all10.sf$avohaakut == "SA", "SA", "other")


all20 <- df.all %>% 
  filter(scenario == "ALL20" & year == 2016)
all20.sf <- left_join(df.geom, all20, by = c("standid" = "id"))
all20.sf$avokReclas <- ifelse(all20.sf$avohaakut == "SA", "SA", "other")







