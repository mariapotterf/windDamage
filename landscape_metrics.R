

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
df.geom <- st_read("C:/MyTemp/avohaakut_db/14.534/14.534/mvj_14.534.shp")
df.geom <- subset(df.geom, select = c("KUVIO_ID"))
names(df.geom) <- c("standid", "geometry")
df.geom$area <- st_area(df.geom)
#df.geom <- subset(df.geom, !standid %in% stands.remove)
df.geom <- subset(df.geom, standid %in% unique(df.all$id))
df.geom$id <- as.numeric(as.character(df.geom$standid ))


# Split df.all data into simpleScenario and SA proportion (0-20):
# Split the string with numbers and characters into string and numbers:
df.all <- 
  df.all %>% 
  tidyr::extract(scenario, 
                 c('scenSimpl2', 'scenNumb'), 
                 '(.*?)(\\d+)', 
                 remove = FALSE) %>% 
  mutate(scenNumb = as.numeric(scenNumb)) %>% 
  mutate(twoRegm = case_when(avohaakut == "SA" ~ "SA",
                             avohaakut != "SA" ~ "no_SA"))


# ----------------------------------
#     Prepare data for animation:
# ----------------------------------


# Animate SA regimes over 63 scenarios:
# Need to restructure the table: 
# each has id, scenario (63) and regime: SA, noSA
# split those into individual landscapes
# question is: are SA always the same stands among scenarios, or not?? not

df.sub <- df.all %>%  
  filter(year == 2016 & simpleScen == "RF") %>%   # regimes are stable over scenarios
  dplyr::select(id, scenSimpl2, scenNumb, twoRegm)


# Check it out:
length(unique(df.sub$id)) # 1470
unique(df.sub$year)   # 2016

# Merge all RF scenarios with 
df.sub.g <-
  df.geom %>% 
  left_join(df.sub, by = "id")# %>% 
  

# SUPER SLOW RENDERING!!!!!!!!!
windows()
out.p<- 
  ggplot(df.sub.g) +
  geom_sf(aes(fill = factor(twoRegm)), colour = NA) +
  scale_fill_manual(values = c("grey92", "red")) +
  facet_wrap(.~scenNumb)
#scale_fill_gradient(fill = terrain.colors(2))


windows()
out.p 
  

# CCF
# -----------------

df.sub <- df.all %>%  
  filter(year == 2016 & simpleScen == "CCF") %>%   # regimes are stable over scenarios
  dplyr::select(id, scenSimpl2, scenNumb, twoRegm)


# Check it out:
length(unique(df.sub$id)) # 1470
unique(df.sub$year)   # 2016

# Merge all RF scenarios with 
df.sub.g <-
  df.geom %>% 
  left_join(df.sub, by = "id")# %>% 


# SUPER SLOW RENDERING!!!!!!!!!
windows()
out.p<- 
  ggplot(df.sub.g) +
  geom_sf(aes(fill = factor(twoRegm)), colour = NA) +
  scale_fill_manual(values = c("grey92", "red")) +
  facet_wrap(.~scenNumb)
#scale_fill_gradient(fill = terrain.colors(2))


windows()
out.p 


# ALL
# -----------

# CCF
# -----------------

df.sub <- df.all %>%  
  filter(year == 2016 & simpleScen == "ALL") %>%   # regimes are stable over scenarios
  dplyr::select(id, scenSimpl2, scenNumb, twoRegm)


# Check it out:
length(unique(df.sub$id)) # 1470
unique(df.sub$year)   # 2016

# Merge all RF scenarios with 
df.sub.g <-
  df.geom %>% 
  left_join(df.sub, by = "id")# %>% 


# SUPER SLOW RENDERING!!!!!!!!!
windows()
out.p<- 
  ggplot(df.sub.g) +
  geom_sf(aes(fill = factor(twoRegm)), colour = NA) +
  scale_fill_manual(values = c("grey92", "red")) +
  facet_wrap(.~scenNumb) + 
  theme(legend.position = "bottom")
#scale_fill_gradient(fill = terrain.colors(2))


windows()
out.p



# The question is how does the configuration of SA contributes to
# hetegorebours/homogenous H_dom among adjacent stands??? !!!!


























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







