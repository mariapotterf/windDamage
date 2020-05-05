

# Import the calculate open_edge datasets
# Calculate wind risk for management regimes
# create ggplots & main conclusions

# ===============================


# 
rm(list = ls())


setwd("C:/MyTemp/myGitLab/windDamage")

# Read libraries

library(ggplot2)
library(dplyr)
library(tidyr)
library(rgdal)
library(ggpubr)
library(sf)
library(rgdal)
library(ggspatial)
library(rgeos)
library(raster)
library(dplyr)
library(spData)
library(sf)
library(RColorBrewer)



# Set working directory
#inDataPath = "U:/projects/2019_windthrowModel/Janita/outSimulated"
#setwd(inDataPath)


# read data with calculated open_edge
#df <- data.table::fread("open_edge_calc_fast.csv") 
df <- data.table::fread("C:/MyTemp/myGitLab/windDamage/output/df_glm.csv", data.table=FALSE)
df.rst <- data.table::fread("C:/MyTemp/myGitLab/windDamage/output/df_glm_raster.csv", data.table=FALSE)

# Read new data contaioning PEAT and mineral soild, and THIN variables
# now the data are stored by 15 stands, need to read them all
#--------------------------------------------------------


# To speed up: use the soil, peat, tHIN chanracteristics by the regime
# and replace those values in my original stand data with open)edges calculated
# (make sure that open_edge calculation fit???) I might have a different 
# SA & SA_DWexstract regimes???


head(df)
head(df.rst)


# Reclassify values:
df.new<-
  df %>% 
  mutate(PEAT.v = case_when(PEAT == 0 ~ "mineral soil",
                            PEAT == 1 ~ "peat"))  %>%
  mutate(SC.v = case_when(SC %in% 1:3 ~ "fertile",
                          SC %in% 4:6 ~ "poor")) %>%                 # COMPLETE SOIL CALSS to get mineral coarse/fine??
  mutate(soil_depth_less30 = ifelse(SOIL_CLASS == 1, TRUE,FALSE)) %>%
  mutate(soilType = case_when(SOIL_CLASS == 0 ~ "organic",
                              SOIL_CLASS %in% 1:4 ~ "mineral_coarse",
                              SOIL_CLASS %in% 5:7 ~ "mineral_fine")) %>% 
  mutate(species = case_when(MAIN_SP == 1 ~ "pine",
                             MAIN_SP == 2 ~ "spruce",
                             TRUE ~ "other")) %>% 
  mutate(H_dom = replace_na(H_dom, 0.01)) %>%  # no possible to get log(0)  
  mutate(H_dom = H_dom * 10) %>%        # Susanne values are in dm instead of meters
  # dplyr::select(my.cols.glm)  %>%      # select columns 
  mutate_if(is.character, as.factor)   # convert all characters to factor
  


# JOin df data with data derived from raster geometry
# make sure they have the same stands
my.stands<- unique(df.new$id)
df.rst <- subset(df.rst, standid %in% my.stands)
names(df.rst) <- c("id",   "area",      "avgTemp",   "windSpeed")


# Merge the datasets
df.all<- df.new %>% 
  left_join(df.rst) # %>% 
#full_join(df.rst, by = c("id" = "standid"))


# -----------------------------------------
# read stand geometry data
# ------------------------------------------

df.geom <- st_read("C:/MyTemp/avohaakut_db/14.534/14.534/mvj_14.534.shp")
df.geom <- subset(df.geom, select = c("KUVIO_ID"))
names(df.geom) <- c("standid", "geometry")
df.geom$area <- st_area(df.geom)

df.geom <- subset(df.geom, !standid %in% stands.remove)





# -----------------------------------
#
# Reorganize the input data to fit Suvanto model's requirement
# 
# -----------------------------------

# Correct order of variables, factors & levels
# need to have same names of columns?? YES
# when data are sourced (source()), they are all available in my script
source("C:/MyTemp/myGitLab/windDamage/myFunctions.R")


# Check the factors and levels in the dataset:
# keep only columns necessary for glm
keep <- c("species", 
          "H_dom", 
          "since_thin", 
          "windSpeed", 
          "open_edge",
          "soilType",
          "soil_depth_less30", 
          "SC.v",   # siteFertility
          "avgTemp")

str(df.all)


# subset only needed columns:
df.sub<-df.all[, keep]

# Rename

glm.colnames <- c("species", 
          "H_dom", 
          "time_thinning", 
          "windSpeed", 
          "open_edge",
          "soilType",
          "soilDepthLess30", 
          "siteFertility",   # siteFertility
          "tempSum")



str(df.sub)
names(df.sub) <- glm.colnames

# -----------------------------------------
# Correct the factor levels 
#   for categoric variables
# ------------------------------------
df.sub$species          <- factor(df.sub$species, 
                                  levels = c("pine", 
                                             "spruce", 
                                             "other"))

df.sub$time_thinning    <- factor(df.sub$time_thinning, 
                                  levels = c("0-5", 
                                             "6-10", 
                                             ">10"))
df.sub$open_edge        <- factor(df.sub$open_edge,
                                  levels = c("FALSE", 
                                             "TRUE"))
df.sub$soilType         <- factor(df.sub$soilType,
                                  levels = c("mineral coarse", 
                                             "mineral fine",
                                             "organic"))
df.sub$soilDepthLess30  <- factor(df.sub$soilDepthLess30, 
                                  levels = c("FALSE", 
                                             "TRUE"))
df.sub$siteFertility    <- factor(df.sub$siteFertility,
                                  levels = c("poor", 
                                             "fertile"))
df.sub$tempSum          <- df.sub$tempSum/100   # according to Susane 


# ------------------------------------------
# Calculate predicted values for wind risk 
# ------------------------------------------
# For temperature sum, I have single value: not variabilit over the landscape: 1299.273
# try to inclrease the variability??

# if teh
#df.sub$tempSum <- runif(nrow(df.sub), 
#                        min = 12, 
#                        max = 12)#*100


df.sub$windDamagePred <- predict.glm(windRisk.m,
                                     df.sub,
                                     type="response")

range(df.sub$windDamagePred, na.rm = T)
# 1.007350e-13 3.602978e-01

# Avohaakut: [1] 1.379309e-13 2.178822e-01

# Get Suvanto's predicted values???

# add wind risk values to original data to obtain year!!!
# df$windRisk <- df.sub$windDamagePred 



# -------------------------
#    Visualise results
# -------------------------
#

# get basic statistics: 
# wind risk by forest management
# by time
# does widn risk increases with % of low stands open_edge = TRUE

# keep just few regimes:
# --------------------------
# select just those regimes
tab1 <- as.data.frame(table(df$year, df$regime))

# the number or management applied over each landscape is different over time
# to have a consistent landscape: always teh same stand, with different 
# management regime, changing over time, I need to subset the sam stands
# i ned to grop by year and regime!!!

# Subset the BAU values, and it's stand IDs to get ~ 6 regimes: 276 stands
fin.stands<-as.numeric(unique(subset(df, regime == "BAU")$standid))

#fin.stands = c(3576574)

# Subset df table to have only those regimes:
# !!!!! does not sufset only few regimes
# how to make sure that my landscape is always same for all management regimes???
df.sim<-
  df %>% 
  group_by(regime, year) %>% 
  filter(all(fin.stands %in% standid)) %>%   # filter just the group group that have all values
  filter(standid %in% fin.stands)  %>%           # second to keep only specific values
  ungroup() %>% 
  mutate(regime = factor(regime))        # drop unused factors


table(df.sim$regime)






# ------------------------
# Analyse the data & 
#     Make boxplots:
# --------------------------
library(ggplot2)

# Temporal dynamics of wind risk of management
ggplot(df.sim, 
       aes(x = as.factor(year),
               y = windRisk)) +
  geom_boxplot() + 
  facet_grid(. ~ regime) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Is the wind risk the same for all regimes in the first simulation year??
p.windRisk <- ggplot(subset(df.sim, year == 2016), 
       aes(x = regime,
           y = windRisk)) +
  geom_boxplot() +
  ggtitle("Wind risk 2016") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p.H_dom <- ggplot(subset(df.sim, year == 2016), 
       aes(x = regime,
           y = H_dom)) +
  geom_boxplot() +
  ggtitle("H_dom 2016")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


p.BA <- ggplot(subset(df.sim, year == 2016), 
       aes(x = regime,
           y = BA)) +
  geom_boxplot()  +
  ggtitle("BA 2016")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggarrange(p.windRisk, p.H_dom, p.BA, 
          labels = c("A", "B", "C"),
          ncol = 3, nrow = 1)

# Why initial stand conditions are not the same 
#  between management regimes???
# ------------------------------------------
# How many stands with open edge each regime has??

# Sum up the area& count of stands with open_edge = TRUE
head(df.sim)

# how many stands with open_edge == TRUE are in every year by manage regime?

open.edge.count<- 
  df %>% 
  group_by(year, regime) %>% 
  filter(open_edge == TRUE) %>% 
  tally() %>% 
  arrange(regime)
    

ggplot(open.edge.count, 
       aes(x = as.factor(year),
           y = n, 
           group = regime,
           color = regime)) +
  geom_line() + 
 # facet_grid(. ~ regime) +
  ggtitle("Count of stands with open edge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




# Investigate when the THIN happen???
df.thin <- subset(df, regime == "BAUwT_GTR")


# Track down history of one stand:
# when does thinning occured??

subset(df.thin, standid == 12469153,
       select = c("standid", "year", "THIN"))




# Investigate when the THIN happen???
df.cc2<- subset(df, regime == "CCF_2")


# Track down history of one stand:
# when does thinning occured??

subset(df.cc2, standid == 12469153,
       select = c("standid", "year", "THIN"))



# Check for monetary values??








# Merge df data with geometry
# Join the geometry table with simulated data

# ----------------------------------------
df.bau <- subset(df.sim, regime == "BAU")
# convert factor to integer
df.geom$standid <- as.numeric(as.character(df.geom$standid ))

# Merge data together
df.bau.g<-
  df.geom %>% 
  left_join(df.bau, by = "standid") %>% 
  filter(!is.na(year)) 


# Plot the variable over years:
# Plot attribute information:
windows()
ggplot(df.bau.g) +
  geom_sf(aes(fill = windRisk)) + 
  facet_wrap(.~year)



# Get data:
library(gapminder)

# Charge libraries:
library(ggplot2)
library(gganimate)
library(transformr)


# My data:

ggplot(df.bau.g) + 
  geom_sf(aes(fill = windRisk)) +
  scale_fill_continuous(low = "lightgreen", 
                        high = "darkgreen",
                        space = "Lab", 
                        na.value = "white", guide = "colourbar")+
  
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme_bw() +
  xlab("Longitude") + 
  ylab("Latitude") +
  # theme(axis.title=element_blank(),
  #      axis.text=element_blank(),
  #     axis.ticks=element_blank()) +
  # gganimate specific bits:
  labs(title = 'WindRISK BAU Year: {current_frame}') +
  transition_manual(year) +
  #transition_time(year) +
  ease_aes('linear')




  
  
  
  







