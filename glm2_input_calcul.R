
# ------------------------------
#         GML for wind risk
# ------------------------------

# include simulated data preprocessing:
#   - identify foret management for all stands
#   - calculate open_edge from spatial datasets
#   - convert teh data into simple dataframe

# get Suvanto's formula
# reorganize the input data: 
#   - correct factor levels in categorical variables
#   - calculate wind risk by year & management
# ------------------------

# 
rm(list = ls())


setwd("C:/MyTemp/myGitLab/windDamage")

source("myFunctions.R")


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
inDataPath = "U:/projects/2019_windthrowModel/Janita/outSimulated"
setwd(inDataPath)

# # read simulated data
# without == climate change is not included
df <- read.csv("rsl_without_MV_Korsnas.csv", sep = ";") 

# st_read do not return tibble just spatial dataframe
df.geom = st_read("outKorsnas_att.shp")

# CHeck attributes that I have:
names(df.geom)

keep.geom<- c("standid",
              "mntrspc",   
              "soiltyp", 
              "area",
              "avgTemp",
              "windSpd",
              "solDpth",
              "slFrtlC")

# Keep only necessary columns
df.geom <- df.geom %>% 
  dplyr::select(keep.geom)


# Find same stands id between geometry and simulated data 
# and keep those
stands.complete = Reduce(intersect, list(df$id, df.geom$standid))

# Reduce df and df.geom to the same stands
df      <- subset(df,id %in% stands.complete)
df.geom <- subset(df.geom, standid %in% stands.complete)



# -----------------------------------------------
# Which stands have the same amout on executed regimes on them?
# Idea: have teh largest landscape under one management at every year
# recreate the constant landscape over year and management
# subset stands that have 20 regimes on them
# OR subset that the regimes that have the most stands?
# -----------------------------------------------
# how many management I have every year?

# select just those regimes
tab1 <- as.data.frame(table(df$year, df$regime))

# the number or management applied over each landscape is different over time
# to have a consistent landscape: always teh same stand, with different 
# management regime, changing over time, I need to subset the sam stands
# how many management I have every year???
table(tab1$Var1, tab1$Var2)

# How oftern every management occurs?
table(tab1$Var2, tab1$Freq)

# Subset the BAU values, and it's stand IDs to get ~ 6 regimes: 276 stands
fin.stands<-unique(subset(df, regime == "BAU")$id)

# Subset df table to have only those regimes:
df.sim<-
  df %>% 
  filter(id %in% fin.stands) %>% 
  mutate(regime = factor(regime))        # drop unused factors


# Subset one FM regime from simulated data
#df.bau<-
 # df %>% 
 # filter(regime == "BAU") %>% 
 # mutate(regime = factor(regime))        # drop unused factors


# Merge geometry and simulated data for one management regime
stand.merged <- sp::merge(df.geom,
                          df.sim, 
                          duplicateGeoms = TRUE,
                          by.x = "standid", 
                          by.y = "id")


# Replace H_dom missing values by 0
stand.merged <-
  stand.merged %>% 
  mutate(H_dom = replace_na(H_dom, 0.01))


# Split dataframe into multiple dataframes list 
# Excecute the function on each of dataframe
# need to get out objects of sf an dataframe
# Need to split by year and management regime!!!
# ===========================================

# Combine the years and management regimes to have 
# a category to split the dataframe
stand.merged$reg_year <- paste(stand.merged$regime, 
                               stand.merged$year,
                               sep = "_")


# split dataframe into list of dataframes
out.split <- split(stand.merged, 
                   f = stand.merged$reg_year)


# drop unused factors - need to finnish if needed
#out.split <- lapply(out.split, 
 #                   function(df) {df$regime <- factor(df$regime)})

# out2 <- findOpenEdge_sf(out.split[[6]])
# Apply a function over a dataframe list
# !!!!! runniong for 10 mins?? started at 10:45
# I have ~ 6 regimes * 20 time steps = 120
# previously: 20 time steps were genereated within 2-5 min??
out.fin <- lapply(out.split, findOpenEdge_sf)


out.df <- Reduce(rbind, out.fin)
# How to convert list of df to df while keeping geometry???
#out.edge.df<- sf::st_as_sf(data.table::rbindlist(out.fin))

# BInd the list of dataframes together, no need to keep geometries anymore

# export the dataframe

write.csv(out.df, "open_edge_calc.csv")





