
# ----------------------------------------
# process Ellinoora data for no CC, and climate change included
# ----------------------------------------

# steps:
# Get input data:
#   - get stand geometry from GPKG - already derived from previous project
#   - select one watershed from South and one from North
#   - get simulated regimes: for no change, CC4.5, CC8.5
# select which regimes to use
#     need the most stands with regimes
#     have the same stand id for geometry and for simulated data
#     if unreal values - replace by mean?
#   - use SA values in 2016 to have the same beginning for data under all regimes

# Calculate wind risk values - prepare data
#   - make landscapes by year
#   - calculate open edge and neighbours - maybe enought with TRE and False values at the beginning
#   - calculate thinning values
#   -


# How to prepare continous landscape??? if there is a missing regime, 
# i can fill in values from the closest regime for the particular stand?



# Make working example for no climate changes; then calculate values for CC scenario 
rm(list = ls())


setwd("C:/MyTemp/myGitLab/windDamage")

# Read libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(rgdal)
library(ggspatial)
library(rgeos)
library(raster)
library(dplyr)
library(spData)
library(sf)
library(RColorBrewer)


# Correct order of variables, factors & levels
# need to have same names of columns?? YES
# when data are sourced (source()), they are all available in my script
source("C:/MyTemp/myGitLab/windDamage/myFunctions.R")



# get data
# ----------------------

# Read corrected simulated names:
df.no <- data.table::fread("C:/MyTemp/myGitLab/windDamage/manuscript_regimes/input_CC/without_MV_Korsnas_rsu.csv", 
                       data.table=FALSE, 
                       stringsAsFactors = FALSE)

# Stands geometry
df.geom <- st_read(paste(getwd(), "manuscript_regimes/input_shp/Korsnas.shp", sep = "/"))
#df.geom <- subset(df.geom, select = c("KUVIO_ID"))
#names(df.geom) <- c("id", "geometry")


# Check if they have the same stand id
length(unique(df.no$id))
length(unique(df.geom$standid))

# keep  only the overlapping standid
shared.stands = intersect(unique(df.no$id), unique(df.geom$standid))



# Calculate thinning values
# ---------------------------------------------
# check values in CCF
unique(df.no$THIN)
unique(df.no$regime)


# Make a check: select one stand and one regime with thinning
df.no %>% 
  filter(regime == "BAUwT") %>% 
  distinct(id)

# Get table with thinnings
df.no2<- 
 df.no %>% 
   mutate(THIN = na_if(THIN, 0))  %>% 
   mutate(THIN2 = substring(THIN,0,4)) %>%  # keep the first 4 characters from CCF regimes, datum in format "2016-04-16" -> to "2016"
   group_by(id, regime) %>% 
   mutate(THIN_filled_lagged = lag(THIN2)) %>%
   mutate(THIN_filled_lagged = as.numeric(THIN_filled_lagged)) %>%
   tidyr::fill(THIN_filled_lagged) %>% 
   mutate(difference = year - THIN_filled_lagged) %>% 
   mutate(since_thin = case_when(is.na(difference) | difference < 0 ~ ">10",
                                 difference %in% c(0:5) ~ "0-5",
                                 difference %in% c(6:10) ~ "6-10",
                                 difference > 10 ~ ">10")) # %>% 
   #filter(id == 3576736 & regime == "BAUwT") %>% 
  # filter(id == 3576736 & regime == "CCF_1") %>% 
#   dplyr::select(id, year, regime, THIN, difference, since_thin)
   # Remove unnecessary columns
 #  dplyr::select(-branching_group, 
  #               -regime.x,
   #              -regime.y)
 
 
# Inspect how many stands I have per regime?
df.no2 %>%
  filter(year == 2016) %>% 
  group_by(regime) %>% 
  distinct(id) %>% 
  tally() %>% 
  print(n = 40)
 
# total number of stands:
stand_n = length(unique(df.no$id))


# total n of stands = 299

# how many stands have all regimes???
df.no2 %>%
  filter(year == 2016) %>% 
  group_by(id) %>% 
  distinct(regime) %>% 
  tally() %>% 
  filter(n<15) # n<5
  #print(n = 40)

# about 150 stands have >20 regimes, but how consistent it is?
# how to fill in stand gaps to recreate continoous landscaes?
# 19 stands have less regimes then 5 

# Let's say that I can fill in gaps by the closest regime:
# how many stands do I have for BAU, SA, CCF_1?
# BAU   = 278
# CCF_1 = 264
# SA    = 299

# do all BAU stands have have al CCF regimes??
# subset df.no$regime == "BAU"
df.bau <- df.no %>% 
  filter(regime == "BAU") %>% 
  distinct(id)

df.ccf <- df.no %>% 
  filter(regime == "CCF_1") %>% 
  distinct(id)


# get the vector of ids
bau_id = df.bau$id
ccf_id = df.ccf$id


# check number of CCF regimes per BAU
df.no %>% 
  filter(id %in% bau.id) %>% 
  filter(regime == "CCF_1") %>% 
  distinct(id) %>% 
  tally()


# why some stands are not suitable for BAU??
#separate in two groups: BAU, only SA and compare age, V, tree height
# subset only stands that are not in BAU
all_id = unique(df.no$id)

# keep only stands that have just SA regime:
# keep  only the overlapping standid
(sa_id_bau = setdiff(all_id, bau_id))
(sa_id_ccf = setdiff(all_id, ccf_id))

# https://stackoverflow.com/questions/31573087/difference-between-two-vectors-in-r

# split 
df.sa<- df.no %>% 
  filter(regime == "SA_DWextract" & year == 2016 & id %in% sa_id_bau)  %>% 
  mutate(type = "onlySA")


df.bau<- df.no %>% 
  filter(regime == "BAU" & year == 2016 & id %in% bau_id) %>% 
  mutate(type = "active_BAU")


df2 <- rbind(df.sa, df.bau)

# make a plot for Age
df2 %>% 
  ggplot(aes(y = Age,
             x = type)) +
  geom_boxplot()


# Get the same for CCF?
# split 
df.sa<- df.no %>% 
  filter(regime == "SA_DWextract" & year == 2016 & id %in% sa_id)  %>% 
  mutate(type = "onlySA")


df.bau<- df.no %>% 
  filter(regime == "BAU" & year == 2016 & id %in% bau_id) %>% 
  mutate(type = "active_BAU")


df2 <- rbind(df.sa, df.bau)

