
# ------------------------------
#         GML for wind risk
# ------------------------------

# get Suvanto's formula
# get input geometry and stand attribute data
# reorganize the table 
# calculate wind risk for one year at time

# ------------------------

# 
rm(list = ls())


setwd("C:/MyTemp/myGitLab/windDamage")

source("myFunctions.R")


# ----------------------------------
# start the script: using rgdal library
# ----------------------------------

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
df <- read.csv("rsl_without_MV_Korsnas.csv", sep = ";")  # without == climate change is not included
# 
# # Read stand geometry b!!!needs to be changed!!!
#df.geom = read_sf("U:/projects/2019_windthrowModel/Janita/outSimulatedoutKorsnas_att.shp",
 #                 as_tibble = FALSE)

# st_read do not return tibble just spatial dataframe
df.geom = st_read("outKorsnas_att.shp")


# CHeck attributes that I have:
names(df.geom)

keep<- c("standid", "maingrp", "subgrop", "soiltyp", "area",
         "avgTemp",
         "windSpd",
         "solDpth",
         "slFrtlC")

# Keep only necessary columns
df.geom <- df.geom %>% 
  dplyr::select(keep)

# Find same stands id between geometry and simulated data 
# and keep those
stands.complete = Reduce(intersect, list(df$id, df.geom$standid))

# Reduce df and df.geom to the same stands
df <- subset(df,id %in% stands.complete)
df.geom <- subset(df.geom, standid %in% stands.complete)


# Subset one FM regime from simulated data
df.bau<-
  df %>% 
  filter(regime == "BAU") %>% 
  mutate(regime = factor(regime))        # drop unused factors


# Merge geometry and simulated BAU data
stand.merged <- sp::merge(df.geom,
                          df.bau, 
                          duplicateGeoms = TRUE,
                          by.x = "standid", by.y = "id")


# !!!! Add missing columns:
# create fake data!!!!
# -----------------------------------
stand.merged$time_thinning <- factor(sample(c("0-5", "6-10", ">10"),
                                        nrow(stand.merged), replace = TRUE), 
                                 levels = c("0-5", "6-10", ">10"))
stand.merged$soiltyp <- sample(c("mineral coarse", "mineral fine",
                                 "organic"),
                               nrow(stand.merged), replace = TRUE)

# Subset columns crucial for glm()
my.cols.glm <- c("standid",
                 "area",
                 "year",
                 "species",
                 "H_dom", 
                 "time_thinning",
                 "windSpd",
                 "soiltyp",         
                 "solDpth",
                 "slFrtlC", 
                 "avgTemp")

# Sprocess teh data: change species numbers, 
# change NA in H_dom to 0
# !!!!! heck the conversion to factor!!!


stand.merged<-
  stand.merged %>% 
  mutate(species2 = as.character(maingrp)) %>% 
  mutate(species = case_when(
    species2 == "1" ~ "pine",
    species2 == "2" ~ "spruce",
    TRUE ~ "other")) %>% 
  mutate(H_dom = replace_na(H_dom, 0)) %>% 
  mutate(species = as.factor(species))
  
# filter the columns
stand.merged <-
  stand.merged %>%
  dplyr::select(my.cols.glm)


# subset just one year
stand.merged.2016 <- 
  stand.merged %>% 
  filter(year == 2016) 



# Calculate open edge:
stand.merged.2016<- findOpenEdge_sf(stand.merged.2016, H_dom, distance = 40, pixel.width = 16)



# Restructure the data into categorical and quantitative data
# ----------------------------------------------
# Convert categorical into binary
categVars <- c("species", 
               "time_thinning", 
               "open_stand",
               "soiltyp", 
               "solDpth",
               "slFrtlC")



# Convert spatial file to NULL geometry = normal dataframe
st_geometry(stand.merged.2016) <- NULL

library(fastDummies)

df.bin <- fastDummies::dummy_cols(stand.merged.2016,
                                  select_columns = categVars, # only categorical
                                  remove_first_dummy = TRUE)  # remove reference category

# remove the original variables and observed value of damages
df.bin<-df.bin[ , !(names(df.bin) %in% c(categVars))]


# complete the dataframe by interactions 
# to have the same amount of columns as number of coefficients
# add logarithm in a formula
# add columnf for intersectp => fill with 1
df.bin$interc <- 1

df.bin$log_height <- log(df.bin$H_dom)
df.bin$log_Wspeed <- log(df.bin$windSpd)



# add interactions
df.bin$spec.spruce.X.log.height <- df.bin$species_spruce * df.bin$log_height
df.bin$spec.other.X.log.height  <- df.bin$species_other  * df.bin$log_height









# order the columns: 
# Reorder the columns:
# add bo coefficient
stand.merged.2016$b0 <- rep(1, nrow(stand.merged.2016)) 

stand.merged.2016 <-stand.merged.2016["standid",
                                      "area",
                                      "year",
                                      "b0",
                                      "species",
                                      "H_dom", 
                                      "time_thinning",
                                      "windSpd",
                                      "open_edge",
                                      "soiltyp",         
                                      "solDpth",
                                      "slFrtlC", 
                                      "avgTemp"]
  
]

my.cols.glm.ord <- c()



# ---------------------------------
#      Get coefficients:
# ---------------------------------



# Suvanto's coefficients (more accurate from Susanne code): 20 digits
intercept                    = - 14.690374506245104769
b1.spec.spruce               = - 8.494158565180855547
b2.spec.other                = - 9.314355152502169943
b3.log.height                = + 1.660897636823469137   # log
b4.last_thinning.6.10        = - 0.298186071853962231
b5.last_thinning.over.10     = - 0.844019963540904472
b6.log.wind                  = + 0.748957880201017501   # log
b7.open_stand_border         = + 0.310378186345018792
b8.soil_min.fine             = - 0.355681075669793900
b9.soil_organic              = - 0.216004202249671151
b10.soil_depth.less.30cm      = + 0.214100256449853671
b11.site_fertility            = - 0.425096042510456240
b12.temperature_sum           = + 0.095854694562656148
b13.spec.spruce.X.log.height  = + 1.634359050870280550 
b14.spec.other.X.log.height   = + 1.624775941830151726


# Put coefficients in a vector, to replace the coefficients in a formula
suvantoCoeffs<-c(intercept, 
                 b1.spec.spruce,
                 b2.spec.other,
                 b3.log.height,
                 b4.last_thinning.6.10,
                 b5.last_thinning.over.10,
                 b6.log.wind,
                 b7.open_stand_border,
                 b8.soil_min.fine,
                 b9.soil_organic,
                 b10.soil_depth.less.30cm,
                 b11.site_fertility,
                 b12.temperature_sum,
                 b13.spec.spruce.X.log.height,
                 b14.spec.other.X.log.height)





