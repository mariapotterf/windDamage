## 
# Read simulated data and their raster charactersitiscs
# read suvanto;s models
# modify data to count wind risk for each
# open edge: as I have fragmeneted landscape, they alre all 'open'
# -------------------------------------

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



# ---------------------------
# Read simulated optimal landscape
df <- data.table::fread("C:/MyTemp/myGitLab/windDamage/manuscript_regimes/output/df_sim_raster.csv", 
                            data.table=FALSE)

# unique(df$id) # 640 stands


# add open-edge values
df$open_edge = "TRUE"


# Or, generate random values
#df %>% 
#  mutate(pop = sample(c("T", "F"), n(), replace = TRUE))



# # Reclassify values:
df.all<-
  df %>% 
  mutate(PEAT.v = case_when(PEAT == 0 ~ "mineral soil",
                            PEAT == 1 ~ "peat"))  %>%
  mutate(SC.v = case_when(SC %in% 1:3 ~ "fertile",
                          SC %in% 4:6 ~ "poor")) %>%                 # COMPLETE SOIL CALSS to get mineral coarse/fine??
  mutate(soil_depth_less30 = ifelse(SOIL_CLASS == 1, TRUE,FALSE)) %>%
  mutate(soilType = case_when(SOIL_CLASS == 0 ~ "organic",
                              SOIL_CLASS %in% 1:4 ~ "mineral coarse",
                              SOIL_CLASS %in% 5:7 ~ "mineral fine")) %>% 
  mutate(species = case_when(MAIN_SP == 1 ~ "pine",
                             MAIN_SP == 2 ~ "spruce",
                             TRUE ~ "other")) %>% 
  mutate(H_dom = replace_na(H_dom, 0.0001)) %>%  # no possible to get log(0) or log(NA)  
  mutate(H_dom = H_dom * 10) %>%        # Susanne values are in dm instead of meters
  mutate_if(is.character, as.factor)    # convert all characters to factor



# -----------------------------------
#
# Reorganize the input data to fit Suvanto model's requirement
# 
# -----------------------------------

# Correct order of variables, factors & levels
# need to have same names of columns?? YES
# when data are sourced (source()), they are all available in my script
source("C:/MyTemp/myGitLab/windDamage/myFunctions.R")


# try if I can calculate the preidtcion based on subset data
# to make sure that data did not get randomly reorganized
df.all <-
  df.all %>% 
  dplyr::rename(time_thinning   = since_thin,             # new.name = old.name
                soilDepthLess30 = soil_depth_less30,
                siteFertility   = SC.v,
                tempSum         = avgTemp)



# Get vector of columns names to keep for statistics
glm.colnames <- c("species", 
                  "H_dom", 
                  "time_thinning", 
                  "windSpeed", 
                  "open_edge",
                  "soilType",
                  "soilDepthLess30", 
                  "siteFertility",   # siteFertility
                  "tempSum")




# -----------------------------------------
#          Correct the factor levels 
#          for categoric variables
# ------------------------------------
df.all$species          <- factor(df.all$species, 
                                  levels = c("pine", 
                                             "spruce", 
                                             "other"))

df.all$time_thinning    <- factor(df.all$time_thinning, 
                                  levels = c("0-5", 
                                             "6-10", 
                                             ">10"))
df.all$open_edge        <- factor(df.all$open_edge,
                                  levels = c("FALSE", 
                                             "TRUE"))
df.all$soilType         <- factor(df.all$soilType,
                                  levels = c("mineral coarse", 
                                             "mineral fine",
                                             "organic"))
df.all$soilDepthLess30  <- factor(df.all$soilDepthLess30, 
                                  levels = c("FALSE", 
                                             "TRUE"))
df.all$siteFertility    <- factor(df.all$siteFertility,
                                  levels = c("poor", 
                                             "fertile"))
df.all$tempSum          <- df.all$tempSum/100   # according to Susane 


# ------------------------------------------
# Calculate predicted values for wind risk 
# ------------------------------------------
# Susane model: windRisk.m

# Create new models to modify the open_edge by +- SE:
coefficients(windRisk.m)  # the open_edge is coefficient #8

# Create new models for open_edge lower and upper interval:
# modify the coefficient: add/substract the SE from coefficnet #8 
#windRisk.m.open.low <- windRisk.m
#windRisk.m.open.up  <- windRisk.m


# Replace the coefficients by +- SE: 0.095
#windRisk.m.open.low$coefficients[8] <- windRisk.m$coefficients[8] - 0.095  
#windRisk.m.open.up$coefficients[8]  <- windRisk.m$coefficients[8] + 0.095  


# Create random sample of open TRUE or FALSE to see if tehre is effect?
# https://stackoverflow.com/questions/29884432/dplyr-integer-sampling-within-mutate
# --------------------------
#library(dplyr)
#set.seed(0)

#Dummy data.frame to test
# ---------

#df <- tbl_df(data.frame(x = rep(1:3, each = 4)))


# ----------------------


# For temperature sum, I have single value: not variabilit over the landscape: 1299.273
df.all$windRisk <- predict.glm(windRisk.m,
                               subset(df.all, select = glm.colnames),
                               type="response")

# Check how values looks like: 
range(df.all$windRisk, na.rm = T) # 



# Classify data
# replace all NA in volume by 0 - because not volume is available there
df<- 
  df.all %>% 
  dplyr::mutate(V_stand_log = replace_na(V_stand_log, 0)) %>% 
  dplyr::mutate(V_stand_pulp = replace_na(V_stand_pulp, 0)) %>% 
  dplyr::mutate(V_strat_max = replace_na(V_strat_max, 0)) %>% 
  dplyr::mutate(V_strat_max_log = replace_na(V_strat_max_log, 0)) %>% 
  dplyr::mutate(V_strat_max_pulp = replace_na(V_strat_max_pulp, 0)) %>% 
  dplyr::mutate(Harvested_V_log_under_bark = replace_na(Harvested_V_log_under_bark, 0)) %>% 
  dplyr::mutate(Harvested_V_pulp_under_bark = replace_na(Harvested_V_pulp_under_bark, 0)) 


# Replace the no_SA values in TwoRegms: change to Management
df <- 
  df %>% 
  dplyr::mutate(Management = as.character(twoRegm))%>% # copy the columns
    #distinct(Management)
  dplyr::mutate(Management = replace(Management, # replace the values
                                     Management == "no_SA", "Active")) %>%
  dplyr::mutate(Management = replace(Management, # replace the values
                                     Management == "SA", "Set Aside"))



# Reclassify teh regimes
# Reclassify names of tables:
# continuous, shortening, extecsion, green tree ret
# I already have define if thining occurs or not, so maybe keep this simple
df<- 
  df %>% 
  mutate(modif = case_when(grepl("CCF_", avohaakut) ~ "CCF",
                           grepl("SR", avohaakut)   ~ "shortening",
                           grepl("TTN", avohaakut)  ~ "GTR",
                           grepl("LRT", avohaakut)  ~ "extension",
                           grepl("THwoTM20", avohaakut) ~ "shortening",
                           grepl("THNS", avohaakut) ~ "GTR",
                           grepl("THwoT10", avohaakut) ~ "extension",
                           grepl("LRH", avohaakut)  ~ "extension",
                           grepl("SA", avohaakut)   ~ "SA",
                           TRUE ~ "no"))


# Export data
data.table::fwrite(df, 
                   "C:/MyTemp/myGitLab/windDamage/manuscript_regimes/output/df_sim_windRisk.csv")

