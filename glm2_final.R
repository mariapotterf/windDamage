

# Import the calculate open_edge datasets - merge by id and landscape value
# inport raster data
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
# Read simulated optimal landscape
df.sim <- data.table::fread("C:/MyTemp/myGitLab/windDamage/output/even_flow/df_sim_opt.csv", 
                            data.table=FALSE)



# Read data with calculated open_edge
df.open <- data.table::fread("C:/MyTemp/myGitLab/windDamage/output/even_flow/df_landscape_open_edge.csv", 
                        data.table=FALSE)
df.open <- subset(df.open, select = -c(H_dom))  # remove H+_dom because have different number of decimals than other one and causes problems later

# Read raster derived input variables: average wind, temperature, ...
df.rst <- data.table::fread("C:/MyTemp/myGitLab/windDamage/output/even_flow/df_glm_raster.csv", 
                            data.table=FALSE)

#length(unique(df.rst$standid)) # 1482



# ---------------------------------
# Merge the optimal simulated dataset, open_edge and raster data
# -----------------------------

# make sure they have the same stands
my.stands<- unique(df.sim$id)
df.rst <- subset(df.rst, standid %in% my.stands)
names(df.rst) <- c("id",   
                   "area",
                   "avgTemp",
                   "windSpeed")


# Merge the datasets
df <- df.sim %>%
  left_join(df.open, by = c("id", "landscape")) %>% # , , "H_dom"
  left_join(df.rst, by = ("id")) 



# ---------------------------------------
# Test if THIN is correct for CCF and RF???
# -------------------------------------

# test if it ok well calculated??
# Subset two regimes and recalculate teh THIN values:
df %>% 
  filter(id == 6667292 &  avohaakut == "LRT30" & scenario == "ALL20" ) %>%   # (avohaakut == "CCF_3_45" )) %>%  # | avohaakut == "LRT30"
  arrange(scenario) %>% 
  dplyr::select(scenario, id, year, THIN, THIN_filled_lagged, difference, avohaakut, since_thin) #%>%

#df.s %>% print(n = 80)


# Reclassify values:
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


# For temperature sum, I have single value: not variabilit over the landscape: 1299.273
df.all$windRisk <- predict.glm(windRisk.m,
                                     subset(df.all, select = glm.colnames),
                                     type="response")

# Calculate wind risk for new models:
#df.all$windRisk.open.l <- predict.glm(windRisk.m.open.low,
 #                              subset(df.all, select = glm.colnames),
#                               type="response")


#df.all$windRisk.open.u <- predict.glm(windRisk.m.open.up,
#                                      subset(df.all, select = glm.colnames),
#                                      type="response")

range(df.all$windRisk, na.rm = T) # 
# 2.220446e-16 9.591686e-01

range(df.all$windRisk)  # na.rm = T



# Add string with indication of the optimla scenarioL: 0-20 from teh SA%
# --------------------------------------
# Split the string with numbers and characters into string and numbers:
# Change the 
# -----------------------------------------------
df.all2 <- 
  df.all %>% 
  mutate(scenario = gsub("./Bundles_2_nocow_INCOME_MANAGE_price_three_0_0_1_1", '', scenario)) %>% 
  mutate(landscape = gsub("./Bundles_2_nocow_INCOME_MANAGE_price_three_0_0_1_1", '', landscape)) %>% 
  tidyr::extract(scenario, 
                 c('scenSimpl2', 'scenNumb'), 
                 '(.*?)(\\d+)', 
                 remove = FALSE) %>% 
  mutate(scenNumb = as.numeric(scenNumb))





# Export data
#data.table::fwrite(df.all, 
 #                  "C:/MyTemp/myGitLab/windDamage/output/df_sim_windRisk.csv")




# Add data for NPI values
# ---------------------------------------

# Create two regimes: SA and non-SA"
df.all2 <- 
  df.all2 %>% 
  mutate(twoRegm = case_when(avohaakut == "SA" ~ "SA",
                             avohaakut != "SA" ~ "no_SA"))

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
df.all2 <- subset(df.all2, !id %in% stands.remove)


# stands geometry
df.geom <- st_read("C:/MyTemp/avohaakut_db/14.534/14.534/mvj_14.534.shp")
df.geom <- subset(df.geom, select = c("KUVIO_ID"))
names(df.geom) <- c("id", "geometry")
df.geom$area <- st_area(df.geom)
df.geom <- subset(df.geom, id %in% unique(df.all$id))

# Total area of watershed:
tot.area = as.numeric(sum(df.geom$area))


# Read NPI values:
# -------------------------
# get the NPVI&NPI values over scenarios 
#df.npi <- fread("C:/MyTemp/myGitLab/windDamage/params/MF_NPI.csv")
df.npi <- read.csv("C:/MyTemp/myGitLab/windDamage/params/MF_NPI.csv")

# reorganize teh data to correspond to simulated scenarios:
df.npi <- 
  df.npi %>% 
  rename(scenario = Type) %>%   # rename Type to scenario
  mutate(scenario = gsub("not_CCF", "RF", scenario)) %>% 
  tidyr::separate(scenario,   # Separate text from the number
                  into = c("scenSimpl2", "scenNumb"), 
                  sep = "(?<=[A-Za-z])(?=[0-9])") %>% 
  dplyr::select(-TypeSimple) %>% 
  mutate(scenNumb = as.numeric(scenNumb)) %>% 
  mutate(NPI = NPI/tot.area*10)   # change the values to corresponds Kyle's values



# -------------------------------------
# remove excessive columns from simulated data
# add NPI & MF values
# ------------------------------
df <-
  df.all2 %>% 
  # remove excessive columns
  dplyr::select(-c(AREA, V_total_deadwood, 
                   DEVEL_CLASS, SC,
                   SOIL_CLASS, MAIN_SP, 
                   name, THIN2,
                   scenario,
                   THIN_filled_lagged,
                   #windSpeed, 
                   PEAT.v )) %>% 
                   #tempSum)) 
   left_join(df.npi,      # join the NPI table
             by = c("scenSimpl2", "scenNumb"))


# Complete factors:
# ---------------------
# CHaracterize stands extends:
# ---------------------
#length(unique(df.geom$area))
#mean(df.geom$area)
#max(df.geom$area)
#min(df.geom$area)

#hist(df.geom$area/10000)


# Calculate the % of SA and add to table:
df.SA_prop <-
  df %>% 
  group_by(scenSimpl2, scenNumb, avohaakut) %>% 
  distinct(id) %>% 
  summarise(stands_n = n()) %>%
  filter(avohaakut == "SA") %>% 
  mutate(SA_prop = 100* (stands_n / 1470)) %>%
  dplyr::select(-c(avohaakut))


# Add SA % (frequency) to the simulated data table
df <- 
  df %>% 
  left_join(df.SA_prop, by = c("scenSimpl2", "scenNumb"))# %>% 
  #mutate(scenSimpl2 = simpleScen)

# export simplified table

fwrite(df, "output/even_flow/final_df_solution8_3.csv")

df %>% 
  group_by(landscape) %>% 
  tally() %>% 
  print(n = 1300)

# HereI end up with 1470 stands. Important to calculate open edge with the 
# original data, not with the processed one containing GLM wind risk

  







