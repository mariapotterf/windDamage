
# Add this to your R code:
#.libPaths(c("/projappl/project_2003256/project_rpackages2", .libPaths()))
#libpath <- .libPaths()[1]

# This command can be used to check that the folder is now visible:
#.libPaths() # It should be first on the list

# Package installations should now be directed to the project
# folder by default. You can also specify the path, e.g.:
#install.packages("fastDummies", lib = libpath)


# ----------------------------------
# Recontruct optimal landscape
# ----------------------------------

# filter simulated data (with correct names from optmal scenarios)
# to create optimal lanscape: 58 regimes, 63 landscapes x 20 time steps

# Read .csv files of optimal solutions
# if there is not 1.0 solution: 
# keep only stand that has higher proportion of the regime
# Based on the scenarios, subset the individual simulated stands
# ----------------------------------
rm(list = ls())


library(sf)
library(dplyr)
require(data.table)
library(tidyr)
library(stringr)

stands.remove <- c(13243875,
                   13243879,
                   13243881)

# Read corrected simulated names:
df.sim<- data.table::fread("/projappl/project_2003256/windDamage/output/simulated_AVK_regimes.csv", 
                           data.table=FALSE, 
                           stringsAsFactors = FALSE)

df.sim <- subset(df.sim, !id %in% stands.remove)



# -----------------------------
# Read optimal solution:
# -----------------------------

source("/projappl/project_2003256/windDamage/myFunctions.R")


# Read all optimal solutions to 
# see applied regimes?? 
# -------------------------------
setwd("/projappl/project_2003256/Solutions_6")
df.optim = list.files(pattern=".csv$",
                      full.names = TRUE) # ends with .csv$, \\. matches .


# Replace names indications by simpler way 

df.names <- gsub("./Bundles_2_nocow_NPV_MANAGE_price_three_0_0_1_1_", "", df.optim)
df.names <- gsub(".csv", "", df.names)
df.names <- gsub("not_CCF", "RF", df.names)

(df.names)

# Read all dataframes in a loop
df.opt.ls = lapply(df.optim, readOptimal)

# Filter the stands from optimal scenario to exlude corrupted dstands
df.opt.ls <- lapply(df.opt.ls, function(df) subset(df, !id %in% stands.remove ))

# Merge optimal data in one files, filter for incorrect stands
opt.df.all <- do.call(rbind, df.opt.ls)


# Filter the simulated data by optimal scenario:
# Subset individual landscapes  = optimal from simulated df (all in one table)

df.sim.opt <- lapply(df.opt.ls, 
                     # Semi join subset by the stand id and by specific regime 
                     # simulated in single optimal scenario 
                     function(df.optim)  {
                       df.sim %>%
                         semi_join(df.optim,
                                   by = c("id" = "id",
                                          "avohaakut" = "regime")) })



# Add indication of the scenarios as new column
df.sim.opt <- Map(cbind, 
                  df.sim.opt, 
                  scenario = df.names)

# Convert to single dataframe, 
# will be further split in multiple lists
df.sim.all <- do.call(rbind, df.sim.opt)

# Create new category to group teh data into landscapes:
df.sim.all$landscape <- paste(df.sim.all$year, df.sim.all$scenario, sep = "_")



# ----------------------------------
# Interpret of THIN years:
# ------------------------------
# convert 0 to NA
# IN CCF: years area stored as "2016-04-16": 
# keep only first 4 characters to convert this to numeric values
# i.e. "2016-04-16" -> to "2016""
# to calculate yearly differences

df.sim.all2 <- 
  df.sim.all %>%
  mutate(THIN = na_if(THIN, 0))  %>% 
  mutate(THIN2 = substring(THIN,0,4)) %>%  # keep the first 4 characters from CCF regimes, datum in format "2016-04-16" -> to "2016"
 
   group_by(id, avohaakut, scenario) %>% 
  mutate(THIN_filled_lagged = lag(THIN2)) %>%
  mutate(THIN_filled_lagged = as.numeric(THIN_filled_lagged)) %>%
  tidyr::fill(THIN_filled_lagged) %>% 
  mutate(difference = year - THIN_filled_lagged) %>% 
  mutate(since_thin = case_when(is.na(difference) | difference < 0 ~ ">10",
                                difference %in% c(0:5) ~ "0-5",
                                difference %in% c(6:10) ~ "6-10",
                                difference > 10 ~ ">10")) %>% 
  # Remove unnecessary columns
  dplyr::select(-branching_group, 
                -regime.x,
                -regime.y)


# Check:

unique(df.sim.all2$difference)
unique(df.sim.all2$since_thin)


# Create new factors: existence of thinning, simpleScen
# --------------------------------
df.sim.all2 <-
  df.sim.all2 %>% 
  # Differentiate between SA, CCF and RF with and without thinning
  mutate(avoh_Simpl = case_when(   
    str_detect(avohaakut, "SA")   ~ "SA",
    str_detect(avohaakut, "CCF_") ~ "CCF",
    str_detect(avohaakut, "LRH")  ~ "RF_noT",
    str_detect(avohaakut, "LRT")  ~ "RF_T",
    str_detect(avohaakut, "SR5")  ~ "RF_noT",
    str_detect(avohaakut, "SRT5") ~ "RF_T",
    str_detect(avohaakut, "TH")   ~ "RF_noT",
    str_detect(avohaakut, "TT")   ~ "RF_T")) %>% 
  # Simple 3 scenarios: RF, CCF and ALL
  mutate(simpleScen = case_when(
    stringr::str_detect(scenario, "RF") ~ "RF",
    stringr::str_detect(scenario, "ALL") ~ "ALL",
    stringr::str_detect(scenario, "CCF") ~ "CCF"))


# write the table
fwrite(df.sim.all2, "/projappl/project_2003256/windDamage/output/df_sim_opt.csv")










# ------------------------
# Calculate open edge
# ------------------------

# input:
#    - stand geometry
#    - simulated optimal data including H_dom
# output: 
#    - df wit indication of standid, scenario, H_dom...
# to be able to merge it back to simulated optimal data (if those woould change)

rm(list = ls())


library(sf)
library(dplyr)
require(data.table)
library(tidyr)


stands.remove <- c(13243875,
                   13243879,
                   13243881)



# read optimal simulated data
df<- data.table::fread("/projappl/project_2003256/windDamage/output/df_sim_opt.csv",
                       data.table=FALSE, 
                       stringsAsFactors = FALSE)

df <- subset(df, !id %in% stands.remove)

keep_id <- unique(df$id)

# Read stand geometry - subset twice
# -----------------------------
df.geom <- st_read("/projappl/project_2003256/windDamage/14.534/14.534/mvj_14.534.shp")
df.geom <- subset(df.geom, select = c("KUVIO_ID"))
names(df.geom) <- c("id", "geometry")
# df.geom$area <- st_area(df.geom)
df.geom <- subset(df.geom, !id %in% stands.remove)
df.geom <- subset(df.geom, id %in% keep_id)



# Calculate open_edges
# ---------------------------------
source("/projappl/project_2003256/windDamage/myFunctions.R")

# Split dataframe into dataframe list
#land.ls.all <- df %>% 
 # dplyr::select(id, year, scenario, avohaakut, landscape, H_dom) %>% 
 # group_by(landscape) %>% 
 # group_split()

#object.size(land.ls.all)

land.ls <- df %>% 
  dplyr::select(id, landscape, H_dom) %>% 
  group_by(landscape) %>% 
  group_split()

# object.size(land.ls.sel) # object with subesetted columns is 12 times smalled than with all columns!!!
# ---------------------------------------------------
# Calculate the pairs of neighbors:
# calculate open_edge
# ---------------------------------------------------
nbrs <- find_nbrs_geom(df.geom)


# calculate on one landscape
open_edge.ls <- lapply(land.ls, function(df) open_edge_by_nbrs(nbrs, df))

# merge open edge data with the landscapes and ids 
merged.ls <- Map(merge, land.ls, open_edge.ls, by="id")

# Merge partial dfs into one
merged.df <- do.call(rbind, merged.ls)

# write the table
fwrite(merged.df, "/projappl/project_2003256/windDamage/output/df_landscape_open_edge.csv")



# Import the calculate open_edge datasets - merge by id and landscape value
# inport raster data
# Calculate wind risk for management regimes
# create ggplots & main conclusions

# ===============================


# 
rm(list = ls())


setwd("/projappl/project_2003256/windDamage")

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
df.sim <- data.table::fread("/projappl/project_2003256/windDamage/output/df_sim_opt.csv", 
                            data.table=FALSE)

# Read data with calculated open_edge
df.open <- data.table::fread("/projappl/project_2003256/windDamage/output/df_landscape_open_edge.csv", 
                        data.table=FALSE)
df.open <- subset(df.open, select = -c(H_dom))  # remove H+_dom because have different number of decimals than other one and causes problems later

# Read raster derived input variables: average wind, temperature, ...
df.rst <- data.table::fread("/projappl/project_2003256/windDamage/output/df_glm_raster.csv", 
                            data.table=FALSE)


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
df1 <- df.sim %>%
  left_join(df.open, by = c("id", "landscape")) %>% # , , "H_dom"
  left_join(df.rst, by = ("id")) 




# ---------------------------------------
# Test if THIN is correct for CCF and RF???
# -------------------------------------

# test if it ok well calculated??
# Subset two regimes and recalculate teh THIN values:
df1 %>% 
  filter(id == 6667292 &  avohaakut == "LRT30" & scenario == "ALL20" ) %>%   # (avohaakut == "CCF_3_45" )) %>%  # | avohaakut == "LRT30"
  arrange(scenario) %>% 
  dplyr::select(scenario, id, year, THIN, THIN_filled_lagged, difference, avohaakut, since_thin) #%>%

#df.s %>% print(n = 80)


# Reclassify values:
df.all<-
  df1 %>% 
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
source("/projappl/project_2003256/windDamage/myFunctions.R")


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
# -----------------------------------------------
df.all <- 
  df.all %>% 
  mutate(scenario = gsub('./Bundles_2_nocow_INCOME_MANAGE_price_three_0_0_1_1', '', scenario) ) %>%
  tidyr::extract(scenario, 
                 c('scenSimpl2', 'scenNumb'), 
                 '(.*?)(\\d+)', 
                 remove = FALSE) %>% 
  mutate(scenNumb = as.numeric(scenNumb))




# Export data
data.table::fwrite(df.all, 
                   "/projappl/project_2003256/windDamage/output/df_sim_windRisk.csv")

rm(list = ls())

# , eval = FALSE
library(data.table)
library(dplyr)
library(raster)
library(ggplot2)
library(sf)
library(stringr)
library(gridExtra)
library(tidyr)
library(ggpubr)


theme_set(theme_classic())
theme_update(panel.grid.major = element_line(colour = "grey95",
                                             size = 0.1,
                                             linetype = 2))

# Read datasets:
# ----------------------------------------
df.all <- fread("/projappl/project_2003256/windDamage/output/df_sim_windRisk.csv")

# Create two regimes: SA and non-SA"
df.all <- 
  df.all %>% 
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
df.all <- subset(df.all, !id %in% stands.remove)


# stands geometry
df.geom <- st_read("/projappl/project_2003256/windDamage/14.534/14.534/mvj_14.534.shp")
df.geom <- subset(df.geom, select = c("KUVIO_ID"))
names(df.geom) <- c("id", "geometry")
df.geom$area <- st_area(df.geom)
df.geom <- subset(df.geom, id %in% unique(df.all$id))

# Total area of watershed:
tot.area = as.numeric(sum(df.geom$area))


# Read NPI values:
# -------------------------
# get the NPVI&NPI values over scenarios 
df.npi <- fread("/projappl/project_2003256/windDamage/params/MF_NPI.csv")

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
  df.all %>% 
  # remove excessive columns
  dplyr::select(-c(V_total_deadwood, 
                DEVEL_CLASS, SC,
                SOIL_CLASS, MAIN_SP, 
                name, THIN2,
                scenario,
                THIN_filled_lagged,
                windSpeed, PEAT.v, tempSum)) %>% 
  left_join(df.npi, by = c("scenSimpl2", "scenNumb"))


# remove original table to save the memory
rm(df.all)



# Complete factors:
# ---------------------
# CHaracterize stands extends:
# ---------------------
length(unique(df.geom$area))
mean(df.geom$area)
max(df.geom$area)
min(df.geom$area)

hist(df.geom$area/10000)


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
  left_join(df.SA_prop, by = c("scenSimpl2", "scenNumb"))

# export simplified table

fwrite(df, "output/final_df.csv")

