

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
library()
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


# read data with calculated open_edge
df <- data.table::fread("open_edge_calc_fast.csv") 

# st_read do not return tibble just spatial dataframe
df.geom = st_read("outKorsnas_att.shp")


# Find same stands id between geometry and simulated data 
# and keep those
stands.complete = Reduce(intersect, list(df$standid, df.geom$standid))

# Reduce df and df.geom to the same stands
# df.geom = just in case for visualisation, no need 
# for wind disk calculation!!!!
df      <- subset(df,standid %in% stands.complete)
df.geom <- subset(df.geom, standid %in% stands.complete)


# -------------------------------
# !!! Add missing columns: - create fake data!!!!
# variables used fromSIMO instead from rasters:
# will be later replaced directly from SIMO
#
# SC - identify fetility class
# SOIL_CLASS - identify coarsiness of teh soil ^ soil_depth <30 TRUE/FALSE
# PEAT - identify where is peat
# 
# -----------------------------------
df$time_thinning <- factor(sample(c("0-5", "6-10", ">10"),
                                  nrow(df), replace = TRUE), 
                                  levels = c("0-5", "6-10", ">10"))

df$soiltyp <- factor(sample(c("mineral coarse", 
                              "mineral fine",
                              "organic"), nrow(df), replace = TRUE),
                               levels = c("mineral coarse", 
                                          "mineral fine",
                                          "organic")) 

# replace NA values - due to centroids not overlapping with raster
df$slFrtlC <- replace_na(df$slFrtlC, "poor")

# Soil depth: TRUE & FALSE
# soil depth < 30: 1 = TRUE, 0 = FALSE
df$solDpth <- factor(ifelse(rbinom(nrow(df), 1, 0.5),
                                      "FALSE","TRUE"))


# Subset columns crucial for glm()
#my.cols.glm <- c("standid",
 #                "area",
 #                "year",
 #                "species",
 #                "H_dom", 
 #                "time_thinning",
 #                "windSpd",
 #                "soiltyp",         
 #                "solDpth",
 #                "slFrtlC", 
 #                "avgTemp")

# Replace the species values
# seems that these data have only pine???
# mainGRp is land type == forest = 1
# need to use MAIN_SP from simulated data !! 
# or mntrspc

df<-
  df %>% 
  mutate(species = case_when(mntrspc == 1 ~ "pine",
                             mntrspc == 2 ~ "spruce",
                             TRUE ~ "other")) %>% 
  mutate(H_dom = replace_na(H_dom, 0.01)) %>%  # no possible to get log(0)  
  mutate(H_dom = H_dom * 10) %>%        # Susanne values are in dm instead of meters
 # dplyr::select(my.cols.glm)  %>%      # select columns 
  mutate_if(is.character, as.factor)   # convert all characters to factor




# Change column names to correspond glm() 
colnames(df)[colnames(df) == 'windSpd'] <- 'windSpeed'
colnames(df)[colnames(df) == 'soiltyp'] <- 'soilType'
colnames(df)[colnames(df) == 'solDpth'] <- 'soilDepthLess30'
colnames(df)[colnames(df) == 'slFrtlC'] <- 'siteFertility'
colnames(df)[colnames(df) == 'avgTemp'] <- 'tempSum'



# subset just one year
df.2016 <- 
  df %>% 
  filter(year == 2016) 

df1 <- df

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
          "time_thinning", 
          "windSpeed", 
          "open_edge",
          "soilType",
          "soilDepthLess30", 
          "siteFertility",
          "tempSum")

str(df)


# subset only needed columns:
df.sub<-df[, keep]


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
df.sub$tempSum    <- df.sub$tempSum/100   # according to Susane 


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


# add wind risk values to original data to obtain year!!!
df$windRisk <- df.sub$windDamagePred 



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

# Temporal dynamics of wind risk oof management
ggplot(df.sim, 
       aes(x = as.factor(year),
               y = windRisk)) +
  geom_boxplot() + 
  facet_grid(. ~ regime)


# Is the wind risk the same for all regimes in the first simulation year??
p.windRisk <- ggplot(subset(df.sim, year == 2016), 
       aes(x = regime,
           y = windRisk)) +
  geom_boxplot() +
  ggtitle("Wind risk 2016")

p.H_dom <- ggplot(subset(df.sim, year == 2016), 
       aes(x = regime,
           y = H_dom)) +
  geom_boxplot() +
  ggtitle("H_dom 2016")


p.BA <- ggplot(subset(df.sim, year == 2016), 
       aes(x = regime,
           y = BA)) +
  geom_boxplot()  +
  ggtitle("BA 2016")


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
  ggtitle("Count of stands with open edge")


  
  
  
  







