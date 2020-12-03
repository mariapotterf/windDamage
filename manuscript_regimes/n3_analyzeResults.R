
# ----------------
# Create some pre-plots
# ----------------

# selet the some plots
# check if they differ


# 
rm(list = ls())



# ----------------------------------
# start the script: using rgdal library
# ----------------------------------

#library(ggplot2)
library(dplyr)
library(tidyr)
library(rgdal)
library(sf)
library(rgdal)
library(ggspatial)
library(rgeos)
library(sf)
library(raster)
library(dplyr)
library(spData)
library(sf)
#library(RColorBrewer)


# Read corrected simulated names:
df<- data.table::fread("C:/MyTemp/myGitLab/windDamage/manuscript_regimes/output/df_sim_raster.csv", 
                       data.table=FALSE, 
                       stringsAsFactors = FALSE)

