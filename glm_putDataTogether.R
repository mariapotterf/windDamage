

# Puta data together
# ------------------------

# get glm for the sample simulated stand
# Need to all parameters: 
#  
# - from simulated data
#           tree species
#           tree height
#           time since thinning

# - from stand geometry:
#           open_edge

# - extracted rasters: 
#           wind return
#           soil type
#           mineral soil depth
#           site fertility
#           temperature sum


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
library(sf)
library(raster)
library(dplyr)
library(spData)
library(sf)
library(RColorBrewer)




# Set working directory
setwd("U:/projects/2019_windthrowModel/Janita/outSimulated")
 
# # read simulated data
df <- read.csv("rsl_without_MV_Korsnas.csv", sep = ";")  # without == climate change is not included
# 
# # Read stand geometry
df.geom = read_sf("MV_Korsnas.shp")


# 
# Get daily temperatures 
# zonal statistics
# -------------

# Get input path
# get yearly temperatures (raster stack) day by day (bands)
tempSumPath <- "U:/rawData/Finland/DailyMeanTemperat_1961-2018/daily_mean_temperature_1961_2018_geotiff"

# list file names - get last 30 rasters
gridNames <- list.files(tempSumPath, pattern = "*.tif$")

# Subset last 30 years: 1989-2018
gridNames30 <- tail(gridNames, 30)

dailyMean = calculateDailyMeans(sf  = df.geom, gridNames30)


# Get wind values
# --------------------
stand.centr <- st_centroid(df.geom)

# wind path
windpath <- "U:/rawData/Finland/windSpeedReturn10/wind_speed"

wind.r <- raster(paste(windpath, "Wind_10y_return_level.tif", sep = "/"))

# Extract wind speed
df.geom$windSpeed <- raster::extract(wind.r, stand.centr)


# Mineral soil depth
# ----------------------
# soilDepthPath = 
# wrom which LUKE tile to take the data? korsnas

                 