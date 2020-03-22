

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

# Output: export geometry with new attributes



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
inDataPath = "U:/projects/2019_windthrowModel/Janita/outSimulated"
setwd(inDataPath)
 
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
setwd(tempSumPath)

# list file names - get last 30 rasters
gridNames <- list.files(tempSumPath, pattern = "*.tif$")

# Subset last 30 years: 1989-2018
gridNames30 <- tail(gridNames, 30)

# Read data as raster bricks
r.grds <- lapply(gridNames30, brick)

# Calculate daily means
dailyMean = calculateDailyMeans(sf  = df.geom, gridNames30)

df.geom$avgTemp <- rep(dailyMean, nrow(df.geom))

rm(r.grds)

# Get wind values
# --------------------
stand.centr <- st_centroid(df.geom)

# wind path
windpath <- "U:/rawData/Finland/windSpeedReturn10/wind_speed"

wind.r <- raster(paste(windpath, "Wind_10y_return_level.tif", sep = "/"))

rm(wind.r)

# Extract wind speed
df.geom$windSpeed <- raster::extract(wind.r, stand.centr)


# Mineral soil depth
# ----------------------
# soild depth data are binary:
# mineral soil alle30cm
# 0 = FALSE
# 1 = TRUE

soilDepthPath = "U:/rawData/Finland/mineralSoilAlle30/Research Data"
soil.depth.r = raster(paste(soilDepthPath, "mineral_alle30cm_P3.tif", sep = "/"))
# wrom which LUKE tile to take the data? korsnas is in P3
stand.centr <- st_centroid(df.geom)
rm(soil.depth.r)   


df.geom$soilDepth <- raster::extract(soil.depth.r, stand.centr)


# Site fertility
# ---------------------
# values: poor/fertile
# fertile = from herb-rich to mesic on mineral soils & eutrophic to meso-ologitrophic peatlands
# 
# 
soilFertilityPath = "U:/rawData/Finland/siteFertility/siteFertilitySouth/2017/kasvupaikka_vmi1x_1317_P3.tif"
fertility.r = raster(paste(soilFertilityPath, "kasvupaikka_vmi1x_1317_P3.tif", sep = "/"))

df.geom$soilFertility <- raster::extract(fertility.r, stand.centr)

# Reclassify: 1-3 =  fertile, 4-8 = poor
# POZOR !!! centroids generated NA values!! maybe differenc approach? zonal stats?
df.geom$soilFertilityClass <- ifelse(df.geom$soilFertility <= 3, "fertile", "poor")



# Soil type - vector data
# --------------------
# values: mineral-coarse, mineral-fine,organic 
# organic = peatlands polygons
# mineral-fine = clay and fine sands
# mineral-coarse = sands and coarser soils



# ---------------------
# Save sf object
# ----------------------
st_write(df.geom, paste0(inDataPath, "outKorsnas_att.shp"))




