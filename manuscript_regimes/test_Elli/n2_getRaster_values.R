

# Get raster attributes

# Input: 
#    - read shps of stands
#    - raster daily temperatures
#    - raster wind speed

# Process:
# read input data
# daily temp: 10km raster, get one value per watershed
# get means per stand from windSpeed values
# both works by creating a centroid of the shp and then get values


# output:
#    - df with id and mean  


# Libs  -------------------------------------------------------------------

# 
rm(list = ls())

# ----------------------------------
# start the script: using rgdal library
# ----------------------------------

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
library(data.table)


# Read data ---------------------------------------------------------------
source("C:/MyTemp/myGitLab/windDamage/myFunctions.R")

# Get path to shp data
pathData = "C:/MyTemp/myGitLab/windDamage/manuscript_regimes"
outPath = paste(pathData, "input_CC", sep = "/")

# Get and simplify stands as sf objets
df.geom <- st_read(paste(pathData, "input_shp/stands/MV_Raasepori.shp", sep = "/"))
df.geom <- subset(df.geom, select = c("standid"))

# Read raster data: daily temprituress, as stock of values;
# this needs to be as working directory, to rea all rasters
tempSumPath <- "C:/MyTemp/Ellinoora_CC/input_rasters/dailyTemperatures/ilmatiede/10km_daily_mean_temperature/geotiff/daily_mean_temperature_1961_2020_geotiff"
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




# Get wind values  ----------------------------------------------------------------------------------
stand.centr <- st_centroid(df.geom)

wind.r <- raster("C:/MyTemp/Ellinoora_CC/input_rasters/windSpeed_10yrs/ilmatiede/wind_speed/Wind_10y_return_level.tif")

# Extract wind speed
df.geom$windSpeed <- raster::extract(wind.r, stand.centr)

rm(wind.r)


# Convert df.geom to normal dataframe ----------------------------------------------------------------
df.raster <- df.geom
# get rid of geometry, keep only the data
st_geometry(df.raster) <-NULL

# export csv
fwrite(df.raster, paste(outPath, "df_raster_Raasepori.csv", sep = "/"))


