

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
outPath = paste(pathData, "NFI_data", sep = "/")


# Read XY data from NFI: 

# The input file geodatabase
# the CRS was changes in ArcGIS to fit clim data coordinate system in UTM 35
require(sf)
df.geom <- sf::st_read("C:/MyTemp/2021_WindRisk_biodiversity/output", 
                  layer = "XY_UTM_35")

# Read raster data: 

# daily temprituress, as stock of values;
# this needs to be as working directory, to read all rasters
tempSumPath <- "C:/MyTemp/Ellinoora_CC/input_rasters/dailyTemperatures/ilmatiede/10km_daily_mean_temperature/geotiff/daily_mean_temperature_1961_2020_geotiff"
setwd(tempSumPath)

# list file names - get last 30 rasters
gridNames <- list.files(tempSumPath, pattern = "*.tif$")

# Subset last 30 years: 1989-2018
gridNames30 <- tail(gridNames, 30)

# process rasters to extract the rastervalues
r.grds <- lapply(gridNames30, brick)


# Get the mean temparature value per day per stand (stand contains multiple vectors)
ls.means<- lapply(r.grds, function(r) {
  raster::extract(r, df.geom)
})

# Remove the base temperatture = 5 c from each daily mean
ls.diff <- lapply(ls.means, function(df) df - 5)

# calculate the difference with base value
ls.posit <- lapply(ls.diff, function(df) {
  df[df<0] <- 0
  return(df)
})

# Sum up the positive difference value by year
ls.sum <- lapply(ls.posit, rowSums)

# Calculate the means for each row in a DF list
# add it as a new attribute to stands
df.geom$avgTemp <- rowMeans(do.call(cbind, ls.sum))

# plot if correct??
windows()
plot(df.geom["avgTemp"])

rm(r.grds)




# Get wind values  ----------------------------------------------------------------------------------
wind.r <- raster("C:/MyTemp/Ellinoora_CC/input_rasters/windSpeed_10yrs/ilmatiede/wind_speed/Wind_10y_return_level.tif")

# Extract wind speed
df.geom$windSpeed <- raster::extract(wind.r, df.geom)

windows()
plot(df.geom[c('AlajakoNim', "avgTemp", "windSpeed")])

rm(wind.r)


# Convert df.geom to normal dataframe ----------------------------------------------------------------
df.raster <- df.geom
# get rid of geometry, keep only the data
st_geometry(df.raster) <-NULL

# export csv
fwrite(df.raster, paste(outPath, "df_raster_XY.csv", sep = "/"))


