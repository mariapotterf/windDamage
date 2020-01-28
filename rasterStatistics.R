

# Get raster statistics by the stand
# ==========================


# calculate the zonal statsitic by the stand value
rm(list = ls())

library(ggplot2)  # for choropleth map plot
library(broom) # to convert spatial data to dataframe
library(mapproj)
library(maptools)
library(spdep)    # neighbours
library(rgdal)
library(rgeos)
library(sf)
library(raster)
library(dplyr)
library(spData)
library(sf)
library(RColorBrewer)


# set working directory
setwd("//fileservices.ad.jyu.fi/homes/mpotterf/Desktop/dataStackOver")


# get yearly temperatures (raster stack) day by day (bands)
tempSumPath <- "U:/rawData/Finland/DailyMeanTemperat_1961-2018/daily_mean_temperature_1961_2018_geotiff"

setwd(tempSumPath)

# list file names:
grids <- list.files(tempSumPath, pattern = "*.tif$")

# read the file name as raster
r<- brick(grids[[1]], layer = 0)

# check number of bands: 365
nbands(r)

# get zonal statistics by bands
# Read input forest stand data
poly = readOGR("//fileservices.ad.jyu.fi/homes/mpotterf/Desktop/dataStackOver", 
               layer = "forest_fc")

# change the projection to raster CSC
# Transform the projection of the nfi.pts (smaller size that GPKG)
poly.t <- spTransform(poly, proj4string(r))


# Get zonal statistics
# https://rpubs.com/rural_gis/254726

ex <- extract(r, poly, fun=mean, na.rm=TRUE, df=TRUE)

## !!! ex works geat!! 
# check if I need to loop over the individual meteo raster, 
# or I can stack all of them into single stack file


# Create a raster stack
#create a raster stack
s <- stack(paste0(tempSumPath, "/", grids))


# read wind speed raster
# Here I can create a stack of rasters: 
# - wind speed
# -  soil type
# - mineral soil depth
# - site fertility
# - temperature sum

r <- raster("Wind_10y_return_level.tif")

# try raster statistic from the raster stack

plot(r)

# Read input forest stand data
poly = readOGR(getwd(), 
                    layer = "forest_fc")

# change the projection to raster CSC
# Transform the projection of the nfi.pts (smaller size that GPKG)
poly.t <- spTransform(poly, proj4string(r))


# Get zonal statistics
# https://rpubs.com/rural_gis/254726

ex <- extract(r, poly, fun=mean, na.rm=TRUE, df=TRUE)

#for (i in 1:length(grids)){
 # ex <- extract(s, poly, fun=sum, na.rm=TRUE, df=TRUE)
#}
