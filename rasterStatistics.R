

# Get raster statistics by the stand
# ==========================

# calculate the raster zonal statistic by the stand value
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
gridNames <- list.files(tempSumPath, pattern = "*.tif$")

# get zonal statistics by bands
# Read input forest stand data
poly = readOGR("//fileservices.ad.jyu.fi/homes/mpotterf/Desktop/dataStackOver", 
               layer = "forest_fc")

# change the projection to raster CSC
# Transform the projection of the nfi.pts (smaller size that GPKG)
poly.t <- spTransform(poly, proj4string(r))


# Get zonal statistics
# https://rpubs.com/rural_gis/254726

# read the file name as raster
r<- brick(gridNames[[1]], layer = 0)

r.grds <- lapply(gridNames, brick)

lapply(r.grds, nbands) # check number of bands: 365/366

# Get the mean temparature value per day per stand (stand contains multiple vectors)
ls.means<- lapply(r.grds, function(r) {
  extract(r, 
          poly, 
          fun=mean, na.rm=TRUE, df=TRUE)
  })

# Check if dataset contains any NA values?
# remember that standID  = ID are included as first column!
tempSumYear <- lapply(ls.means, function(df) {
  df<-df[,!(names(df) %in% c("ID"))]   
})


# Make a fake example: 
# calcutethe temperature sum rowwise

df <- data.frame(d1 = c(5,1,2,1,5),
                 d2 = c(6,1,8,1,8),
                 d3 = c(8,1,9,1,6),
                 d4 = c(1,2,1,3,0),
                 d5 = c(1,3,1,2,1),
                 d6 = c(1,5,2,3,3))

df<-ls.means[[1]]

df[1,]  # get the position by row

tempLim = 2
n_dd = 2

sum(df[1,][df[1,]>tempLim][1:n_dd])


# Make  a function to 
tempSum <- function(df) {
  
  tempLim = 5
  n_dd = 100
  
  # Calculate temperature sum
  sum(a[a>tempLim][1:n_dd])
  
  df<-df[,!(names(df) %in% c("ID"))]
}


tempSum(ls.means[[1]])


windows()
par(mar=c(1,1,1,1))
plot(ls.means[[1]])






a = c(1,1,1,1,5,5,5,7,2,1,1,3,5,4,8)

# subset the first 100 elements that are > 5
tempLim = 5
n_dd = 100

# Calculate temperature sum
sum(a[a>tempLim][1:n_dd])




#ex <- extract(r, poly, fun=mean, na.rm=TRUE, df=TRUE)

## !!! ex works geat!! 
# check if I need to loop over the individual meteo raster, 
# or I can stack all of them into single stack file


# Create a raster stack
#create a raster stack
s <- stack(paste0(tempSumPath, "/", gridNames))




# ------------------------
#     WIND speed
# ------------------------
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
