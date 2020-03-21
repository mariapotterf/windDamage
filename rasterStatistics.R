

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



# Get zonal statistics
# https://rpubs.com/rural_gis/254726

# read the file name as raster
# r<- brick(gridNames[[1]], layer = 0)

r.grds <- lapply(gridNames, brick)

# change the projection to raster CSC
# Transform the projection of the nfi.pts (smaller size that GPKG)
poly.t <- spTransform(poly, proj4string(r.grds[[1]]))

# Calculate centroids
centroids <- gCentroid(poly.t, byid = TRUE)

# Add attributes to SpatialPoints object
# !!!
#cent.spdf <- SpatialPointsDataFrame(coords = coordinates(centroids),
#                                    data = poly.t@data)


#lapply(r.grds, nbands) # check number of bands: 365/366

# Get the mean temparature value per day per stand (stand contains multiple vectors)
ls.means<- lapply(r.grds, function(r) {
  raster::extract(r, 
                  centroids)  # fun=mean,na.rm=TRUE, df=TRUE
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
poly.t@data$avgTempSum <- rowMeans(do.call(cbind, ls.sum))




# 




# ==================================================

# working example how to calculate mean value between dataframes by rows!
df1 = data.frame(val = c(4,1,0))
df2 = data.frame(val = c(5,2,1))
df3 = data.frame(val = c(6,3,2))

myLs=list(df1, df2, df3)


# create an index or 
rowMeans(do.call(cbind, myLs))

# Calculate the mean of list by rows
lapply(myLs, function(df, i) mean(df[i,]))


# --------------------------

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
