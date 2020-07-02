

# Collect raster attributes for Suvanto's model
# ------------------------

#           wind return
#           temperature sum

# Output: export df with new attributes



# 
rm(list = ls())


#setwd("C:/MyTemp/myGitLab/windDamage")

#source("myFunctions.R")


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


stands.remove <- c(13243875,
                   13243879,
                   13243881)


# Read stand geometry - subset twice
# -----------------------------
df.geom <- st_read("C:/MyTemp/avohaakut_db/14.534/14.534/mvj_14.534.shp")
df.geom <- subset(df.geom, select = c("KUVIO_ID"))
names(df.geom) <- c("standid", "geometry")
df.geom$area <- st_area(df.geom)

df.geom <- subset(df.geom, !standid %in% stands.remove)

source("C:/MyTemp/myGitLab/windDamage/myFunctions.R")

# 
# Get daily temperatures 
# zonal statistics
# -------------

# Get input path
# get yearly temperatures (raster stack) day by day (bands)
tempSumPath <- "C:/MyTemp/myGitLab/windDamage/data/daily_mean_temperature_1961_2018_geotiff"
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
#windpath <- "C:/MyTemp/myGitLab/windDamage/data"

#wind.r <- raster(paste(windpath, "Wind_10y_return_level.tif", sep = "/"))

wind.r <- raster("C:/MyTemp/myGitLab/windDamage/data/Wind_10y_return_level.tif")

# Extract wind speed
df.geom$windSpeed <- raster::extract(wind.r, stand.centr)

rm(wind.r)



# Safe dataframe
df.raster <- df.geom

# get rid of geometry, keep only the data
st_geometry(df.raster) <-NULL


fwrite(df.raster, "C:/MyTemp/myGitLab/windDamage/output/df_glm_raster.csv")




# ---------------------
# Save sf object
# ----------------------
st_write(df.geom, paste(inDataPath, "outKorsnas_att.shp", sep = "/"),
         driver="ESRI Shapefile",
         delete_dsn=TRUE) #




