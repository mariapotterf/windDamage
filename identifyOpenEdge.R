
# Identify nearest neighbours stands
# =====================================

# Process:
# Read shapefile stands data
# get fake tree height among polygons
# calculate medium tree height per stand
# compare medium tree height with his neighbours:
# if the difference betweeen tree height is more than 5 m, assign attribite table: open_edge as TRUE

# --------------------------------

# \\fileservices.ad.jyu.fi\homes\mpotterf\Desktop\2019_selectWatersheds\raw\out_MV_Kitee.gpkg\main.stand
rm(list = ls())

library(ggplot2)  # for choropleth map plot
library(broom) # to convert spatial data to dataframe
library(mapproj)
library(spdep)
library(rgdal)
library(sf)
library(raster)
library(dplyr)
library(spData)


# create simple reproductible example



r <- raster(nrow=6, ncol=6)
values(r) <- matrix(data = c(rep(1,8), 9, rep(1,8), 9), 
                    nrow = 6, ncol = 6)

plot(r)

# Conevrt raster to polygon
pols <- rasterToPolygons(r)

plot(pols, add = T)

# Check if the cell has a neighbour, 
# if yes, add this attribute
# pseudocode:
# subset one cell & remove from others 
# check if cell touches neighbours
# subset those cells
# compare 

# Subset first row in SpatialPolygonDataFrame


one = pols[5, ]

# Keep the remaining polygons
left = pols[-5,]

# Spatially subset the neighbours
gTouches(sp::geometry(one),
         sp::geometry(left), 
         byid = TRUE)  # compare one by one

nbrs <- left[which(gTouches(sp::geometry(one),
                               sp::geometry(left), 
                               byid = TRUE)),]

# Compare if the values are different
height.one = rep(one$layer[1], nrow(nbrs))
height.nbrs = nbrs$layer

# Get the differences between the neighbouring stands
difference = height.one - height.nbrs

# If the difference in more than 5, set open_edge = TRUE 
one$open_edge <- any(difference > 5)

polUpd = one


# apply a for loop: update the open_edge value for all stands
polUpd<-SpatialPolygonsDataFrame()

for (i in seq_along(pols)) {
  print(i)
  
  # subset one polygon
  one = pols[i, ]
  
  # Keep the remaining polygons
  left = pols[-i,]
  
  # Spatially subset the neighbours
  gTouches(sp::geometry(one),
           sp::geometry(left), 
           byid = TRUE)  # compare one by one
  
  # List neighbours
  nbrs <- left[which(gTouches(sp::geometry(one),
                              sp::geometry(left), 
                              byid = TRUE)),]
  
  # Compare if the values are different
  height.one = rep(one$layer[1], nrow(nbrs))
  height.nbrs = nbrs$layer
  
  # Get the differences between the neighbouring stands
  difference = height.one - height.nbrs
  
  # If the difference in more than 5, set open_edge = TRUE 
  one$open_edge <- any(difference > 5)
  
  polUpd = rbind(one)
  
  
}





plot(one, add = T, col = "blue")
plot(left, add = T, col = "red")
plot(nbrs, add = T, col = "yellow")


#




























setwd("//fileservices.ad.jyu.fi/homes/mpotterf/Desktop/2019_selectWatersheds/raw")

gpkgName = "out_MV_Kitee.gpkg"

# Read input forest stand data
forest_fc = readOGR(gpkgName, 
                    layer = "stand")

windows()
plot(forest_fc)

# Add field to attribute table
set.seed(5)

# create fake height
forest_fc@data$treeHeight = runif(nrow(forest_fc), 
                                  min=0, 
                                  max=25)



