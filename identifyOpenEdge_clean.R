
# Identify nearest neighbours stands
# =====================================

# Process:
# Read shapefile stands data
# subset one stand
# identify neighbours: # https://walkerke.github.io/2016/07/spatial-neighbors-in-r---an-interactive-illustration/
#   - by distance (if gaps between stands...)
#   - by continuity: rook & queen neigh
# create surrounding buffer: outside only
# subset all stands falling in the outside buffer
# compare tree height between stand in the middle and stands in buffer
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


# Try real data

setwd("U:/Desktop/2019_selectWatersheds/raw/myDir")

# Read input forest stand data
forest_fc = readOGR(getwd(), 
                    layer = "forest_fc")

# Identify open edge:
# read shp forest data row by row
# for each cell in the middle: 
# create buffer 16*5 = *) = diameter, r = polomer
# subset all stands falling within the buffer
# intersect neighbors woth buffer: is the area > 16*16?? 256 m? 
#     yes = open_edge == TRUE
#     no = compare tree tree height between center and neighbors
# is the difference in tree height > 5 m? 
#     yes = open_edge == TRUE
#     no = open_edge == FALSE

# which one is less computationally intense??



# Create new variable
forest_fc@data$open_edge <- FALSE


# loop through the dataframe
forest_fc@data$open_edge <- FALSE
for (i in seq_along(forest_fc)) {
  
  # define stands and leftover forest
  one  = forest_fc[i, ]
  left = forest_fc[-i,]
  
  # Create buffer and intersectb buffer with neighbors: evalues if any are left?
  buff = buffer(one, 40)
  
  
  # Identify neighbors 
  nbrs.buff <- left[which(gOverlaps(sp::geometry(buff),
                                    sp::geometry(left), 
                                    byid = TRUE)),]
  
  # Conditions for open edge
  if (nrow(nbrs.buff) == 0) {
    forest_fc@data[i,]$open_edge <- TRUE  
   
  } else {
    
    # Compare the height of the stands: 
    height.one  = rep(one@data$treeHeight, nrow(nbrs.buff))
    height.nbrs = nbrs.buff@data$treeHeight
    
    # Get the differences between the neighbouring stands
    difference = height.one - height.nbrs
    
    # compare here the tree heights of stands
    if(any(difference > 5)) {
      forest_fc@data[i,]$open_edge <- TRUE
    } else {                    # Check for the gap by the buffer 
     
      # Get the difference between two shapefiles???
      int.buff.one = rgeos::gDifference(buff, nbrs.buff + one)
      
      # Is the size of the openning larger than one pixel 16x16 m? 
      if (!is.null(int.buff.one) ) {
        
        # Calculate area of intersected data
        int.buff.one.area = gArea(int.buff.one)
        
        if (int.buff.one.area > 16*16) {
          forest_fc@data[i,]$open_edge <- TRUE
        }
      }
    }
  }
}



forest_fc@data


plot(forest_fc)



# Make a function

defineOpenEdge <- function(spdf, treeHeight, distance = 40, pixel.width = 16, ...) {
  
  # loop through the dataframe
  spdf@data$open_edge <- FALSE
  for (i in seq_along(spdf)) {
    
    # define stands and leftover forest
    one  = spdf[i, ]
    left = spdf[-i,]
    
    # Create buffer and intersectb buffer with neighbors: evalues if any are left?
    buff = buffer(one, distance)
    
    
    # Identify neighbors 
    nbrs.buff <- left[which(gOverlaps(sp::geometry(buff),
                                      sp::geometry(left), 
                                      byid = TRUE)),]
    
    # Conditions for open edge:
    #    - no neighbors
    if (nrow(nbrs.buff) == 0) {
      spdf@data[i,]$open_edge <- TRUE  
      
    } else {  # neighbors are smaller than the stands
      
      # Compare the height of the stands: 
      height.one  = rep(one@data$treeHeight, nrow(nbrs.buff))
      height.nbrs = nbrs.buff@data$treeHeight
      
      # Get the differences between the neighbouring stands
      difference = height.one - height.nbrs
      
      # compare here the tree heights of stands
      if(any(difference > 5)) {
        spdf@data[i,]$open_edge <- TRUE
      
      # Check if there is a big gap in neighborhood    
      } else {                     
        
        # Get the difference between two shapefiles???
        int.buff.one = rgeos::gDifference(buff, nbrs.buff + one)
        
        # Is the size of the openning larger than one pixel 16x16 m? 
        if (!is.null(int.buff.one) ) {
          
          # Calculate area of intersected data
          int.buff.one.area = gArea(int.buff.one)
          
          if (int.buff.one.area > 16*16) {
            spdf@data[i,]$open_edge <- TRUE
          }
        }
      }
    }
  }
 return(spdf) 
} 


# Create output file:
windows()
outfc = defineOpenEdge(spdf = forest_fc, treeHeight = treeHeight, distance = 10)


plot(outfc)

# convert to simple feature
out_sf<-st_as_sf(outfc) #


ggplot(out_sf) +
  geom_sf(aes(fill = open_edge))
 





plot(forest_fc)
plot(nbrs, col = "lightgreen", add = T)
plot(buff, add = T)

plot(one, add = T, col = "red")
plot(nbrs.buff, add = T, col = "blue")
plot(int.buff.one, add = T, col = "darkgreen")


# how to account for gap or slivers?











