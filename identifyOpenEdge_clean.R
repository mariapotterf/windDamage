
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

library(ggplot2)  
library(spdep)    # neighbours
library(rgdal)
library(rgeos)
library(sf)
library(raster)
library(dplyr)
library(spData)
library(sf)
#library(RColorBrewer)


# Try real data

setwd("U:/Desktop/2019_selectWatersheds/raw/myDir")

# Read input forest stand data
forest_fc = readOGR(getwd(), 
                    layer = "forest_fc")

my.sf = read_sf("forest_fc.shp")

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


# ==================================

# Make a function using 'simplefeature' approach??


library(spdep)    # neighbours
library(raster)


r <- raster(nrow=11, ncol=6, crs = crs(forest_fc)) #"+init=epsg:2957"
values(r) <- matrix(data = c(20,  NA, NA, NA, NA,20,
                             NA, NA, NA, 20, 20, 20, 
                             NA, NA, 20, 20, 20, 20, 
                             NA, NA, 20, 20, 20, 20,
                             NA, NA, 20, 20, 20, 20,
                             NA, NA, 20, 20, 20, 20,
                             NA, 20, 20, 20, 20, 20,
                             NA, 20, 20, 20, 20, 20,
                             NA, 20, 20, 20, 20, 20,
                             NA, 20, 20, 20, 20, 20,
                             NA, 20, 20, 20, 20, 20),
                    nrow = 11,
                    ncol = 6, 
                    byrow = TRUE)


# Convert raster to polygon
forest_fc <- rasterToPolygons(r)


# run on my data
# ----------------------------------
# make fake stand tree height numbers

#my.sf$treeHeight <-1:nrow(my.sf)

my.sf$open_edge <- FALSE

# Subset the data to create two independent shps
#i = 46

for (i in 1:nrow(my.sf)) {
  print(i)
  
  # define stands and leftover forest
  one  = my.sf[i, ]
  left = my.sf[-i,]
  
  # Create buffer and intersectb buffer with neighbors: evalues if any are left?
  buff = st_buffer(one, 10) # distance
  
  # Subset the polygons that overlaps with the buffer
  nbrs.buff <- left[st_intersects(buff, left, sparse =  FALSE),]
  
  # If conditions to determine if the forest has open edge or not
  if (nrow(nbrs.buff) == 0) {
    my.sf[i,]$open_edge <- TRUE  
    
  } else {  # neighbors are smaller than the stands
    
    # Compare the height of the stands: 
    height.one  = rep(one$treeHeight, nrow(nbrs.buff))
    height.nbrs = nbrs.buff$treeHeight
    
    # Get the differences between the neighbouring stands
    difference = height.one - height.nbrs
    
    # compare here the tree heights of stands
    if(any(difference > 5)) {
      my.sf[i,]$open_edge <- TRUE
      
      # Check if there is a big gap in neighborhood    
    } else {                     
      
      # Get the difference between two shapefiles???
      # Add `one` to `neighbors` and dissolve (union) inner boundaries  
      u <- st_union(rbind(nbrs.buff, one))
      
      # Erase existing stands from the buffer
      int.buff.one = st_difference(st_geometry(buff), st_geometry(u)) 
      
      # check if gaps exists 
      if (length(int.buff.one) > 0 ) {
     
        # Calculate area of intersected data
        int.buff.one.area = st_area(int.buff.one)
        
        if (as.numeric(int.buff.one.area) > 16*16)  {
          my.sf[i,]$open_edge <- TRUE
        }
      }
    }
  }
}





# -------------------------------
# Create sf and spdf input data
# -------------------------------

# SF
my.sf<-st_as_sf(forest_fc)
names(my.sf) <- c("treeHeight", "geometry")


# SPDF
forest_fc@data$treeHeight <-  forest_fc@data$layer




# --------------------------------------------------
# Make open_edge function for sf objects
# --------------------------------------------------

findOpenEdge_sf <- function(sf, treeHeight, distance = 40, pixel.width = 16, ...) {
  
  # loop through the dataframe
  sf$open_edge <- FALSE
  
  for (i in 1:nrow(sf)) {
    
    # define stands and leftover forest
    one  = sf[i, ]
    left = sf[-i,]
    
    # Create buffer and intersectb buffer with neighbors: evalues if any are left?
    buff = st_buffer(one, distance) # distance
    
    # Subset the polygons that overlaps with the buffer
    nbrs.buff <- left[st_intersects(buff, left, sparse =  FALSE),]
    
    # If conditions to determine if the forest has open edge or not
    if (nrow(nbrs.buff) == 0) {
      sf[i,]$open_edge <- TRUE  
      
    } else {  # neighbors are smaller than the stands
      
      # Compare the height of the stands: 
      height.one  = rep(one$treeHeight, nrow(nbrs.buff))
      height.nbrs = nbrs.buff$treeHeight
      
      # Get the differences between the neighbouring stands
      difference = height.one - height.nbrs
      
      # compare here the tree heights of stands
      if(any(difference > 5)) {
        sf[i,]$open_edge <- TRUE
        
        # Check if there is a big gap in neighborhood    
      } else {                     
        
        # Get the difference between two shapefiles???
        # Add `one` to `neighbors` and dissolve (union) inner boundaries  
        u <- st_union(rbind(nbrs.buff, one))
        
        # Erase existing stands from the buffer
        int.buff.one = st_difference(st_geometry(buff), st_geometry(u)) 
        
        # check if gaps exists 
        if (length(int.buff.one) > 0 ) {
          
          # Calculate area of intersected data
          int.buff.one.area = st_area(int.buff.one)
          
          if (as.numeric(int.buff.one.area) > pixel.width^2)  {
            sf[i,]$open_edge <- TRUE
          }
        }
      }
    }
  }
  return(sf) 
} 


# CHeck execution time
start_time <- Sys.time()
sf.open<- findOpenEdge_sf(sf = my.sf, treeHeight=treeHeight, distance = 10, pixel.width = 16)
end_time <- Sys.time()

end_time - start_time



# --------------------------------------------------
# Make open_edge function for spdf objects
# --------------------------------------------------


# Make a function: find open edge

identifyOpenEdge_spdf <- function(spdf, treeHeight, distance = 40, pixel.width = 16, ...) {
  
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

start_time <- Sys.time()
spdf.open = identifyOpenEdge_spdf(spdf = forest_fc, treeHeight = treeHeight, distance = 10)
end_time <- Sys.time()

end_time - start_time













# ============================================
# 
#   Compare execution time
#
# ============================================

# between functions for spdf and sf objects
#

sleep_for_a_30sec <- function() { Sys.sleep(30) }

start_time <- Sys.time()
sleep_for_a_30sec()
end_time <- Sys.time()

end_time - start_time
































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










# ---------------------------------------
#
# working example

# -----------------------------------


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
















