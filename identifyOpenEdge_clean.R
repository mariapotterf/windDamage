
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
#library(broom) # to convert spatial data to dataframe
#library(mapproj)
#library(maptools)

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






# run on my data
# ----------------------------------

my.sf$open_edge <- FALSE

# Subset the data to create two independent shps
i = 16

for (i in 1:nrow(my.sf)) {
  
  # define stands and leftover forest
  one  = my.sf[i, ]
  left = my.sf[-i,]
  
  # Create buffer and intersectb buffer with neighbors: evalues if any are left?
  buff = st_buffer(one, 5) # distance
  
  # Subset the polygons that overlaps with the buffer
  nbrs.buff <- left[st_overlaps(buff,left)[[1]],]
  
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
      
      
      # Is the size of the openning larger than one pixel 16x16 m? 
      if (!is.null(int.buff.one) ) {
        
        # Calculate area of intersected data
        int.buff.one.area = st_area(int.buff.one)
        
        if (as.numeric(int.buff.one.area) > 16*16) {
          my.sf[i,]$open_edge <- TRUE
        }
      }
    }
  }
}

my.sf[i,]$open_edge



# There are different results with nbrs.buff10 and 40??
height.one  = rep(one$treeHeight, nrow(nbrs.buff10))
height.nbrs10 = nbrs.buff10$treeHeight
difference10 = height.one - height.nbrs10


height.one  = rep(one$treeHeight, nrow(nbrs.buff40))
height.nbrs40 = nbrs.buff40$treeHeight
difference40 = height.one - height.nbrs40




ggplot(my.sf) + 
  geom_sf(aes(fill = open_edge)) +
  geom_sf_label(aes(label =  row.names(my.sf)))



ggplot(my.sf) + 
  geom_sf() +
  geom_sf(data = nbrs.buff40, fill = "lightblue") +
  geom_sf(data = nbrs.buff10, fill = "lightgreen") +
  geom_sf(data = buff40, fill = "blue") +
  geom_sf(data = buff10, fill = "green") +
  #geom_sf(data = nbrs.buff) + 
  geom_sf(data = one, fill = "red") 





ggplot(int.buff.one) +
  geom_sf() #+
geom_sf(data = buff, aes(color = "red")) +
  geom_sf(data = nbrs.buff) +
  geom_sf(data = one) +
  geom_sf(data = int.buff.one, aes(col = "yellow")) 






# ---------------------------------------
# example: correct erase tool??
# ----------------------------------------

library(ggplot2)  # for choropleth map plot
library(spdep)    # neighbours
library(rgdal)
library(rgeos)
library(sf)
library(raster)
library(dplyr)
library(spData)
library(sf)


# Load data
shp = system.file("shape/nc.shp", package="sf")

my.sf <- st_read(shp, quiet = TRUE)

# Convert crs to projected system to make buffer
my.sf.web<- st_transform(my.sf, 3857)

# Subset the data to create two independent shps
i = 10

# Split datasets in two files
one  = my.sf.web[i, ]
left = my.sf.web[-i,]

# Create buffer 
buff = st_buffer(one, 35000 ) # distance


# CHeck which polygons overlaps with my buffer
out.overlap = st_overlaps(buff, left)


# Subset the polygons that overlaps with the buffer
nbrs.buff <- left[st_overlaps(buff,left)[[1]],]

# Merge together `neighbors`` and `one`
u <- st_union(rbind(nbrs.buff, one))

#u <- st_union(st_geometry(nbrs.buff), st_geometry(one), by_feature = FALSE)
#u <- st_union(st_combine(st_geometry(nbrs.buff)), 
 #             st_combine(st_geometry(one)))
int.buff.one = st_difference(buff, u) 

int.buff.one.area <- st_area(int.buff.one)










ggplot(buff) + 
  geom_sf() +
  geom_sf(data = nbrs.buff) +
  geom_sf(data = int.buff.one, aes(col = "violet")) +
  geom_sf(data = u, aes(col = "yellow")) #+
  #geom_sf(data = one, aes(col = "red")) +
  #geom_sf(data = buff)


library(ggpubr)

p1 <- ggplot(u) + 
  geom_sf() + 
  geom_sf(data = buff, fill = "red") + 
  ggtitle("Buffer") + theme_bw()

p2 <- ggplot(u) + 
  geom_sf(data = buff) +
  geom_sf( fill = "red") +
  #geom_sf(data = int.buff.one, fill = "violet") +
  ggtitle("Union")  + theme_bw()

p3 <- ggplot(buff) + 
  geom_sf() +
  geom_sf(data = nbrs.buff) +
  geom_sf(data = int.buff.one, fill = "red") +
  geom_sf(data = u, fill = NA, col  = "black") +
  ggtitle("Erase buffer - Union") + theme_bw()

  

library("gridExtra")
grid.arrange(p1, p2, p3, ncol = 3, nrow = 1)































st_overlaps(buff,left)[[1]]



ggplot(my.sf.web) + geom_sf(aes(fill = AREA))  +
   geom_sf(data = buff, aes(color = "red")) +
  geom_sf(data = one) +
  geom_sf(data = nbrs.buff)





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





# Make a function: find open edge

identifyOpenEdge <- function(spdf, treeHeight, distance = 40, pixel.width = 16, ...) {
  
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
outfc = identifyOpenEdge(spdf = forest_fc, treeHeight = treeHeight, distance = 10)


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
















