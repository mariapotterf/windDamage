
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

# Udp: 28.4. - try to split oepn_edge identification in two parts:
# first find neighbors, find cells with constantly open edge
# second: compare teh heights between uncertain
# purpose: run tehbuffer, erase, intersect just once in year 2016, further just compare 
# cell with its neighbors

# --------------------------------

rm(list = ls())

library(ggplot2)  
library(spdep)    # neighbours
library(rgdal)
library(rgeos)
library(sf)
library(raster)
library(dplyr)
library(spData)
library(sp)


# Try real data

#setwd("U:/Desktop/2019_selectWatersheds/raw/myDir")

setwd("//fileservices/Homes/mpotterf/projects/2019_selectWatersheds/raw/myDir")
# Read input forest stand data
#forest_fc = readOGR(getwd(), 
   #                 layer = "forest_fc")

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

my.sf$id    <- paste("a", 1:nrow(my.sf), sep = "_")


# Get simulated data:
df.sim <- my.sf
df.sim$H_dom <- rep(10, nrow(df.sim))

st_geometry(df.sim) <- NULL

# First find couples central - neighbors 
# exports list of dataframes
# -------------------------------
# define stands and leftover forest


# Get the geometry and neighbors of stands
# export ad a list of dataframes
# function loops throught stands

find_nbrs_geom <- function(sf, ...) {
  
  # create output to store the results
  nbrs.df.ls <- list()
  
  for (i in 1:nrow(sf)) {
   # i = 11
    
    one  = sf[i, ]
    left = sf[-i,]
    
    # Create buffer and intersectb buffer with neighbors: evalues if any are left?
    buff = st_buffer(one, 10) # distance
    
    # Subset the polygons that overlaps with the buffer
    nbrs.buff <- left[st_intersects(buff, left, sparse =  FALSE),]
    
    # Add `one` to `neighbors` and dissolve (union) inner boundaries  
    u <- st_union(rbind(nbrs.buff, one))
    
    # Erase existing stands from the buffer
    int.buff.one = st_difference(st_geometry(buff), st_geometry(u)) 
    
    # Calculate area of intersected data
    int.buff.one.area = st_area(int.buff.one)
    
    # Create output dataframe
    central   <- one$id
    nbrs      <- if(nrow(nbrs.buff) == 0) (0) else (nbrs.buff$id) 
    open_area <- round(as.numeric(int.buff.one.area),1)
    
    # Create dataframes
    nbrs.df <- data.frame(central, nbrs, open_area)
    
    # add datyaframe to output dataframe list
    nbrs.df.ls[[i]] <- nbrs.df
    
  }
  return(nbrs.df.ls)
}

nbrs<- find_nbrs_geom(sf = my.sf)




# Use couple central-neighbors to compare tree heights or set open_edge = T
#  ------------------------------
# this should loop throuugh landscape
open_edge_by_nbrs <- function(nbrs, 
                             df.sim,...) {
  
   # get ids of the central stand and neighbors
  central_id <- as.character(unique(nbrs$central))
  nbrs_id    <- unique(nbrs$nbrs)
  
  print(central_id)
  
  # Get the stand height from simulated data for central stand and 
  # neighbors 
  central_H = rep(subset(df.sim, id %in% central_id)$H_dom, 
                  length(nbrs_id))
  nbrs_H    = rep(subset(df.sim, id %in% nbrs_id)$H_dom, 
                  length(nbrs_id))

  # Evaluate if the stand has open gap near by
  if (unique(nbrs$open_area) > 16*16) {
    output<-c(central_id,"TRUE")
    print(output)
    return(output)
  } else {
    # Get the differences between the neighbouring stands
    difference = central_H - nbrs_H
    
    # if any difference is more then 5
    if(any(difference > 5)) {
    
      output<-c(central_id,"TRUE")
      print(output)
      return(output)
      
      # Check if there is a big gap in neighborhood    
    } else {
      
      output<-c(central_id,"FALSE")
      print(output)
      return(output)
    }
  }
}


# apply function for all cells (one time series)
out.ls<- lapply(nbrs, function(i) open_edge_by_nbrs(nbrs = i, 
                                                   df.sim = df.sim))

# for simulated data: yearly changes would woork 
# once to find teh neighbors, then 20 times to define_eopn_edge among neighbors






















# Fill 




# Make a function using 'simplefeature' approach


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
# Example for raster data
#my.sf<-st_as_sf(forest_fc)
#names(my.sf) <- c("treeHeight", "geometry")


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

# Create output:
sf.open<- findOpenEdge_sf(sf = my.sf, treeHeight=treeHeight, distance = 10, pixel.width = 16)

# Real data:
sf.open<- findOpenEdge_sf(sf = my.sf, treeHeight=treeHeight, distance = 10, pixel.width = 16)



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

spdf.open = identifyOpenEdge_spdf(spdf = forest_fc, treeHeight = treeHeight, distance = 10)













# ============================================
# 
#   Compare execution time
#
# ============================================

# between functions for spdf and sf objects
#


# SF
start_time <- Sys.time()
sf.open<- findOpenEdge_sf(sf = my.sf, treeHeight=treeHeight, distance = 10, pixel.width = 16)
end_time <- Sys.time()

end_time - start_time




# SPDF
start_time <- Sys.time()
spdf.open = identifyOpenEdge_spdf(spdf = forest_fc, treeHeight = treeHeight, distance = 10)
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
















