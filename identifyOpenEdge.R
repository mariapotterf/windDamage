
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

#forest_sf <- read_sf("forest_fc.shp")


# # --------------------------
# rgeos::buffer and intersect
# --------------------------


# Subset first row in SpatialPolygonDataFrame
i = 1
one = forest_fc[i, ]

# Keep the remaining polygons
left = forest_fc[-i,]

# Create buffer within distance
buff = rgeos::gBuffer(one, width = 40)  # diameter 40 m

# Get neighbors of one cell:
nbrs <- left[which(gTouches(sp::geometry(one),
                            sp::geometry(left), 
                            byid = TRUE)),]

# Compare if the values are different 
height.one  = rep(one@data$treeHeight, nrow(nbrs))
height.nbrs = nbrs@data$treeHeight

# Get the differences between the neighbouring stands
difference = height.one - height.nbrs

# If the difference in at least one stand is 
# in more than 5, set open_edge = TRUE 
# or if no neighbours find


# Does the cell have any neighbors??
# in no neighbors and the tree height is more then 5
# , set open_edge == TRUE 
if_else(nrow(nbrs) == 0 & one$treeHeight > 5, 
        one$open_edge <- TRUE,   #TRUE
        if(any(difference > 5)) one$open_edge <- TRUE)

polUpd = one











plot(forest_sf)





# Calculate the open_edge index
# -----------------
#  Neighbors
# -----------------

# continuity based neighbourhood: 
# import whole 
# shapefile, do not split it by one feature at time
nb <- poly2nb(forest_fc, 
              #row.names = forest_fc,
              snap = 0) # snap corrects for the gaps/slivers 80

# store the number of neighbours by cell
forest_fc$nb_count<- card(nb)
(forest_fc$nb_count)

# plot number of neighbors

# Define my colors
my.palette <- brewer.pal(n = 9, name = "Greens")

windows()
plot(forest_fc, 
     #col = forest_fc$nb_count, 
     col = "white",
     border = "grey")
plot(nb, 
     coordinates(forest_fc), 
     add = T, 
     lwd = 2, 
     col = "black",
     pch = 16,
     cex = 0.5)
text(forest_fc, "nb_count", col = "red", cex = 1.2)

spplot(forest_fc, 
       "nb_count", 
       col.regions = my.palette,
       cuts = length(my.palette) - 1) 

polygonsLabel(coordinates(forest_fc),
              labels=forest_fc$nb_count, 
              method = c("inpolygon"))


# Has the stand an open edge? Is surrounded by neighbors, pre-value is FALSE
forest_fc$open_edge = ifelse(card(nb) <8, "TRUE", "FALSE")

# If has complete neighbors,
# check the differences in height

# Get the position of the cell surrounded by neighbors
center.index <- which(forest_fc$nb_count == 8)

# Get the stand height of a stand
# as a vector to compare element wise
center.height = forest_fc[center.index,]$layer


# Loop through the cells with neighbors:
# keep height of the central stand
# get height of neighbors
# compare the height between them
# if difference is more than 5: => open_edge = TRUE
for (i in seq_along(center.index)) {
  
    # Get central stand height 
  center.height = polys[center.index[i],]$layer

  # Identify neighbors of the stands
  # by the index value
  # print("neighbors are:")
  nb.index = unlist(nb[center.index[i]])

  # print(nb.index)
  # Get heights of the stands
  nb.height = polys[nb.index,]$layer

  # Adjust Center.height length as a vector to compare element wise
  center.height.v = rep(center.height, length(nb.index))

  # Compare the heights 
  h.diff = center.height.v - nb.height
  print(length(h.diff))
  
  center.index[i]
  # if any diference is more than 5
  # if there is not a difference in height, change open_edge = TRUE
  # value to "TRUE"
  if (any(h.diff > 5)) {
    forest_fc@data[center.index[i],]$open_edge <- "TRUE"
  }
}

forest_fc@data




# # Add field to attribute table
# set.seed(5)
# 
# # create fake height
# forest_fc@data$treeHeight = runif(nrow(forest_fc), 
#                                   min=0, 
#                                   max=25)

# remove unnecessary fields to share data
#forest_fc@data <- subset(forest_fc@data, select = c("treeHeight"))

# Export shapefile to share

# writeOGR(obj=forest_fc, 
#          dsn="myDir", 
#          layer="forest_fc", 
#          driver="ESRI Shapefile")




# ------------------------------------
# stack overflow
# =================================

# https://gis.stackexchange.com/questions/291824/determine-if-a-polygon-is-not-enclosed-by-other-polygons

rm(list = ls())

library(spdep)    # neighbours
library(raster)


r <- raster(nrow=11, ncol=6, crs = "+init=epsg:2957")
values(r) <- matrix(data = c(NA,  NA, NA, NA, NA,1,
                             NA, NA, NA, 1, 1, 1, 
                             NA, NA, 2, 1, 3, 1, 
                             NA, NA, 1, 1, 1, 1,
                             NA, NA, 1, 2, 2, 1,
                             NA, NA, 1, 1, 1, NA,
                             NA, 1, 1, 1, 1, NA,
                             NA, 1, 1, 1, 1, NA,
                             NA, 1, 1, 1, 1, NA,
                             NA, 1, 1, 1, NA, NA,
                             NA, 1, NA, NA, NA, NA),
                    nrow = 11,
                    ncol = 6, 
                    byrow = TRUE)


# Convert raster to polygon
polys <- rasterToPolygons(r)


# -----------------
#  Neighbors
# -----------------

# continuity based neighbourhood: 
# import whole 
# shapefile, do not split it by one feature at time
nb <- poly2nb(polys, 
              queen=TRUE, # queen = FALSE refferes to Rook neighborhood
              #row.names = polys,
              snap = 0) # snap corrects for the gaps/slivers


# store the number of neighbours by cell
polys$nb_count<- card(nb)

# Has the stand an open edge? Is surrounded by neighbors, pre-value is FALSE
polys$open_edge = ifelse(card(nb) <max(card(nb)), 1, 0)


# If has complete neighbors,
# check the differences in height
windows()
par(mfrow=c(2,1))
spplot(polys, 
       "open_edge") #, 











# Define my colors
my.palette <- brewer.pal(n = 7, name = "Greens")


plot(polys)
text(polys, 1:length(polys))

spplot(polys, 
       "layer", 
       col.regions = my.palette,
       cuts = length(my.palette) - 1,
       col = "transparent")  # number of cuts smaller by one 

# Check if the cell has a neighbour:
#  neighbourhood by distance, 
#                by continuity
# if no neighbors or open from > = 1 edge: open_edge = TRUE
# if surrounded by neighbours: 
# compare the stand heights between neighbours
#

# -----------------
#  Neighbors
# -----------------

# continuity based neighbourhood: 
# import whole 
# shapefile, do not split it by one feature at time
nb <- poly2nb(polys, 
              queen=FALSE,
              #row.names = polys,
              snap = 0) # snap corrects for the gaps/slivers

# store the number of neighbours by cell
polys$nb_count<- card(nb)

# Has the stand an open edge? Is surrounded by neighbors, pre-value is FALSE
polys$open_edge = ifelse(card(nb) <max(card(nb)), 1, 0)

# If has complete neighbors,
# check the differences in height

spplot(polys, 
       "open_edge") #, 
       #col.regions = my.palette,
       #cuts = length(my.palette) - 1,
       #)  # number of cuts smaller by one col = "transparent"


# Get the position of the cell surrounded by neighbors
center.index <- which(polys$nb_count == 8)

# Get the stand height of a stand
# as a vector to compare element wise
center.height = polys[center.index,]$layer


# Loop through the cells with neighbors:
# keep height of the central stand
# get height of neighbors
# compare the height between them
# if difference is more than 5: => open_edge = TRUE
for (i in seq_along(center.index)) {
  
  # get index of the stand in a center
  print(i)
  print(center.index[i])
  
  # Get central stand height 
  center.height = polys[center.index[i],]$layer
  print(center.height)
  
  # Identify neighbors of the stands
  # by the index value
  # print("neighbors are:")
  nb.index = unlist(nb[center.index[i]])
  print(nb.index)
  class(nb.index)
  # print(nb.index)
  # Get heights of the stands
  nb.height = polys[nb.index,]$layer
  print(nb.height)
  
  # Adjust Center.height length as a vector to compare element wise
  center.height.v = rep(center.height, length(nb.index))
  (center.height.v)
  
  # Compare the heights 
  h.diff = center.height.v - nb.height
  print(length(h.diff))
  
  center.index[i]
  # if any diference is more than 5
  # if there is not a difference in height, change open_edge = TRUE
  # value to "TRUE"
  if (any(h.diff > 5)) {
    polys@data[center.index[i],]$open_edge <- "TRUE"
  }
}

polys@data


















# convert to full binary adjacency matrix
mm = nb2mat(nb, zero.policy = TRUE, style = "B")



isTRUE(all.equal(nb, polys, check.attributes = FALSE))

windows(height = 3, width = 6)
par(mfrow = c(1,2))


plot(r)
plot(polys, add = T)
rgeos::polygonsLabel(polys, labels = row.names(polys))




plot(polys, border = "grey", lwd = 0.5)
plot(nb, coordinates(polys), points=FALSE, add = T, lwd = 0.7)

# extract the number of neighbours
card(nb)

# add number of neighbours to original polygon
polys$nb_count<- card(nb)

# check the quality of the objects
summary(nb)

# 
summary(nb, coordinates(polys))  # if coordinates are provided
#


# --------------------------
# rgeos::gTouches
# --------------------------


# Subset first row in SpatialPolygonDataFrame
i = 1
one = polys[i, ]

# Keep the remaining polygons
left = polys[-i,]

nbrs <- left[which(gTouches(sp::geometry(one),
                               sp::geometry(left), 
                               byid = TRUE)),]

# Compare if the values are different
height.one  = rep(one$layer[1], nrow(nbrs))
height.nbrs = nbrs$layer

# Get the differences between the neighbouring stands
difference = height.one - height.nbrs

# If the difference in at least one stand is 
# in more than 5, set open_edge = TRUE 
# or if no neighbours find
one$open_edge <- any(difference > 5)

polUpd = one





# # --------------------------
# rgeos::buffer and intersect
# --------------------------


# Subset first row in SpatialPolygonDataFrame
i = 10
one = polys[i, ]

# Keep the remaining polygons
left = polys[-i,]

# Create buffer within distance
buff = buffer(one, width = 100)



# subset set of neighbours by spatial overlap
nbrs <- left[which(gContains(sp::geometry(buff),
                                    sp::geometry(left), byid = T)),]


# Compare if the values are different
height.one  = rep(one$layer[1], nrow(nbrs))
height.nbrs = nbrs$layer

# Get the differences between the neighbouring stands
difference = height.one - height.nbrs

# If the difference in at least one stand is 
# in more than 5, set open_edge = TRUE 
# or if no neighbours find
one$open_edge <- any(difference > 5)


# 
polUpd = one



plot(polys)
plot(left, add = T, col = "green")
plot(one, add = T, col = "red")
plot(buff, add = T, col = "blue")
plot(nbrs, add = T, col = "orange")






for (i in seq_along(polys)) {
  
  print(i)
  
  # subset one polygon
  one = polys[i, ]
  
  # Keep the remaining polygons
  left = polys[-i,]
  
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




















# -----------------------------------

# spatial overlay



# ----------------------------------

nb.cover <- left[gCovers(sp::geometry(buff),
                         sp::geometry(left),
                         byid = TRUE),]

nb.over<- left[over(sp::geometry(buff),
                    sp::geometry(left)),]

class(over(buff, left))



nb.contains <- left[which(gContainsProperly(sp::geometry(buff),
                                            sp::geometry(left), byid = T)),]


gCoveredBy(sp::geometry(buff),
           sp::geometry(left), byid = T)



nbrs <- left[which(over(sp::geometry(buff),
                        sp::geometry(left), 
                        byid = TRUE)),]


# --------------------------------





