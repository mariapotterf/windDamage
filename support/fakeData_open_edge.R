

# 
rm(list = ls())

# Read the fin\unction

setwd("C:/MyTemp/myGitLab/windDamage")
source("myFunctions.R")


# Test new library 
# sf objects


library(ggplot2)  # for choropleth map plot
library(spdep)    # neighbours
library(rgdal)
library(rgeos)
library(sf)
library(raster)
library(dplyr)
library(spData)
library(sf)

# Read real data just to get crs
# Set working directory
setwd("U:/projects/2019_windthrowModel/Janita/outSimulated")

# Read stand geometry
df.geom = read_sf("MV_Korsnas.shp")






# Test if the fuinction correctly loops over lit of data

r1 <- raster(nrow=11, ncol=6, crs = crs(df.geom)) #"+init=epsg:2957"
values(r1) <- matrix(data = c(20,  NA, NA, NA, NA,20,
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
my.sf1<-st_as_sf(rasterToPolygons(r1))
names(my.sf1) <- c("treeHeight", "geometry")




# make raster r2
r2 <- r1
values(r2) <- matrix(data = c(20,  NA, NA, NA, NA,20,
                              NA, NA, NA, 20, 20, 20, 
                              NA, NA, 20, 20, 20, 20, 
                              NA, NA, 20, 20, 20, 20,
                              NA, NA, 20, 20, 20, 20,
                              NA, NA, 20, 20, 20, 20,
                              NA, 20, 20, 20, 20, 20,
                              NA, 20, 20,  0,  0, 20,
                              NA, 20, 20,  0,  0, 20,
                              NA, 20, 20, 20, 20, 20,
                              NA, 20, 20, 20, 20, 20),
                     nrow = 11,
                     ncol = 6, 
                     byrow = TRUE)


# Convert raster to polygon
my.sf2<-st_as_sf(rasterToPolygons(r2))
names(my.sf2) <- c("treeHeight", "geometry")


# Make raster 3

r3 <- r1
values(r3) <- matrix(data = c(20,  NA, NA, NA, NA,20,
                              NA, NA, NA, 20, 20, 20, 
                              NA, NA, 5,  8, 20, 20, 
                              NA, NA, 20, 10, 20, 20,
                              NA, NA, 20, 20, 0, 20,
                              NA, NA, 20, 20, 20, 20,
                              NA, 20, 20, 20, 0, 20,
                              NA, 20, 20, 20, 20, 20,
                              NA, 20, 20, 0, 20, 20,
                              NA, 20, 20, 20, 20, 20,
                              NA, 20, 1, 0, 0, 20),
                     nrow = 11,
                     ncol = 6, 
                     byrow = TRUE)


# Convert raster to polygon
my.sf3<-st_as_sf(rasterToPolygons(r3))
names(my.sf3) <- c("treeHeight", "geometry")












# list without open_edge calculated
my.ls1<-list(my.sf1, my.sf2)
my.ls2<-list(my.sf1, my.sf2, my.sf3)



# Try ifg open_edge function works on both:
sf1<-findOpenEdge_sf(sf = my.sf1, 
                     treeHeight=treeHeight, 
                     distance = 10, 
                     pixel.width = 16)


sf2<-findOpenEdge_sf(sf = my.sf2, 
                     treeHeight=treeHeight, 
                     distance = 10, 
                     pixel.width = 16)


plot(sf1)
plot(sf2)



# seems that individually algorith works

# Put data together in a list of sfs objects, try again
my.ls<-list(sf1, sf2)



# run the function just on one of them
my.ls[[2]]


plot(my.ls[[2]])

# are teh files in a list and original identical??
identical(my.ls[[2]], sf2)



# Does the function works on a element of a list?
outLs1<-findOpenEdge_sf(sf = my.ls1[[2]], 
                treeHeight=treeHeight, 
                distance = 10, 
                pixel.width = 16)


# Lapply a function over the list 
out<-lapply(my.ls2, findOpenEdge_sf)

plot(out[[3]])





#my.sf$open_edge <- FALSE


# Create output:
sf.open<- findOpenEdge_sf(sf = my.sf, treeHeight=treeHeight, distance = 10, pixel.width = 16)




# ==================================
# Test data on true geometry
# -----------------------------------------------

# Set working directory
setwd("U:/projects/2019_windthrowModel/Janita/outSimulated")

# read simulated data
df <- read.csv("rsl_without_MV_Korsnas.csv", sep = ";")  # without == climate change is not included

# Read stand geometry
df.geom = read_sf("MV_Korsnas.shp")
#df.geom = readOGR(dsn = getwd(),
#                    layer = "MV_Korsnas")



# Test if the function works: create new H_dom variable, 
# fill in with pre-defined colours
# make for multiple landscapes (time)
# to see if the function works

# clean up unnecessary columns
out<- subset(df.geom, select = c("standid"))

sf1<-out
sf2<-out
sf3<-out

# Create new landscapes with different tree heights 
sf1$H_dom <- rep(20, nrow(sf1))

# CReate another landscape
sf2$H_dom <- rep(c(20, 30), each = nrow(df.geom)/2)
sf3$H_dom <- rep(c(20, 30), nrow(df.geom)/2)




# Calculate open edge
sf1.open =  findOpenEdge_sf(sf = sf1, H_dom = H_dom, distance = 10, pixel.width = 16)
sf2.open =  findOpenEdge_sf(sf = sf2, H_dom = H_dom, distance = 10, pixel.width = 16)
sf3.open =  findOpenEdge_sf(sf = sf3, H_dom = H_dom, distance = 10, pixel.width = 16)

plot(sf1.open["open_edge"])
plot(sf2.open["open_edge"])
plot(sf3.open["open_edge"])

































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
buff35 = st_buffer(one, 35000 ) # distance
buff50 = st_buffer(one, 50000)

# CHeck which polygons overlaps with my buffer
out.overlap = st_overlaps(buff, left)


# Subset the polygons that overlaps with the buffer
nbrs.buff35 <- left[st_overlaps(buff35,left)[[1]],]
nbrs.buff50 <- left[st_overlaps(buff50,left)[[1]],]



# Merge together `neighbors`` and `one`
u <- st_union(rbind(nbrs.buff, one))

int.buff.one = st_difference(buff, u) 

int.buff.one.area <- st_area(int.buff.one)

library(gridExtra)

p1 <- ggplot(nbrs.buff50) +
  geom_sf(fill = "lightblue") +
  geom_sf(data = buff50, fill = NA, col = "red", lwd = 1)
  
p2 <- ggplot(nbrs.buff35) +
  geom_sf(fill = "grey") +
  geom_sf(data = buff35, fill = NA, col = "red", lwd = 1)

grid.arrange(p1, p2,  nrow = 1)




# ---------------------------------------------------

# Why overlay operation does not subset the completely overlapped features?
# ----------------------------------------------------


# Load data
shp = system.file("shape/nc.shp", package="sf")

my.sf <- st_read(shp, quiet = TRUE)

# Convert crs to projected system to make buffer
my.sf.web<- st_transform(my.sf, 3857)

# Subset the data to create two independent shps
i = 10

# Split datasets in two files
one  = my.sf.web[i, ]
#left = my.sf.web[-i,]

# Create buffer 
#buff35 = st_buffer(one, 35000 ) # distance
buff50 = st_buffer(one, 50000)

# CHeck which polygons overlaps with my buffer
out.overlap = st_overlaps(buff50, my.sf.web)

out.overlap <- my.sf.web[st_intersects(buff50, my.sf.web, sparse =  FALSE),]



# Subset the polygons that overlaps with the buffer
#nbrs.buff35 <- left[st_overlaps(buff35,left)[[1]],]
nbrs.buff50 <- my.sf.web[st_overlaps(buff50,my.sf.web)[[1]],]
#nbrs.buff50 <- my.sf.web[st_covers(buff50,my.sf.web)[[1]],]
nbrs.buff50 <- my.sf.web[st_within(buff50,my.sf.web)[[1]],]



library(gridExtra)

ggplot(buff50) +
  geom_sf(data = my.sf.web, fill = "lightgrey") +
    geom_sf(data = nbrs.buff50, fill = "red") +
    geom_sf(fill = NA, col = "green", lwd = 2) +
  geom_sf(data = out.overlap, fill = "violet")
  

ggplot(out.overlap) +
  geom_sf(data = out.overlap) +
  geom_sf(data = buff50, col = "red", fill = NA)


p2 <-
  ggplot(nbrs.buff50) +
  geom_sf(fill = "lightgrey") +
  geom_sf(data = buff50, fill = NA, col = "red", lwd = 1)


grid.arrange(p1, p2,  nrow = 1)

