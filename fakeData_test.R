

# Test new library 
# sf objects

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

