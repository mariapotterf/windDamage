

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



