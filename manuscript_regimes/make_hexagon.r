# ------------------------------------------
# CReate hexagon to fill in information for stands and have a continuous landscape
# ------------------------------------------

rm(list = ls())



# Get the watershed data
library(sf)
library(dplyr)
require(data.table)
library(tidyr)


# Stands geometry
df.watershed <- st_read("C:/MyTemp/avohaakut_db/14.534/14.534/14.534.shp") # read watershed


# Make a grid 
g = st_make_grid(df.watershed, 
                 cellsize = 170,  # at 200 cell size we have 917 fcs
                 #cellsize = c(diff(st_bbox(df.geom)[c(1, 3)]), 
                   #           diff(st_bbox(df.geom)[c(2, 4)]))/n,
                 square = FALSE)  # make hexagon, not square

# Convert st object to s objects
g2<- st_as_sf(g)

# Export the new file
st_write(g2, paste( getwd(), "manuscript_regimes/output/hexa.shp", sep = "/"))
