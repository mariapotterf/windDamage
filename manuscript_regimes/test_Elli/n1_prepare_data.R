
# ----------------------------------------
# process Ellinoora data for no CC, and climate change included
# ----------------------------------------

# steps:
# Get input data:
#   - get stand geometry from GPKG - already derived from previosu project
#   - get simulated regimes: for no change, CC4.5, CC8.5
# select which regimes to use
#     need the most stands with regimes
#     have the same stand id for geometry and for simulated data
#     if unreal values - replace by mean?

# Calculate wind risk values - prepare data
# make landscapes by year


# 
rm(list = ls())


setwd("C:/MyTemp/myGitLab/windDamage")

# Read libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(rgdal)
library(ggspatial)
library(rgeos)
library(raster)
library(dplyr)
library(spData)
library(sf)
library(RColorBrewer)


# Correct order of variables, factors & levels
# need to have same names of columns?? YES
# when data are sourced (source()), they are all available in my script
source("C:/MyTemp/myGitLab/windDamage/myFunctions.R")



# get data
# ----------------------

# Read corrected simulated names:
df.no <- data.table::fread("C:/MyTemp/myGitLab/windDamage/manuscript_regimes/input_CC/CC45_SA_MV_Korsnas_rsu.csv", 
                       data.table=FALSE, 
                       stringsAsFactors = FALSE)

# Stands geometry
df.geom <- st_read("C:/MyTemp/Ellinoora_CC/input/watershedsToSIMO/watershedsToSIMO/output/MV_Korsnäs.shp")
#df.geom <- subset(df.geom, select = c("KUVIO_ID"))
#names(df.geom) <- c("id", "geometry")


# Check if they have the same stand id
length(unique(df.no$id))
length(unique(df.geom$standid))

# keep  only the overlapping standid
shared.stands = intersect(unique(df.no$id), unique(df.geom$standid))



# Calculate thinning values
# ---------------------------------------------
# check values in CCF
unique(df.no$THIN)
unique(df.no$regime)
