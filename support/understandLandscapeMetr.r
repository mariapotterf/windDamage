
# -----------------------------------
# Understand landscape metrics
# -----------------------------------

# 
rm(list = ls())

# , eval = FALSE
library(data.table)
library(dplyr)
library(raster)
library(ggplot2)
library(sf)
library(stringr)
library(landscapemetrics)


theme_set(theme_classic())




# Test if the fuinction correctly loops over lit of data
r1 <- raster(nrow=11, 
             ncol=6, 
             crs = "+init=epsg:2957") #   crs(df.geom)) #"+init=epsg:2957"
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


# 


# Plot raster
par(mfrow = c(1,2))
plot(r1)


# ----------------------------------
# Learn landscape metrics:
# https://cran.rstudio.com/web/packages/landscapemetrics/vignettes/getstarted.html
# --------------------------------

# Firts, check the landscape
check_landscape(landscapemetrics::landscape) 

plot(landscape)

# General structure: lsm_'level'_"metric"

# Patch level: _p_
lsm_p_enn(landscape)

# Class level: _c_
lsm_c_enn(landscape)

# Landscape level: _l_
lsm_l_enn(landscapemetrics::landscape)


# Check the landscape
# Landscape could have properties that prevent calculation of some metrics:
# need to check before:
check_landscape(landscapemetrics::landscape) 

check_landscape(landscapemetrics::podlasie_ccilc)

check_landscape(landscapemetrics::augusta_nlcd)


# Requirements to calculate meaning full metrics:
# - distance unit in meters
# rasters  encodes landscape classes as interegs (1,2,3,4,5...)
# landscape metrics describe categorical landscapes  = landscape needs to be classifies 
# (throw a warning if more then 30 classes)

# # Calculate perimeter of all patches:
lsm_p_perim(landscape)

# Using landscape in a tidy workflow:
# ----------------

# all patch IDs of class 2 with an ENN > 2.5
subsample_patches <- landscape %>% 
  lsm_p_enn() %>%
  dplyr::filter(class == 2 & value > 2.5) %>%
  dplyr::pull(id)


# Use multiple metric functions
# Print all available metrics
list_lsm() %>% 
  print(n = 150)
