

#source("identifyOpenEdge_clean.R")

getwd()


# 
rm(list = ls())



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



# ----------------------------------
# start the stcprit: using rgdal library
# ----------------------------------



library(ggplot2)
library(dplyr)
library(tidyr)
library(rgdal)
library(ggpubr)
library(sf)
library(rgdal)
library(ggspatial)


# Set working directory
setwd("U:/projects/2019_windthrowModel/Janita/outSimulated")

# read simulated data
df <- read.csv("rsl_without_MV_Korsnas.csv", sep = ";")  # without == climate change is not included

# Read stand geometry
#stands.all = read_sf("MV_Korsnas.shp")
stands.all = readOGR(dsn = getwd(),
                     layer = "MV_Korsnas")
   
# Get the standid of unique stands - no all stands were simulated
stands.simul <- unique(df$id)

# Subset stand geometry:
stands.sub <- subset(stands.all, standid %in% stands.simul )

# convert the stand id "integer" to the 'character'
df$id <- as.character(df$id)


# Subset by one regime, to create clean attribute table
df.bau<-
  df %>% 
  filter(regime == "BAU") %>% 
  mutate(regime = factor(regime))        # drop unused factors


# Is there the same number of simulated stands and stands with geometry??
setdiff(unique(stands.sub$standid), unique(df.bau$id))

# Need to have teh same ids in stand geometry and in simulated data
length(unique(stands.sub$standid))
length(unique(df.bau$id))

# Subset just stand geometry that has simulated data on 
stands.bau.geom <- subset(stands.all, standid %in% unique(df.bau$id))

length(unique(stands.bau.geom$standid))
length(unique(df.bau$id))


setdiff(unique(stands.bau.geom$standid), 
        unique(df.bau$id))





# JOin the simulated data with the stand geometry
# Join the geometry table with simulated data




stand.merged <- sp::merge(stands.bau.geom,
                          df.bau, 
                          duplicateGeoms = TRUE,
                          by.x = "standid", by.y = "id")


# how to filter the data in SPDF format???



# Try to calculate open_edge index for one year
stand.merged.2016 <- 
  stand.merged %>% 
  filter(year == 2016) %>% 
  mutate(H_dom = replace_na(H_dom, 0))



head(stand.merged.2016)


# Convert spdf to sf to make animation
out_sf<-st_as_sf(stand.merged) #


out_sf <- out_sf %>% 
  filter(!is.na(year)) %>% 
  mutate(H_dom = replace_na(H_dom, 0))


# Get data:
#library(gapminder)

# Charge libraries:
library(ggplot2)
library(gganimate)
library(transformr)


# My data:

ggplot(out_sf) + 
  geom_sf(aes(fill = H_dom)) +
  scale_fill_continuous(low = "lightgreen", 
                        high = "darkgreen",
                        space = "Lab", 
                        na.value = "red", guide = "colourbar")+
  
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme_bw() +
  xlab("Longitude") + 
  ylab("Latitude") +
  # theme(axis.title=element_blank(),
  #      axis.text=element_blank(),
  #     axis.ticks=element_blank()) +
  # gganimate specific bits:
  labs(title = 'KOrsnas BAU Year: {current_frame}') +
  transition_manual(year) +
  #transition_time(year) +
  ease_aes('linear')


# Save at gif:
anim_save("korsnas_BAU.gif")




  