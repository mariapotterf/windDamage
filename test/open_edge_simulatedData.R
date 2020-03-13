

# 
rm(list = ls())


setwd("C:/MyTemp/myGitLab/windDamage")

source("myFunctions.R")




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
df.geom = read_sf("MV_Korsnas.shp")
#df.geom = readOGR(dsn = getwd(),
 #                    layer = "MV_Korsnas")


# Investigate the fit between simulated data and their geometry:
# keep only simulated stands and their corresponding geometry
# ----------------------------------------

extra.stand.geom <- setdiff(unique(df.geom$standid), unique(df$id))
extra.stand.simul <- setdiff(unique(df$id), unique(df.geom$standid))


# Get the standid of unique simulated stands - no all stands were simulated
stands.simul <- unique(df$id)

# Subset stand geometry:
stand.geom.sub <- subset(df.geom, standid %in% stands.simul )

# convert the stand id "integer" to the 'character'
df$id <- as.character(df$id)


# Subset by one regime, to create clean attribute table
# here also the geometry can differ???
df.bau<-
  df %>% 
  filter(regime == "BAU") %>% 
  mutate(regime = factor(regime))        # drop unused factors


# Is there the same number of simulated stands and stands with geometry??
setdiff(unique(stand.geom.sub$standid), unique(df.bau$id))

# Need to have teh same ids in stand geometry and in simulated data
length(unique(stand.geom.sub$standid))
length(unique(df.bau$id))

# Subset just stand geometry that has simulated data on 
stands.bau.geom <- subset(df.geom, standid %in% unique(df.bau$id))

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




# Try to calculate open_edge index for one year
stand.merged.2016 <- 
  stand.merged %>% 
  filter(year == 2016) %>% 
  mutate(H_dom = replace_na(H_dom, 0))



head(stand.merged.2016)



# ----------------------------
# Calculate the open_edge for the 2016
# ----------------------------


stand.merged.2016.edge <- findOpenEdge_sf(stand.merged.2016, H_dom, 40, 16)


ggplot(stand.merged.2016.edge) +
  geom_sf(aes(fill = open_edge))



#
# Split dataframe into multiple dataframe list 
# Excecute the function on each of dataframe

outEdge<-
  stand.merged %>% 
  group_by(year) %>% 
  group_split() %>% 
  findOpenEdge_sf(.)
  



# split dataframe into list of dataframes
out <- split(stand.merged, f = stand.merged$year)

# Apply a function over a dataframe list

out.edge <- lapply(out, findOpenEdge_sf)


# Convert back to dataframe
out.edge.df <- dplyr::bind_rows(out.edge)


# !!!
# How to convert list of df to df while keeping geometry???


# --------------------------------
#       Make a git
# -------------------------------


# Charge libraries:
library(ggplot2)
library(gganimate)
library(transformr)


# My data:

ggplot(out.edge.df) + 
  geom_sf(aes(fill = open_edge)) +
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




  