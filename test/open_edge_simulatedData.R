

# 
rm(list = ls())


setwd("C:/MyTemp/myGitLab/windDamage")

source("myFunctions.R")




# ----------------------------------
# start the script: using rgdal library
# ----------------------------------



library(ggplot2)
library(dplyr)
library(tidyr)
library(rgdal)
library(ggpubr)
library(sf)
library(rgdal)
library(ggspatial)


# # Set working directory
setwd("U:/projects/2019_windthrowModel/Janita/outSimulated")
# 
# # read simulated data
df <- read.csv("rsl_without_MV_Korsnas.csv", sep = ";")  # without == climate change is not included
# 
# # Read stand geometry
df.geom = read_sf("MV_Korsnas.shp")
# 
# 
# 
# # Test if the function works: create new H_dom variable, 
# # fill in with pre-defined colours
# # make for multiple landscapes (time)
# # to see if the function works
# 
# # clean up unnecessary columns
out<- subset(df.geom, select = c("standid"))
st_geometry(out) <- NULL
# 
sf1<-out
sf2<-out
sf3<-out
# 
# # Create new landscapes with different tree heights 
sf1$H_dom <- rep(20, nrow(sf1))
# 
# # CReate another landscape
sf2$H_dom <- rep(c(20, 30), each = nrow(df.geom)/2)
sf3$H_dom <- rep(c(20, 30), nrow(df.geom)/2)

# list together time saries of landscape
sf.ls <- list(sf1, sf2, sf3)




# identify neighbors
nbrs <-find_nbrs_geom(sf = df.geom,
                      id = standid)


# apply neighbors list of each stand to find if have open_edge or not
nbrs_open <- open_edge_by_nbrs(nbrs, df.sim = sf1)


# Apply over multiple landscape:
open_edge_time<- lapply(sf.ls, function(df) open_edge_by_nbrs(nbrs, df))


# convert back to geometry to see the oouptuts

df.geom1 <- df.geom %>% 
  left_join(open_edge_time[[1]])

df.geom2 <- df.geom %>% 
  left_join(open_edge_time[[2]])

df.geom3 <- df.geom %>% 
  left_join(open_edge_time[[3]])


plot(df.geom1["open_edge"])
plot(df.geom2["open_edge"])
plot(df.geom3["open_edge"])




# Seems working!! 


# test on real simulated data: df

# MAke a list by year & regime
df.bau <- subset(df, regime == "BAU") %>% 
  mutate(H_dom = replace_na(H_dom, 0.01))


df.ls <- split(df.bau, df.bau$year)

df.bau.geom <- subset(df.geom, standid %in% unique(df.bau$id))

# Adjust geometry
# column names

# identify neighbors
nbrs.bau <-find_nbrs_geom(sf = df.bau.geom,
                          id = standid)


lapply(nbrs.bau, nrow)

nbrs = nbrs.bau
df.sim = df.ls[[1]]



open_edge_time<- lapply(df.ls, function(df) open_edge_by_nbrs(nbrs.bau, df))

lapply(open_edge_time, function(df) table(df$open_edge))


# seems that it is working on real simulated data and larger geometry





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

# Replace H_dom missing values by 0
stand.merged <-
  stand.merged %>% 
  mutate(H_dom = replace_na(H_dom, 0))


# Calculate open_edge index for one year
stand.merged.2016 <- 
  stand.merged %>% 
  filter(year == 2016) 
 


stand.merged.2096 <- 
  stand.merged %>% 
  filter(year == 2096) 





# Seems that the function is ok, vhy the open_edge does not change over time? need to run it one by one??


par(mfrow = c(1,2) )
plot(stand.merged.2016["H_dom"], main = "H_2016", key.pos = NULL, reset = FALSE)
plot(stand.merged.2096["H_dom"], main = "H_2096", key.pos = NULL, reset = FALSE)


# ----------------------------
# Calculate the open_edge for the 2016
# ----------------------------


stand.merged.2016.edge <- findOpenEdge_sf(stand.merged.2016, H_dom, 40, 16)

stand.merged.2096.edge <- findOpenEdge_sf(stand.merged.2096, H_dom, 40, 16)


#par(mfrow = c(1,2) )
plot(stand.merged.2016.edge["open_edge"], main = "H_2016", key.pos = NULL, reset = FALSE)
plot(stand.merged.2096.edge["open_edge"], main = "H_2096", key.pos = NULL, reset = FALSE)






library(gridExtra)


p1<- ggplot(stand.merged.2016.edge) +
  geom_sf(aes(fill = open_edge))


p2<- ggplot(stand.merged.2111.edge) +
  geom_sf(aes(fill = open_edge))



grid.arrange(p1, p2)

# Get the function to work !!! by individual shapes
# retunr same outputs of open edge!!!





#
# Split dataframe into multiple dataframe list 
# Excecute the function on each of dataframe
# need to get out objects of sf an dataframe


# split dataframe into list of dataframes
out <- split(stand.merged, f = stand.merged$year)

# Apply a function over a dataframe list

out.edge <- lapply(out, findOpenEdge_sf)

# How to convert list of df to df while keeping geometry???
out.edge.df<- sf::st_as_sf(data.table::rbindlist(out.edge))



# --------------------------------
#       Make a gif
# -------------------------------


# Charge libraries:
library(ggplot2)
library(gganimate)
library(transformr)


# My data:


ggplot(stand.merged) + #  
  geom_sf(aes(fill = H_dom)) + # H_dom
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
  labs(title = 'Korsnas BAU Year: {current_frame}') +
  transition_manual(year) +
  #transition_time(year) +
  ease_aes('linear')






ggplot(out.edge.df) + # stand.merged 
  geom_sf(aes(fill = open_edge)) + # H_dom
  #scale_fill_continuous(low = "lightgreen", 
   #                     high = "darkgreen",
    #                    space = "Lab", 
     #                   na.value = "red", guide = "colourbar")+
  
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
  labs(title = 'Korsnas BAU Year: {current_frame}') +
  transition_manual(year) +
  #transition_time(year) +
  ease_aes('linear')


# Save at gif:
anim_save("korsnas_BAU_open_edge.gif")




  