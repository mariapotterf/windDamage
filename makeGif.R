
# ==========================================================
# Create gif
# how does the tree height changes??? where are windthrows??

# read the SIMO output .csv data
# select one management over landscape
# plot tree height over shapefile
# identify where is and where is not forest cover???

# ======================================================

rm(list = ls())


library(ggplot2)
library(dplyr)
library(sf)
library(rgdal)
library("ggspatial")


# Set working directory
setwd("U:/projects/2019_windthrowModel/Janita/outSimulated")


# Read input data: no wind
df <- read.csv( "rsl_without_MV_Pori.csv", sep = ";") 

# Read geometry
#stands.all = readOGR(dsn = getwd(),
#                   layer = "MV_Pori")

stands.all = read_sf("MV_Pori.shp")


# Get the standid of uqinue stands:
# subset the shapefiles - only 10 stands
stands.simul <- unique(df$id)

# Subset stand geometry:
stands.sub <- subset(stands.all, standid %in% stands.simul )



# Plot attribute information:
windows()
ggplot(stands.sub) + geom_sf(aes(fill = soiltyp))


# JOin the stand geometry with the SIMO output data
# subset single management regime
# plot continuous tree height
# forest management regimes etc.

dim(df)

length(unique(df$regime))



# convert the stand id "integer" to the 'character'
df$id <- as.character(df$id)


# Subset by one regime, to create clen attribute table
df.bau<-
  df %>% 
  filter(regime == "BAU") %>% 
  mutate(regime = factor(regime))        # drop unused factors


# three stands are missing between simulated data and geometry data
length(unique(df.bau$id))
length(unique(stands.sub$standid))


setdiff(unique(stands.sub$standid), unique(df.bau$id))

# Identify which stands are missing???
stands.notSimulated <- subset(stands.all, standid %in% setdiff(unique(stands.sub$standid), unique(df.bau$id)))





# Check if I have regime at each year
table(df.bau$year, df.bau$regime)

# Each regime has one stand once a year
table(df.bau$year, df.bau$id)


# Count number of unique stands:
length(unique(df.bau$id))


# Inspect teh tree height values
unique(subset(df.bau, year == 2016)$H_dom)


# Join the geometry table with simulated data
stand.all<-
  stands.sub %>% 
  left_join(df.bau, by = c("standid" = "id")) %>% 
  filter(!is.na(year)) 




# Plot tree height year by year, for one selected regime
windows()
ggplot(subset(stand.all, year == 2016)) + 
  geom_sf(aes(fill = H_dom)) +
  scale_fill_continuous(low = "lightgreen", 
                        high = "darkgreen",
                        space = "Lab", 
                        na.value = "grey", guide = "colourbar")+ 
 
  #geom_sf(data = stands.notSimulated, fill = "red") +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  xlab("Longitude") + 
  ylab("Latitude") +
  ggtitle("Tree heights Pori", subtitle = "(2016)") +
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                        size = 0.5), panel.background = element_rect(fill = "aliceblue"))


windows()
ggplot(stand.all) + 
  geom_sf(aes(fill = H_dom)) +
  scale_fill_continuous(low = "lightgreen", 
                        high = "darkgreen",
                        space = "Lab", 
                        na.value = "grey", guide = "colourbar")+
  #geom_sf(data = stands.notSimulated, fill = "red") +
  facet_wrap(~ year, nrow = 4, ncol = 5) +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())



# Example in R animate
# https://www.r-graph-gallery.com/271-ggplot2-animated-gif-chart-with-gganimate.html


# Get data:
library(gapminder)

# Charge libraries:
library(ggplot2)
library(gganimate)
library(transformr)


# My data:

ggplot(stand.all) + 
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
  labs(title = 'Pori BAU Year: {current_frame}') +
  transition_manual(year) +
  #transition_time(year) +
  ease_aes('linear')


# Save at gif:
anim_save("pori_BAU.gif")



ggplot(stand.all) + 
  geom_sf(aes(fill = BA)) +
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
  labs(title = 'Pori BAU Year: {current_frame}') +
  transition_manual(year) +
  #transition_time(year) +
  ease_aes('linear')




# Check why do I have NA values in H_dom???
# because my Harvested_V has increased: 
# can I replace all NA by ??













# https://stackoverflow.com/questions/1298100/creating-a-movie-from-a-series-of-plots-in-r

library(fields) # for tim.colors
library(caTools) # for write.gif
m = 400 # grid size
C = complex( real=rep(seq(-1.8,0.6, length.out=m), each=m ), imag=rep(seq(-1.2,1.2, length.out=m), m ) )
C = matrix(C,m,m)

Z = 0
X = array(0, c(m,m,20))
for (k in 1:20) {
  Z = Z^2+C
  X[,,k] = exp(-abs(Z))
}

image(X[,,k], col=tim.colors(256)) # show final image in R
write.gif(X, 'Mandelbrot.gif', col=tim.colors(256), delay=100)




# Another working example:

# Make a ggplot, but add frame=year: one image per year
ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, color = continent)) +
  geom_point() +
  scale_x_log10() +
  theme_bw() +
  # gganimate specific bits:
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')

# Save at gif:
anim_save("271-ggplot2-animated-gif-chart-with-gganimate1.gif")























##################################################
## London Data Maps Visualisation               ##
## Author: Mark Bulling                         ##
## Date: 2011-06-01                             ##
## Comments: code to generate maps of London    ##
##################################################

library(ggplot2)
library(maps)
library(sp)

gpclibPermit()

#### Read in shapefiles for UK LA's from GADM
con <- url("http://www.gadm.org/data/rda/GBR_adm2.RData")
print(load(con))
close(con)

#### Fortify this using ggplot2 features - this turns the shapefiles into a dataframe
ukmap <- fortify.SpatialPolygonsDataFrame(gadm, region="NAME_2")
ukmap$order <- rownames(ukmap) ### Order is important when drawing the map

#### Read in the immigration data from London datastore
url <- "http://data.london.gov.uk/datafiles/demographics/nino-registrations-overseas-borough-2009.csv"

london.data <- read.csv(url)

### Melt the data to turn it into something that can be cast
london.data.melt <- melt(x, id=c("Code", "Area", "Total.Number.of.Registrations", "MYE.Working.age.2008"))
london.data.cast <- cast(london.data.melt, variable~., fun.aggregate=sum)
colnames(london.data.cast) <- c("Area", "total")

london.data.cast <- london.data.cast[order(london.data.cast$total, decreasing=TRUE), ]
london.data.cast$rank <- as.numeric(rank(london.data.cast$total))

### Drop the data that is extraneous (not elegant)
london.data.melt <- merge(london.data.melt, london.data.cast, by.x="variable", by.y="Area")
london.data.melt <- subset(london.data.melt, as.numeric(rank)>=185)

### Merge on immigration data to the uk la map dataframe
ukmap.data <- merge(ukmap, london.data.melt, by.x="id", by.y="Area")
ukmap.data <- ukmap.data[order(ukmap.data$order), ]

### Drop other extraneous data
ukmap.data.sub <- subset(ukmap.data, value>0)
ukmap.data.sub <- subset(ukmap.data.sub, group!="London.1")

### Merging the data above messes up the order of the points, so reorder to 
ukmap.data.sub$order <- as.numeric(ukmap.data.sub$order)
ukmap.data.sub <- ukmap.data.sub[order(ukmap.data.sub$rank, ukmap.data.sub$group, ukmap.data.sub$order), ]

#### Plot the map - facetted by country of origin
ggplot(data = ukmap.data.sub, aes(x = long, y = lat, fill = cut_interval(log(value), n = 9), grouping = group))+
  geom_polygon()+
  theme_bw()+
  geom_path(colour="grey")+
  xlim(c(-0.6, 0.4))+
  ylim(c(51.25, 51.7))+
  facet_wrap(~variable)+
  scale_fill_brewer(palette = "PuRd", label = "Number of registered immigrants")+
  xlab("")+
  ylab("")
