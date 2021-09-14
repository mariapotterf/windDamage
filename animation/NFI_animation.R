
# -------------------------
# Get XY animation
# -------------------------
rm(list.ls())


# changing regimes over finland:
# get NFI geometry
# link datatable for one regime over years
# color the code

library(ggplot2)
require(sf)
library(ggspatial)
library(data.table)
library(dplyr)


# Read input data: -------------------
# XY points
# Finland shape
# simulated data


df.geom <- sf::st_read("C:/MyTemp/2021_WindRisk_biodiversity/output.gdb", 
                       layer = "XY_attr")

# Get polygon of teh Finland
df.FI <- sf::st_read("C:/MyTemp/2021_WindRisk_biodiversity/raw", 
                       layer = "FIN_adm0_3067")


# Get simulated data
inPath = 'C:/MyTemp/2021_WindRisk_biodiversity/output/windRisk_csv'
df.sim <- data.table::fread(paste(inPath, "rcp0BAU.csv",  sep = "/"),  # 
                            data.table=TRUE, 
                            stringsAsFactors = FALSE,
                            integer64="character")



# JOin geometry with simulated data: only one regime   ---------------------------------
df.all<-
  df.geom %>% 
  dplyr::select(-c(FID_XY_3067, X, Stand, Padded_id_1, FID_MetsaKasvVyoh)) %>% 
  dplyr::rename(X = X_10,
                Y = Padded_id) %>% 
  mutate(ID = as.character(ID)) %>% 
  left_join(df.sim, by = c("ID" = "id")) 


# Sample the specific IDs to speed up the plotting -------------------
my_ids  <- unique(df.all$ID)
sub_ids <- sample(my_ids, 5000) 


# subset teh same stands from each regime
df.all.sub <- df.all %>% 
                   filter(ID %in% sub_ids) %>% 
  filter(!is.na(windRisk))


# plot data -------------------------------------------------
library(viridis)
library(ggthemes)

theme_set(theme_bw())


# Plot map ggplot2 ------------------------------------------------


windows()
df.all.sub %>% 
  filter(year == 2021) %>% 
  ggplot() + 
  geom_sf(aes(color = H_dom))  + # , size = 0.5 , size = AREA
  scale_color_continuous(low = "lightgreen", 
                         high = "darkgreen",
                         space = "Lab", 
                         na.value = "red", guide = "colourbar")#+

# Plot quantiles
ggplot(df.FI) + 
  geom_sf(color = 'black', 
          fill  = 'grey93') + 
  geom_point(data = filter(df.all.sub, year == 2111),
             aes(x = X,
                 y = Y, 
                 color = windRisk_cl), 
             size = 1)  +  # risk_cat
  viridis::scale_color_viridis(discrete = TRUE, option = "magma") +
  ggtitle("Wind damage risk [2016]") +
  ylab('') +
  xlab('') +
  theme_map()


# on continuous data
ggplot(df.FI) + 
  geom_sf(color = 'black', 
          fill  = 'grey93') + 
  geom_point(data = filter(df.all.sub, year == 2111),
             aes(x = X,
                 y = Y, 
                 color = windRisk), 
             size = 1)  +  # risk_cat
  viridis::scale_color_viridis(discrete = FALSE, option = "magma") +
  ggtitle("Wind damage risk [2016]") +
  ylab('') +
  xlab('') +
  theme_map()




# R animate: ------------------------------------------------


# Get data:
library(gapminder)

# Charge libraries:
library(ggplot2)
library(gganimate)
library(transformr)

ggplot(df.FI) +   # base map
  geom_sf(color = 'black', 
          fill  = 'grey93') + 
  geom_sf(data = df.all.sub,
          aes(color = H_dom)) + # , size = Age, size = 0.8size by factor itself!
  viridis::scale_color_viridis(discrete = FALSE, option = "viridis",
                               na.value = "red") +
  annotation_scale(location = "bl", width_hint = 0.4) +
  theme_bw() +
  xlab("Longitude") + 
  ylab("Latitude") +
  # gganimate specific bits: ------------------
labs(title = 'Changes in Tree Height over years {current_frame}',
     color  = "Tree height [m]") +
  transition_manual(year) +
  #transition_time(year) +
  ease_aes('linear')


anim_save("NFI_treeHeight.gif")
# zac. 15:42~ cca 15 min to make one!








# Do not run: 


# Make classification using quantiles
# https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/#discrete-classes-with-quantile-scale

no_classes <- 10
labels <- c()

quantiles <- quantile(df.all.sub$windRisk*100, na.rm = T, 
                      probs = seq(0, 1, length.out = no_classes + 1))

# here I define custom labels (the default ones would be ugly)
labels <- c()
for(idx in 1:length(quantiles)){
  labels <- c(labels, paste0(round(quantiles[idx], 2), 
                             " – ", 
                             round(quantiles[idx + 1], 2)))
}


# I need to remove the last label 
# because that would be something like "66.62 - NA"
labels <- labels[1:length(labels)-1]

# Create a classified variable
df.all.sub$windRisk_cl <- cut(df.all.sub$windRisk, 
                              breaks = quantiles, 
                                     labels = labels, 
                                     include.lowest = T)



