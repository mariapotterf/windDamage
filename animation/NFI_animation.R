
# -------------------------
# Get XY animation
# -------------------------
rm(list.ls())


# changing regimes over finland:
# get NFI geometry
# link datatable for one regime over years
# color the code

require(sf)
df.geom <- sf::st_read("C:/MyTemp/2021_WindRisk_biodiversity/output", 
                       layer = "XY_UTM_35")


# plot with ggplot; ggplot is quite slow
library(ggplot2)
ggplot(df.geom) + geom_sf(aes(fill = Luokka))


# subset one regimes for a nice attribute table

df.bau<-
  df.no2 %>% 
  filter(regime == "BAU") %>% 
  mutate(regime = factor(regime))  #%>%       # drop unused factors

library(dplyr)
df.bau <- df.bau %>% 
  mutate(ID = as.character(id)) %>% 
  dplyr::select(id)

# remove the large file
rm(df.no)
rm(df.no2)

# JOin geometry with simulated data: only one regime
df.all<-
  df.geom %>% 
  mutate(ID = as.character(ID)) %>% 
  left_join(df.bau, by = c("ID")) 



# plot data
library(ggspatial)
theme_set(theme_bw())


windows()
df.all %>% 
  filter(year == 2021) %>% 
  ggplot() + 
  geom_sf(aes(color = H_dom))  + # , size = 0.5 , size = AREA
  scale_color_continuous(low = "lightgreen", 
                         high = "darkgreen",
                         space = "Lab", 
                         na.value = "red", guide = "colourbar")#+





# R animate: 


# Get data:
library(gapminder)

# Charge libraries:
library(ggplot2)
library(gganimate)
library(transformr)


ggplot(df.all) + 
  geom_sf(aes(color = H_dom)) + # , size = Age, size = 0.8size by factor itself!
  scale_color_continuous(#low = "lightgreen", 
                         #high = "black",
                         #space = "Lab", 
                         #na.value = "red", 
                         #guide = "colourbar"
                         )+
  
  annotation_scale(location = "bl", width_hint = 0.4) +
  # annotation_north_arrow(location = "bl", which_north = "true", 
  #                       pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                      style = north_arrow_fancy_orienteering) +
  theme_bw() +
  xlab("Longitude") + 
  ylab("Latitude") +
  # theme(axis.title=element_blank(),
  #      axis.text=element_blank(),
  #     axis.ticks=element_blank()) +
  # gganimate specific bits:
  labs(title = 'Tree Age BAU Year: {current_frame}') +
  transition_manual(year) +
  #transition_time(year) +
  ease_aes('linear')


anim_save("NFI_Age.gif")
# zac. 15:42~ cca 15 min to make one!


