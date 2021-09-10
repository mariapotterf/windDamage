
# -------------------------
# Get XY animation
# -------------------------
rm(list.ls())

# Read simulated data with calculated differences in values
df <- data.table::fread(paste(inPath, 'output/plotting','id_1000.csv', sep = "/"),  #
                                                data.table=TRUE, 
                        integer64="character")


# Categorize the change values:
library(dplyr)
df <- df %>% 
  mutate(risk_cat= cut(perc_change_risk,
                       breaks=c(seq(-100, 100, 20))))


# changing regimes over finland:
# get NFI geometry
# link datatable for one regime over years
# color the code

require(sf)
df.geom <- sf::st_read("C:/MyTemp/2021_WindRisk_biodiversity/output.gdb", 
                       layer = "XY_attr")

# Get polygon of teh Finland
df.FI <- sf::st_read("C:/MyTemp/2021_WindRisk_biodiversity/raw", 
                       layer = "FIN_adm0_3067")


# Filter the NFI point data to the number of points in simulated data
geom_sub <- df.geom %>% 
  filter(ID %in% sub_ids) %>% 
  dplyr::select(ID, Nimi, 
                X_10,Padded_id) %>% 
  dplyr::rename(X = X_10,
                Y = Padded_id)




# Make a function to extract data for regimes and plot
# them on the map

# Add simulated data to point geom -----------------
# first need to subset to have one scenario per map
# subset the CC and adaptation

my_regimes <- c('short_30')  # c("ext_30")

# Needs update!! now works if specied infivifually, but not in a loop!
makeData_forMap <- function(regime_name, df, ...) {
  
#
    # Extract specific alternative or regime
  df_sub <-   df %>% 
      filter(climChange == 'no' & regime == regime_name) # "short_30"

 
    # Join the geometry with simulated data
  df_join<-
    geom_sub %>%
    mutate(ID = as.character(ID)) %>%
    left_join(df_sub, by = c("ID" = 'id'))

  return(df_join)
  
}

d1<- makeData_forMap(my_regimes, df)

# !!! does not work in loop!
#d2 <- lapply(my_regimes, makeData_forMap(my_regimes, df))

# For now just make it manually -------------------------------

# Filter the simulated data
df_00_short <-   df %>% 
  filter(climChange == 'no' & regime == "short_30") # 

df_00_ext <-   df %>% 
  filter(climChange == 'no' & regime == "ext_30")

# Filter the simulated data
df_00_CC <-   df %>% 
  filter(climChange == 'no' & regime == "CCF") # 



# Join the geometry with simulated data
# SHORT
df_join_short<-
  geom_sub %>% 
  mutate(ID = as.character(ID)) %>% 
  left_join(df_00_short, by = c("ID" = 'id')) 


# EXT
df_join_ext<-
  geom_sub %>% 
  mutate(ID = as.character(ID)) %>% 
  left_join(df_00_ext, by = c("ID" = 'id')) 



# CCF
df_join_CCF<-
  geom_sub %>% 
  mutate(ID = as.character(ID)) %>% 
  left_join(df_00_CC, by = c("ID" = 'id')) 




# Make example plot
library(ggplot2)
library(ggpubr)
library(ggthemes)



# Shorten 
p.short <- 
  ggplot(df.FI) + 
  geom_sf(color = 'black', 
          fill  = 'grey93') + 
  geom_point(data = df_join_short, 
             aes(x = X,
                 y = Y, 
                 color = perc_change_risk  ), size = 1)  +  # risk_cat
  scale_colour_gradient2(
    limits = c(-100, 100),
    low = "#a50026",
    mid = NA,
    high = "#313695",
    midpoint = 0,
    breaks = breaks
  ) +
  ggtitle("Short BAU (30%)") +
  ylab('') +
  xlab('') +
  theme_map()



p.ext<- 
  ggplot(df.FI) + 
  geom_sf(color = 'black', 
          fill  = 'grey93') + 
  geom_point(data = df_join_ext, 
             aes(x = X,
                 y = Y, 
                 color = perc_change_risk  ), size = 1)  +  # risk_cat
  scale_colour_gradient2(
    limits = c(-100, 100),
    low = "#a50026",
    mid = NA,
    high = "#313695",
    midpoint = 0,
    breaks = breaks
  ) +
  ggtitle("Extend BAU (30%)") +
  ylab('') +
  xlab('') +
  theme_map()



# CCF
p.CCF <- 
  ggplot(df.FI) + 
  geom_sf(color = 'black', 
          fill  = 'grey93') + 
  geom_point(data = df_join_CCF, 
             aes(x = X,
                 y = Y, 
                 color = perc_change_risk  ), size = 1)  +  # risk_cat
  scale_colour_gradient2(
    limits = c(-100, 100),
    low = "#a50026",
    mid = NA,
    high = "#313695",
    midpoint = 0,
    breaks = breaks
  ) +
  ggtitle("CCF") +
  ylab('') +
  xlab('') +
  theme_map()





#library(gridExtra)
#grid.arrange(p.short, p.ext, ncol = 2)


ggarrange(p.short, p.ext, p.CCF, ncol = 3, common.legend = TRUE)




# -------------------------------------

 




# plot with ggplot; ggplot is quite slow
library(ggplot2)
ggplot(df.FI) + 
  geom_point(data = df.geom, aes(x = X_10,
                                 y = Padded_id, color = PaajakoNro)) +
  geom_sf(color = "black", fill = NA, size = 1.2) 





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


