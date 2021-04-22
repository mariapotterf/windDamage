
# ----------------------------------------
# process Ellinoora data for no CC, and climate change included
# ----------------------------------------

# steps:
# Get input data:
#   - get stand geometry from GPKG - already derived from previous project
#   - select one watershed from South and one from North
#   - get simulated regimes: for no change, CC4.5, CC8.5
# Select which regimes to use
#     need the most stands with regimes
#     have the same stand id for geometry and for simulated data
#     if unreal values - replace by mean?
#   - use SA values in 2016 to have the same beginning for data under all regimes

# Calculate wind risk values - prepare data
#   - make landscapes by year
#   - calculate open edge and neighbours - maybe enought with TRE and False values at the beginning
#   - calculate thinning values
#   -


# How to prepare continous landscape??? if there is a missing regime, 
# i can fill in values from the closest regime for the particular stand?



# Make working example for no climate changes; then calculate values for CC scenario 
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
inPath = "C:/MyTemp/myGitLab/windDamage/manuscript_regimes"

# Read corrected simulated names:
df.no <- data.table::fread(paste(inPath, "input_CC/without_MV_Korsnas_rsu.csv", sep = "/"),
                       data.table=FALSE, 
                       stringsAsFactors = FALSE)
df.cc45 <- data.table::fread(paste(inPath, "input_CC/CC45_MV_Korsnas_rsu.csv", sep = "/"),
                           data.table=FALSE, 
                           stringsAsFactors = FALSE)
df.cc85 <- data.table::fread(paste(inPath, "input_CC/CC85_MV_Korsnas_rsu.csv", sep = "/"),
                             data.table=FALSE, 
                             stringsAsFactors = FALSE)


# seems that 5 stands id: 3576410,  3576082, 3576632, 3576696, 3576723 have no values
df.cc85 %>% filter(id == 3576723) #%>% distinct(regime)

# Stands geometry
df.geom <- st_read(paste(inPath, "input_shp/stands/MV_Korsnäs.shp", sep = "/"))
df.geom <- subset(df.geom, select = c("standid"))
#names(df.geom) <- c("id", "geometry")


# Get values for wind speed and temps sum
df.raster <-   data.table::fread(paste(inPath, "input_CC/df_raster_Korsnas.csv", sep = "/"),
                                 data.table=FALSE, 
                                 stringsAsFactors = FALSE)

length(unique(df.raster$standid))  # 302 


# Explore data --------------------

# Check if they have the same stand id
length(unique(df.no$id))
#length(unique(df.geom$standid))

# keep  only the overlapping standid
shared.stands = intersect(unique(df.no$id), unique(df.geom$standid))


# add missing SOIL_CLASS data to no clim change data
df.soil.class <- df.cc45 %>% 
  dplyr::select(id, SOIL_CLASS) %>% 
  distinct()


# Add this to no CC scenario
df.no <- df.no %>% 
  right_join(df.soil.class, by = c('id'))



# Make list of input df (with and without CC) -----------------------------------
df.ls <- list(df.no, df.cc45, df.cc85)

# Add raster values: temp sum and avgWind speed
# indication for raster values
# add temperature and wind speed values to each table by id
df.ls <- lapply(df.ls, function(df) df %>%  right_join(df.raster, 
                                                        by = c('id' = 'standid')))
# Calculate thinning values
# ---------------------------------------------


# Make a check: select one stand and one regime with thinning
#df.no %>% 
 # filter(regime == "BAUwT") %>% 
  #distinct(id)

classifyTHIN <- function(df, ...) {
  library(dplyr)
  df.out <- 
    df %>% 
    mutate(THIN = na_if(THIN, 0))  %>% 
    mutate(THIN2 = substring(THIN,0,4)) %>%  # keep the first 4 characters from CCF regimes, datum in format "2016-04-16" -> to "2016"
    group_by(id, regime) %>% 
    mutate(THIN_filled_lagged = lag(THIN2)) %>%
    mutate(THIN_filled_lagged = as.numeric(THIN_filled_lagged)) %>%
    tidyr::fill(THIN_filled_lagged) %>% 
    mutate(difference = year - THIN_filled_lagged) %>% 
    mutate(since_thin = case_when(is.na(difference) | difference < 0 ~ ">10",
                                  difference %in% c(0:5) ~ "0-5",
                                  difference %in% c(6:10) ~ "6-10",
                                  difference > 10 ~ ">10")) 
  return(df.out)
}


# Classify values
df.ls.thin = lapply(df.ls, classifyTHIN)

# Indicate climate change category
clim.cat = c("no", "cc45", "cc85")
df.ls.thin = mapply(cbind, df.ls.thin, "climChange"=clim.cat, SIMPLIFY=F)



# Get table with thinnings ---------------------
#df.no2<- 
 #df.no %>% 
  # mutate(THIN = na_if(THIN, 0))  %>% 
   #mutate(THIN2 = substring(THIN,0,4)) %>%  # keep the first 4 characters from CCF regimes, datum in format "2016-04-16" -> to "2016"
   # group_by(id, regime) %>% 
   # mutate(THIN_filled_lagged = lag(THIN2)) %>%
   # mutate(THIN_filled_lagged = as.numeric(THIN_filled_lagged)) %>%
   # tidyr::fill(THIN_filled_lagged) %>% 
   # mutate(difference = year - THIN_filled_lagged) %>% 
   # mutate(since_thin = case_when(is.na(difference) | difference < 0 ~ ">10",
   #                               difference %in% c(0:5) ~ "0-5",
   #                               difference %in% c(6:10) ~ "6-10",
   #                               difference > 10 ~ ">10")) # %>% 
   # #filter(id == 3576736 & regime == "BAUwT") %>% 
  # filter(id == 3576736 & regime == "CCF_1") %>% 
#   dplyr::select(id, year, regime, THIN, difference, since_thin)
   # Remove unnecessary columns
 #  dplyr::select(-branching_group, 
  #               -regime.x,
   #              -regime.y)
 


# Inspect data ------------------------------------------------------------


# test if data calculated are equal:
all(df.noTEST == df.no2)
identical(df.noTEST,df.no2)

# Inspect how many stands I have per regime?
df.no2 %>%
  filter(year == 2016) %>% 
  group_by(regime) %>% 
  distinct(id) %>% 
  tally() %>% 
  print(n = 40)
 
# total number of stands:
stand_n = length(unique(df.no$id))


# total n of stands = 299

# how many stands have all regimes???
df.no2 %>%
  filter(year == 2016) %>% 
  group_by(id) %>% 
  distinct(regime) %>% 
  tally() %>% 
  filter(n<15) # n<5
  #print(n = 40)

# about 150 stands have >20 regimes, but how consistent it is?
# how to fill in stand gaps to recreate continoous landscaes?
# 19 stands have less regimes then 5 

# Let's say that I can fill in gaps by the closest regime:
# how many stands do I have for BAU, SA, CCF_1?
# BAU   = 278
# CCF_1 = 264
# SA    = 299

# do all BAU stands have have al CCF regimes??
# subset df.no$regime == "BAU"
df.bau <- df.no %>% 
  filter(regime == "BAU") %>% 
  distinct(id)

df.ccf <- df.no %>% 
  filter(regime == "CCF_1") %>% 
  distinct(id)


# get the vector of ids
bau_id = df.bau$id
ccf_id = df.ccf$id


# check number of CCF regimes per BAU
df.no %>% 
  filter(id %in% bau_id) %>% 
  filter(regime == "CCF_4") %>% 
  distinct(id) %>% 
  tally()
# seems that all CCF_1 are in BAU

# why some stands are not suitable for BAU??
#separate in two groups: BAU, only SA and compare age, V, tree height
# subset only stands that are not in BAU
all_id = unique(df.no$id)

# keep only stands that have just SA regime:
# keep  only the overlapping standid
sa_id_bau = setdiff(all_id, bau_id)
sa_id_ccf = setdiff(all_id, ccf_id)

# https://stackoverflow.com/questions/31573087/difference-between-two-vectors-in-r

# split dataframe in only SA, SA-BAU, SA-CCF 
df.sa.bau<- df.no %>% 
  filter(regime == "SA_DWextract" & year == 2016 & id %in% sa_id_bau)  %>% 
  mutate(type = "onlySA_BAU")

df.sa.ccf<- df.no %>% 
  filter(regime == "SA_DWextract" & year == 2016 & id %in% sa_id_ccf)  %>% 
  mutate(type = "onlySA_CCF")


df.bau<- df.no %>% 
  filter(regime == "BAU" & year == 2016 & id %in% ccf_id) %>% 
  mutate(type = "active_BAU")

df.ccf<- df.no %>% 
  filter(regime == "CCF_1" & year == 2016 & id %in% bau_id) %>% 
  mutate(type = "active_CCF")


df2 <- rbind(df.sa.bau, df.sa.ccf, df.bau, df.ccf)

# make a plot for Age
df2 %>% 
  ggplot(aes(y = Age,
             x = type)) +
  geom_boxplot()


# How much do actively managed stands overlap between BAU and CCF_1?
setdiff(bau_id, ccf_id)
length(bau_id)
length(ccf_id)


# to create a continuous landsacpes, I will fill in values from missing stands 
# from BAU and CCF_1
# remove the stands that were assigned as SA in 2016

# can aso 

# Check how many stands under BAU I have given CC scenario?
df.no_2 %>% 
  filter(regime == "CCF_4") %>% 
  distinct(id) %>% 
  nrow()





# Generate fake open_edge data --------------------------------------------

# calculate wind risk for individual regimes for 3 CC  -----------------------------------------------

# Get vector of columns names to keep for statistics ------------------
glm.colnames <- c("species",
                  "H_dom",
                  "time_thinning",
                  "windSpeed",
                  "open_edge",
                  "soilType",
                  "soilDepthLess30",
                  "siteFertility",   # siteFertility
                  "tempSum")


# function to adjust table ---------------
formatWindRisk <- function(df, ...) {
  
  library(dplyr)
#   df<- df.cc45_2
  # add new column
  df$open_edge = "TRUE"
  #df$avgTemp   = 12.19653  # update this value later
 # df$windSpeed = 10.84851  # update this value later
  
  # Recalssify values
  # # Reclassify values:
  df.all<-
    df %>% 
    mutate(PEAT.v = case_when(PEAT == 0 ~ "mineral soil",
                              PEAT == 1 ~ "peat"))  %>%
    mutate(SC.v = case_when(SC %in% 1:3 ~ "fertile",
                            SC %in% 4:6 ~ "poor")) %>%                 # COMPLETE SOIL CALSS to get mineral coarse/fine??
  
    mutate(soil_depth_less30 = ifelse(SOIL_CLASS == 1, TRUE,FALSE)) %>%
    
    mutate(soilType = case_when(SOIL_CLASS == 0 ~ "organic",
                                SOIL_CLASS %in% 1:4 ~ "mineral coarse",
                                SOIL_CLASS %in% 5:7 ~ "mineral fine")) %>% 
    mutate(species = case_when(MAIN_SP == 1 ~ "pine",
                               MAIN_SP == 2 ~ "spruce",
                               TRUE ~ "other")) %>% 
    mutate(H_dom = replace_na(H_dom, 0.0001)) %>%  # no possible to get log(0) or log(NA)  
    mutate(H_dom = H_dom * 10) %>%        # Susanne values are in dm instead of meters
    mutate_if(is.character, as.factor) #%>%    # convert all characters to factor
     # head()
  
  # try if I can calculate the preidtcion based on subset data
  # to make sure that data did not get randomly reorganized
  df.all <-
    df.all %>% 
    dplyr::rename(time_thinning   = since_thin,             # new.name = old.name
                  soilDepthLess30 = soil_depth_less30,
                  siteFertility   = SC.v,
                  tempSum         = avgTemp)
  
   # -----------------------------------------
  #          Correct the factor levels 
  #          for categoric variables
  # ------------------------------------
  df.all$species          <- factor(df.all$species, 
                                    levels = c("pine", 
                                               "spruce", 
                                               "other"))
  
  df.all$time_thinning    <- factor(df.all$time_thinning, 
                                    levels = c("0-5", 
                                               "6-10", 
                                               ">10"))
  df.all$open_edge        <- factor(df.all$open_edge,
                                    levels = c("FALSE", 
                                               "TRUE"))
  df.all$soilType         <- factor(df.all$soilType,
                                    levels = c("mineral coarse", 
                                               "mineral fine",
                                               "organic"))
  df.all$soilDepthLess30  <- factor(df.all$soilDepthLess30, 
                                    levels = c("FALSE", 
                                               "TRUE"))
  df.all$siteFertility    <- factor(df.all$siteFertility,
                                    levels = c("poor", 
                                               "fertile"))
  df.all$tempSum          <- df.all$tempSum/100   # according to Susane 
  
  return(df.all)
}

# Format the table ------------------------------
df.ls.glm<- lapply(df.ls.thin, formatWindRisk)


# Calculate wind risk ------------------------------

# For temperature sum, I have single value: not variabilit over the landscape: 1299.273

dd<- df.ls.glm[[2]]

dd$windRisk <- predict.glm(windRisk.m,
                               subset(dd, select = glm.colnames),
                               type="response")


# apply the glm formula to calulate wind risk 
df.risk.ls <- lapply(df.ls.glm, function(df) {df$windRisk <- predict.glm(windRisk.m,
                                                           subset(df, select = glm.colnames),
                                                           type="response")
                  return(df)})

# inspect values
lapply(df.risk.ls, function(df) range(df$windRisk, na.rm = T))

# check why there is NA values in my results?
df.risk.ls[[3]] %>% 
  filter(is.na(windRisk)) %>% 
  distinct(id)

# some data are missing for 5 stands; simpy remove the standid that do not have any regime
# from initial data


# merge data into one ----------------------
# Merge optimal data in one files, filter for incorrect stands
df.out <- do.call(rbind, df.risk.ls)


# check risk for regimes
df.out %>% 
  ggplot(aes(x = climChange,
             y= windRisk,
             group = climChange,
             fill = climChange)) +
  geom_boxplot() +
  ylim(0, 0.25) +
  facet_grid(. ~ regime)

# climate change increases wind risk

# classify for type of modification ------------------
# shorten or extent
# get regimes:
unique(df.out$regime)


# Classify the type of regime, type of adjustement (extension or shortening)
# and change in time (how many years)
df.out2 <- 
  df.out %>% 
  mutate(modif = case_when(
    grepl('_m5' , regime) ~ 'shorten',
    grepl('_m20', regime) ~ 'shorten',
    grepl('_5'  , regime) ~ 'extended',
    grepl('_10' , regime) ~ 'extended',
    grepl('_15' , regime) ~ 'extended',
    grepl('_30' , regime) ~ 'extended',
    TRUE~ 'normal')) %>% 
  mutate(change_time = case_when(
    grepl("_15", regime) ~ "15",
    grepl("_5",  regime) ~ "5",
    grepl("_10", regime) ~ "10",
    grepl("_30", regime) ~ "30",
    grepl("_20", regime) ~ "20",
    grepl("_m5", regime) ~ "-5",
    grepl("_m20", regime) ~ "-20",
    TRUE~'0')) %>% 
 # mutate(change_time = replace_na(change_time, 0)) %>% 
  mutate(mainType = case_when(
    grepl("SA", regime) ~ "SA",
    grepl("BAU", regime) ~ "BAU",
    grepl("CCF", regime) ~ "CCF")) %>% 
  mutate(thinning = case_when(
    grepl("wG|wT", regime) ~ "thin_YES",
    grepl("woT", regime) ~ "thin_NO",
    TRUE~'no'))
    
    
head(df.out2)

unique(df.out2$modif)
unique(df.out2$change_time)
unique(df.out2$mainType)
unique(df.out2$thinning)
# HOw does modification of management regime affect wind risk?

# Change order of change time---------------------------------------
df.out2$change_time <-factor(df.out2$change_time, 
                         levels = c("-20", "-15", "-10", "-5", "0",  "5",  "10", "15","20", "25", "30"))


# check risk for regimes-------------------------------------------
df.out2 %>% 
  filter(mainType == "BAU"  ) %>% 
  ggplot(aes(x = modif, #climChange,
             y= windRisk,
             group = modif, #climChange,
             fill = modif)) + #, climChange)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom="point", col = "red") +
 # ylim(0, 0.25) +
  facet_grid(thinning ~ climChange) +
  theme(legend.position = "none") + 
  ggtitle("BAU")


windows()
df.out2 %>% 
  filter(mainType == "CCF") %>% 
  ggplot(aes(x = climChange,
             y= windRisk,
             group = climChange,
             fill = climChange)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom="point", col = "red") +
  ylim(0, 0.25) +
  facet_grid(. ~ modif) +
  theme(legend.position = "none") + 
  ggtitle("CCF")


# check the risk by time delay
# Make plots for change time -----------------------------------------------------------------
p.extended<- 
  df.out2 %>% 
  filter(mainType == "BAU") %>%
  filter(modif == "extended" | modif == "normal" ) %>% 
  ggplot(aes(x = change_time,
             y= windRisk
             #group = climChange,
             )) +
  geom_boxplot(aes(fill = climChange), position=position_dodge(.9)) +
  ylim(0, 0.20) +
  stat_summary(aes(group =climChange ), position=position_dodge(.9), 
               fun = mean, geom="point", col = "red") +

  #facet_grid(climChange ~ year) +
  #theme(legend.position = "none") + 
  ggtitle("a) BAU extended")
  

p.shorten<- 
  df.out2 %>% 
  filter(mainType == "BAU") %>% 
  filter(modif == "shorten" | modif == "normal") %>% 
  ggplot(aes(x = change_time,
             y= windRisk
             #group = climChange,
  )) +
  geom_boxplot(aes(fill = climChange), position=position_dodge(.9)) +
  ylim(0, 0.20) +
  stat_summary(aes(group =climChange ), position=position_dodge(.9), 
               fun = mean, geom="point", col = "red") +
  
  #facet_grid(climChange ~ year) +
  #theme(legend.position = "none") +
  ggtitle("b) BAU shortened")

library(ggpubr)
ggarrange(p.extended, p.shorten, common.legend = TRUE, legend="bottom")



# Plots for Climate change ------------------------------------------------

p.extended<- 
  df.out2 %>% 
  filter(mainType == "BAU") %>% 
  filter(modif == "extended" | modif == "normal") %>% 
  ggplot(aes(x = climChange, 
             y= windRisk
             #group = ,
  )) +
  geom_boxplot(aes(fill = change_time), position=position_dodge(.9)) +
  ylim(0, 0.20) +
  stat_summary(aes(group =change_time ), position=position_dodge(.9), 
               fun = mean, geom="point", col = "red") +
  
  #facet_grid(climChange ~ year) +
  #theme(legend.position = "none") + 
  ggtitle("a) extended")


p.shorten<- 
  df.out2 %>% 
  filter(modif == "shorten" | modif == "normal") %>% 
  ggplot(aes(x = climChange,
             y= windRisk
             #group = ,
  )) +
  geom_boxplot(aes(fill = change_time), position=position_dodge(.9)) +
  ylim(0, 0.20) +
  stat_summary(aes(group =change_time ), position=position_dodge(.9), 
               fun = mean, geom="point", col = "red") +
  
  #facet_grid(climChange ~ year) +
  #theme(legend.position = "none") +
  ggtitle("b) shortened")

library(ggpubr)
ggarrange(p.extended, p.shorten, common.legend = FALSE, legend="bottom")


# What regime is the most sensitive to shortening/extension? -----------------

p.extended<- 
  df.out2 %>% 
  filter(modif == "extended" | modif == "normal") %>% 
  ggplot(aes(x = change_time,
             y= windRisk
             #group = climChange,
  )) +
  geom_boxplot(aes(fill = climChange), position=position_dodge(.9)) +
  ylim(0, 0.20) +
  stat_summary(aes(group =climChange ), position=position_dodge(.9), 
               fun = mean, geom="point", col = "red") +
  
  #facet_grid(climChange ~ year) +
  #theme(legend.position = "none") + 
  ggtitle("a) extended")


p.shorten<- 
  df.out2 %>% 
  filter(modif == "shorten"  & mainType == "BAU") %>% 
  ggplot(aes(x = change_time,
             y= windRisk
             #group = climChange,
  )) +
  geom_boxplot(aes(fill = climChange), position=position_dodge(.9)) +
  ylim(0, 0.20) +
  stat_summary(aes(group =climChange ), position=position_dodge(.9), 
               fun = mean, geom="point", col = "red") +
  facet_grid(.~ regime) +
    ggtitle("b) shortened")

library(ggpubr)
ggarrange(p.extended, p.shorten, common.legend = TRUE, legend="bottom")



# need to compare specific regime with its modification:

# Line plots  -------------------------------------------------------------


df.out2 %>% 
  filter(modif == "shorten"  & mainType == "BAU") %>% 
  ggplot(aes(x = change_time,
             y= windRisk
             #group = climChange,
  )) +
  geom_boxplot(aes(fill = climChange), position=position_dodge(.9)) +
  ylim(0, 0.20) +
  stat_summary(aes(group =climChange ), position=position_dodge(.9), 
               fun = mean, geom="point", col = "red") +
  facet_grid(.~ regime) +
  ggtitle("b) shortened")


# get shaded region  ----------------------------------------------
# Get the line plot, for one regime over years
df.out2 %>% 
  #filter(mainType == "BAU" & modif == "extended" ) %>%  #
  filter(mainType == "BAU") %>% 
  group_by(year, climChange, change_time) %>% 
  summarise(my_y = mean(windRisk, na.rm = T)) %>% 
  ungroup() %>% 
  ggplot(aes(x = year )) +
  #geom_line(aes(y = my_y))
  geom_ribbon(
    data = ~ pivot_wider(., 
                         names_from = climChange, 
                         values_from = my_y),
    aes(ymin = no, 
        ymax = cc85), fill="grey80",alpha=0.4) +
  geom_line(aes(y = my_y,
                #color = "black",     
                linetype = climChange),
            lwd  = 1)  +
  scale_linetype_manual(values=c('solid', 'dotted', 'dashed')) +
  theme_bw()  +
  facet_grid(.~change_time) + 
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 90, vjust = 0.5, face="plain", size = 9, family = "sans"))



df.out2 %>% 
  filter(mainType == "BAU" & modif == "shorten" ) %>%  #
  group_by(year, climChange, change_time) %>% 
  summarise(my_y = mean(windRisk, na.rm = T)) %>% 
  ungroup() %>% 
  ggplot(aes(x = year )) +
  #geom_line(aes(y = my_y))
  geom_ribbon(
    data = ~ pivot_wider(., 
                         names_from = climChange, 
                         values_from = my_y),
    aes(ymin = no, 
        ymax = cc85), fill="grey80",alpha=0.4) +
  geom_line(aes(y = my_y,
                #color = "black",     
                linetype = climChange),
            lwd  = 1)  +
  theme_bw()  +
  facet_grid(.~change_time)



# put together and color by specific groups
df.out2 %>% 
 # filter(mainType == "BAU" & (modif == "shorten"| modif == "normal")) %>%  #
  group_by(year, climChange, change_time) %>% 
  summarise(my_y = mean(windRisk, na.rm = T))  %>% 
 # ungroup() %>% 
  ggplot(aes(x = year,
             y = my_y,
             linetype = climChange,
             color = change_time)) +  #
  geom_line() + 
  facet_grid(.~change_time) + 
  
           
         
#         ,
             #color = "black",     
            # linetype = climChange,
#             group = change_time)) +
#  geom_ribbon(
 #   data = ~ pivot_wider(., 
#                         names_from = climChange, 
 #                        values_from = my_y),
  #  aes(ymin = no, 
   #     ymax = cc85), fill="grey80",alpha=0.4) +
  geom_line(aes(),
            lwd  = 1)  +
  theme_bw() 




# Make a different colur of shade by group

library(ggplot2)
library(tidyr)

# example for shaded line plot
dd1 <- data.frame(year = c(1:5),
                 grp = rep(c("a", "b", "c"), each = 5),
                 vals = c(5, 5.2, 5.6, 5.8, 6,
                          5, 4.9, 4.8, 4.7, 4.2,
                          5, 4.8, 4.4, 4,   3),
                 modif = rep('no', each = 15))

dd2 <- dd1
dd2$vals = dd1$vals*0.8
dd2$modif = 'yes'

# create a new factor


dd <- rbind(dd1, dd2)
dd$comb = paste(dd$modif, dd$grp, sep = "_")



dd %>% 
  ggplot(aes(x = year)) +
  ylim(0,6.5) +
  #geom_ribbon(
  #  data = ~ pivot_wider(., names_from = grp, 
   #                      values_from = vals),
  #  aes(ymin = c, ymax = a, fill = modif)
  #) +
  geom_line(aes(y = vals,
                color = interaction(modif, grp)),
           lwd  = 1.5)  +
  theme_bw()


dd %>% 
  ggplot(aes(x = year)) +
 geom_ribbon(
  data = ~ pivot_wider(., names_from = grp,
                      values_from = vals),
  aes(ymin = c, ymax = a, fill = modif)
 ) +
  ylim(0,6.5) +
   geom_line(aes(y = vals,color = modif, linetype = grp),  # color = interaction(modif, grp)
            lwd  = 1.5)  +
 
  theme_bw() +
  facet_grid(modif~.)

