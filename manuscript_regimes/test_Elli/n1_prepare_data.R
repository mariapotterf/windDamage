
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

# Read corrected simulated names:
df.no <- data.table::fread("C:/MyTemp/myGitLab/windDamage/manuscript_regimes/input_CC/without_MV_Korsnas_rsu.csv", 
                       data.table=FALSE, 
                       stringsAsFactors = FALSE)
df.cc45 <- data.table::fread("C:/MyTemp/myGitLab/windDamage/manuscript_regimes/input_CC/CC45_MV_Korsnas_rsu.csv", 
                           data.table=FALSE, 
                           stringsAsFactors = FALSE)
df.cc85 <- data.table::fread("C:/MyTemp/myGitLab/windDamage/manuscript_regimes/input_CC/CC85_MV_Korsnas_rsu.csv", 
                             data.table=FALSE, 
                             stringsAsFactors = FALSE)


# Stands geometry
df.geom <- st_read(paste(getwd(), "manuscript_regimes/input_shp/Korsnas.shp", sep = "/"))
#df.geom <- subset(df.geom, select = c("KUVIO_ID"))
#names(df.geom) <- c("id", "geometry")

# Explore data --------------------


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


# Make a check: select one stand and one regime with thinning
df.no %>% 
  filter(regime == "BAUwT") %>% 
  distinct(id)

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

df.no_2 <- classifyTHIN(df.no)
df.cc45_2 <- classifyTHIN(df.no)
df.cc85_2 <- classifyTHIN(df.no)


# Get table with thinnings
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
# calculate wind risk for individual regimes for 3 CC 
# -----------------------------------------------

# function to adjust table
formatWindRisk <- function(df, ...) {
  
  library(dplyr)
  
  # add new column
  df$open_edge = "TRUE"
  
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
    mutate_if(is.character, as.factor)    # convert all characters to factor
  
  
  # try if I can calculate the preidtcion based on subset data
  # to make sure that data did not get randomly reorganized
  df.all <-
    df.all %>% 
    dplyr::rename(time_thinning   = since_thin,             # new.name = old.name
                  soilDepthLess30 = soil_depth_less30,
                  siteFertility   = SC.v,
                  tempSum         = avgTemp)
  
  
  
  # Get vector of columns names to keep for statistics
#  glm.colnames <- c("species", 
 #                   "H_dom", 
  #                  "time_thinning", 
   #                 "windSpeed", 
    #                "open_edge",
     #               "soilType",
      #              "soilDepthLess30", 
       #             "siteFertility",   # siteFertility
        #            "tempSum")
  
  
  
  
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



