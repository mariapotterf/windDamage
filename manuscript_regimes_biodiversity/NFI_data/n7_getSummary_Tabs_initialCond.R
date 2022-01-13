
# ----------------------------------
# Make tables for manuscript:
# ----------------------------------


# Read data and make tables and plots
rm(list = ls())

# get the common location:
source('myPath.R') 


setwd(paste(myPath, "r_winddamage", sep = "/"))

# Read libraries
library(dplyr)
library(tidyr)
library(data.table)
library(stringr)
library(ggplot2)

# Correct order of variables, factors & levels
# need to have same names of columns?? YES
# when data are sourced (source()), they are all available in my script
source(paste(myPath, 'r_winddamage', 'myFunctions.R', sep = "/"))




# Set themes ----
theme_set(theme_classic())
theme_update(panel.grid.major = element_line(colour = "grey95",  # background gridlines
                                             size = 0.1,
                                             linetype = 2),
             strip.background = element_rect(color="white",      # headings for facets
                                             fill="white",
                                             size=0.1, 
                                             linetype="solid"),
             panel.background = element_rect(fill = "white",     # individual plots
                                             colour = "grey50",
                                             linetype = "solid"))





# Get data ------------------------------------------------------------------------------
inPath   = myPath
inFolder = "output/plotting"
inName   = 'df_filt.csv'

# Input table
df.out <- data.table::fread(paste(inPath, inFolder, inName,  sep = "/"),  # 
                            data.table=TRUE, 
                            stringsAsFactors = FALSE,
                            integer64="character")


# Make table: initial conditions
(tot_stands = length(unique(df.out$id)))

# Make a summary table
summary_df <- 
  df.out %>% 
  filter(year == 2016 & MAIN_SP != 0 & regime == 'BAU' & climChange == 'REF') %>% 
  mutate(species = case_when(MAIN_SP == 1 ~ "pine",
                             MAIN_SP == 2 ~ "spruce",
                            TRUE ~ "other")) %>%
  dplyr::select(-MAIN_SP) %>% 
  group_by(species) %>% 
  summarise(mean_height = round(mean(H_dom, na.rm = T), digits = 1),
            sd_height   = round(sd(  H_dom, na.rm = T), digits = 1),
            mean_Age    = round(mean(Age, na.rm = T),   digits = 1),
            sd_Age      = round(sd(  Age, na.rm = T),   digits = 1),
            #mean_BA     = round(mean(BA, na.rm = T),    digits = 1),
            #sd_BA       = round(sd(  BA, na.rm = T),    digits = 1),
            mean_V      = round(mean(V, na.rm = T),     digits = 1),
            sd_V        = round(sd(  V, na.rm = T),     digits = 1),
            mean_DW = round(mean(V_total_deadwood , na.rm = T), digits = 1),
            sd_DW   = round(sd(  V_total_deadwood, na.rm = T), digits = 1),
            n           = n(), # count species
            share_n     = round(n/tot_stands*100, digits = 1 )) #%>% 
 


# Format output table
formated_df <- 
  summary_df %>% 
  mutate(Height        = stringr::str_glue("{mean_height}±{sd_height}"),
         Age           = stringr::str_glue("{mean_Age}±{sd_Age}"),
         Volume        = stringr::str_glue("{mean_V}±{sd_V}"),
         Deadwood      = stringr::str_glue("{mean_DW}±{sd_DW}"),
         Species_share = stringr::str_glue("{n}({share_n})")) %>%  #,  {scales::percent(sd_height)}
  #Age    = stringr::str_glue("{scales::percent(share_bball, accuracy = 1)} ({count_bball} / {n})")) %>%
  tidyr::complete(species)  %>%

  dplyr::arrange(desc(Species_share)) %>%   # arrange by the importance NOT WORKING!
  dplyr::select(species, Species_share, Height, Age, Volume, Deadwood) 



# # copy the table form teh R data.frame format, insert as text and then Convert text to table.
length(unique(df.out$id))


# Get HSI indices stats -----------------------------------------

tab_HSI <- df.out  %>% 
  dplyr::select(CAPERCAILLIE, HAZEL_GROUSE,  
                THREE_TOED_WOODPECKER, LESSER_SPOTTED_WOODPECKER,
                LONG_TAILED_TIT, SIBERIAN_FLYING_SQUIRREL)%>% 
  # convert from long to wide
  gather(species, 
         value, CAPERCAILLIE:SIBERIAN_FLYING_SQUIRREL, factor_key=TRUE) %>% 
  group_by(species) %>%  
  summarize(min = min(value, na.rm = T),
            max = max(value, na.rm = T),
            mean = round(mean(value, na.rm = T),3),
            med = median(value, na.rm = T)) 





# Get enviro characteristics --------------------------


# Count the soil conditions
# -------------------------
df.out %>% 
  filter(year == 2016 ) %>% 
  mutate(soilType = case_when(SOIL_CLASS == 0 ~ "organic",
                              SOIL_CLASS %in% 1:4 ~ "mineral coarse",
                              SOIL_CLASS %in% 5:7 ~ "mineral fine")) %>% 
  group_by(soilType) %>% 
  summarise(n           = n(), # count species
            share_n = round(n/tot_stands*100, digits = 1 )) 

# seems that all classes are mineral coarse??
# Get this value instead from teh NFI inventory
#soilType           n share_n
#<chr>          <int>   <dbl>
#  1 mineral coarse 52455     100
#> unique(df.out$SOIL_CLASS)


# Site fertility
# -----------------
df.out %>% 
  dplyr::filter(year == 2016 ) %>% 
  mutate(siteFertility = case_when(SC %in% 1:3 ~ "fertile",
                          SC %in% 4:7 ~ "poor")) %>%                 # added 7? COMPLETE SOIL CALSS to get mineral coarse/fine??
  group_by(siteFertility) %>% 
  summarise(n           = n(), 
            share_n = round(n/tot_stands*100, digits = 1 )) 


# siteFertility     n share_n
# <chr>         <int>   <dbl>
# 1 fertile       33458    63.8
# 2 poor          18997    36.2



# Get wind speed and temperature sum
# ----------------------------
df.out %>% 
  filter(year == 2016 ) %>% 
  # group_by(siteFertility) %>% 
  summarise(mean_wind = round(mean(windSpeed, na.rm = T), digits = 1),
            sd_wind   = round(sd(windSpeed, na.rm = T), digits = 1),
            mean_temp = round(mean(avgTemp, na.rm = T), digits = 1),
            sd_temp   = round(sd(avgTemp, na.rm = T), digits = 1))



# mean_wind sd_wind mean_temp sd_temp
#1      11.8     1.9    1190.3   200.6




