
# ----------------------------
# Results and methods for paper
# -----------------------------
rm(list = ls())


# Add this to your R code:
#.libPaths(c("/projappl/project_2003256/project_rpackages", #.libPaths()))
#libpath <- .libPaths()[1]

#.libPaths(c("/projappl/project_2003256/project_rpackages", .libPaths()))


# , eval = FALSE
library(data.table)
library(dplyr)
library(raster)
library(ggplot2)
library(sf)
library(stringr)
library(gridExtra)
library(tidyr)
library(ggpubr)
library(broom)
#library(RColorBrewer)


theme_set(theme_classic())
theme_update(panel.grid.major = element_line(colour = "grey95",
                                             size = 0.1,
                                             linetype = 2),
             strip.background = element_rect(color="grey95", 
                                             fill="grey95",
                                             size=0.1, 
                                             linetype="solid"))



# Read input data
# =========================
#df <- fread("/projappl/project_2003256/windDamage/output/final_df_solution8.csv")
df <- fread(paste(getwd(), "output/even_flow/final_df_solution8.csv", sep = "/"))



# stands geometry
df.geom <- st_read(paste0(getwd(),"/14.534/14.534/mvj_14.534.shp"))

#df.geom <- st_read("C:/MyTemp/avohaakut_db/14.534/14.534/mvj_14.534.#shp")

df.geom <- subset(df.geom, select = c("KUVIO_ID"))
names(df.geom) <- c("id", "geometry")
df.geom$area <- st_area(df.geom)
df.geom <- subset(df.geom, id %in% unique(df$id))



# replace all NA in volume by 0 - because not volume is available there
df<- 
  df %>% 
  dplyr::mutate(V_stand_log = replace_na(V_stand_log, 0)) %>% 
  dplyr::mutate(V_stand_pulp = replace_na(V_stand_pulp, 0)) %>% 
  dplyr::mutate(V_strat_max = replace_na(V_strat_max, 0)) %>% 
  dplyr::mutate(V_strat_max_log = replace_na(V_strat_max_log, 0)) %>% 
  dplyr::mutate(V_strat_max_pulp = replace_na(V_strat_max_pulp, 0)) %>% 
  dplyr::mutate(Harvested_V_log_under_bark = replace_na(Harvested_V_log_under_bark, 0)) %>% 
  dplyr::mutate(Harvested_V_pulp_under_bark = replace_na(Harvested_V_pulp_under_bark, 0)) 


# Replace the no_SA values in TwoRegms: change to Management
df <- 
  df %>% 
  dplyr::mutate(Management = twoRegm) %>% # copy the columns 
  dplyr::mutate(Management = replace(Management, # replace the values
                                     Management == "no_SA", "Active")) %>%
  dplyr::mutate(Management = replace(Management, # replace the values
                                     Management == "SA", "Set Aside"))






# Get basic characteristics of teh initial dataset
# use summaries from the SA data in 2016
# mean area, proportion of species, fertility, soilDept, soil type
# # uce across for multiple numeric variables: https://www.datanovia.com/en/blog/dplyr-how-to-compute-summary-statistics-across-multiple-columns/
# Or specify columnsL:
# https://stackoverflow.com/questions/59214500/summary-table-of-numeric-and-categorical-data-in-r
summary_df <- 
  df %>% 
  filter(year == 2016 & 
           avohaakut == "SA" & 
           landscape == '2016_./Bundles_2_nocow_INCOME_MANAGE_price_three_0_0_1_1ALL0') %>% 
  group_by(species) %>% 
  summarise(mean_height = round(mean(H_dom, na.rm = T), digits = 1),
            sd_height   = round(sd(  H_dom, na.rm = T), digits = 1),
            mean_Age    = round(mean(Age, na.rm = T),   digits = 1),
            sd_Age      = round(sd(  Age, na.rm = T),   digits = 1),
            mean_BA     = round(mean(BA, na.rm = T),    digits = 1),
            sd_BA       = round(sd(  BA, na.rm = T),    digits = 1),
            mean_V      = round(mean(V, na.rm = T),     digits = 1),
            sd_V        = round(sd(  V, na.rm = T),     digits = 1),
            n           = n(), # count species
            share_n = round(n/1470*100, digits = 1 )) 
  
  #nrow()
  #print(n = 100)


# Format output table
formated_df <- 
  summary_df %>% 
  mutate(Height  = stringr::str_glue("{mean_height}±{sd_height}"),
         Age     = stringr::str_glue("{mean_Age}±{sd_Age}"),
         Basal_area      = stringr::str_glue("{mean_BA}±{sd_BA}"),
         Volume       = stringr::str_glue("{mean_V}±{sd_V}"),
         Species_share = stringr::str_glue("{n}({share_n})")) %>%  #,  {scales::percent(sd_height)}
       #Age    = stringr::str_glue("{scales::percent(share_bball, accuracy = 1)} ({count_bball} / {n})")) %>%
  tidyr::complete(species)  %>%
  dplyr::select(species, Species_share, Height, Age, Basal_area, Volume)


# copy the table form teh R data.frame format, insert as text and then Convert text to table.

#knitr::kable(formated_df)


# Count the soil conditions
df %>% 
  filter(year == 2016 & 
         avohaakut == "SA" & 
         landscape == '2016_./Bundles_2_nocow_INCOME_MANAGE_price_three_0_0_1_1ALL0') %>% 
  group_by(soilType) %>% 
  summarise(n           = n(), # count species
            share_n = round(n/1470*100, digits = 1 )) 

#soilType           n share_n
##<chr>          <int>   <dbl>
#  1 mineral coarse  1090    74.1
#2 mineral fine       2     0.1
#3 organic          378    25.7



# Site fertility
df %>% 
  filter(year == 2016 & 
           avohaakut == "SA" & 
           landscape == '2016_./Bundles_2_nocow_INCOME_MANAGE_price_three_0_0_1_1ALL0') %>% 
  group_by(siteFertility) %>% 
  summarise(n           = n(), 
            share_n = round(n/1470*100, digits = 1 )) 

# Get area+- sd

df %>% 
  filter(year == 2016 & 
           avohaakut == "SA" & 
           landscape == '2016_./Bundles_2_nocow_INCOME_MANAGE_price_three_0_0_1_1ALL0') %>% 
 # group_by(siteFertility) %>% 
  summarise(mean_area = round(mean(area, na.rm = T), digits = 1),
            sd_area   = round(sd(area, na.rm = T), digits = 1))

#mean_area sd_area
#   15244.1 16226.5



  
 
  
