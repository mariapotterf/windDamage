
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
inPath = myPath #, "C:/Users/ge95bag/Documents/Projects/2021_WindRisk_biodiversity"
inFolder = "output/windRisk_csv"


# get table for initial conditions: select only one regime and one climate change


# Select only regimes of interest:   pattern="xx1|xxx2", 3 CC
df.names = list.files(paste(inPath, inFolder, sep = "/"), 
                      pattern = "rcp0SA_DWextract_rsk") # .csv$ CCF|x4|x3|x2|x1|BAUwoT_|BAUwGTR_|
(df.names)

# Read dataframes
df.ls <- lapply(df.names, function(name, ...) data.table::fread(paste(inPath, inFolder, name,  sep = "/"),  # 
                                                                data.table=TRUE, 
                                                                stringsAsFactors = FALSE,
                                                                integer64="character"))


# # Sample the specific IDs
# my_ids  <- unique(df.ls[[1]]$id)
# sub_ids <- sample(my_ids, 1000) 
# 
# # convert from integer64 to numeric:
# sub_ids <- as.numeric(sub_ids)
# 
# # subset teh same stands from each regime
# #df.ls2 <- list()
# df.ls2 <- lapply(df.ls, function(df) df %>% 
#                    filter(id %in% sub_ids))

# remove unnecessary columns
cl_keep <- c(
  "year",
  "branching_group",
  "Age",
  "PV",
  "cash_flow",
  "BA",
  "V",
  "Harvested_V_log" ,
  "Harvested_V_pulp",
  "V_total_deadwood",
  #"DEVEL_CLASS"
  "SC",                      
  "SOIL_CLASS",
  # "THIN"  
  "PEAT",
  "H_dom" ,
  # "D_gm"
  "MAIN_SP",
  "CAPERCAILLIE",
  "HAZEL_GROUSE" ,
  "THREE_TOED_WOODPECKER",
  "LESSER_SPOTTED_WOODPECKER",
  "LONG_TAILED_TIT",
  "SIBERIAN_FLYING_SQUIRREL",
  "COMBINED_HSI",
  "name",
  # "cell"
  "id",
  "avgTemp",
  "windSpeed",
  "regime",
  "adapt",
  "magnit" ,
  #[35] "THIN2"                     "THIN_filled_lagged"
  #[37] "difference"
  "since_thin",
  "windRisk"
)

# check if I have the same stands all over??
# lapply(df.ls2, function(df) length(unique(df$id))) # the final number varies between regimes


# keep only needed columns
df.ls <- lapply(df.ls, function(df) df %>% 
                   dplyr::select(cl_keep))


# Merge data together -----------------------------------------------
df.out <- do.call(rbind, df.ls)

# Keep only stands that have reasonable values:
df.out.sub <- df.out %>% 
  filter(id %in% my_stands)



# Make table: initial conditions
(tot_stands = length(unique(df.out.sub$id)))

# Make a summary table
summary_df <- 
  df.out.sub %>% 
  filter(year == 2016 & MAIN_SP != 0) %>% 
  mutate(species = case_when(MAIN_SP == 1 ~ "pine",
                             MAIN_SP == 2 ~ "spruce",
                            TRUE ~ "other")) %>%
  dplyr::select(-MAIN_SP) %>% 
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
            share_n     = round(n/tot_stands*100, digits = 1 )) #%>% 
 


# Format output table
formated_df <- 
  summary_df %>% 
  mutate(Height        = stringr::str_glue("{mean_height}±{sd_height}"),
         Age           = stringr::str_glue("{mean_Age}±{sd_Age}"),
         Basal_area    = stringr::str_glue("{mean_BA}±{sd_BA}"),
         Volume        = stringr::str_glue("{mean_V}±{sd_V}"),
         Species_share = stringr::str_glue("{n}({share_n})")) %>%  #,  {scales::percent(sd_height)}
  #Age    = stringr::str_glue("{scales::percent(share_bball, accuracy = 1)} ({count_bball} / {n})")) %>%
  tidyr::complete(species)  %>%

  dplyr::arrange(desc(Species_share)) %>%   # arrange by the importance NOT WORKING!
  dplyr::select(species, Species_share, Height, Age, Basal_area, Volume) 



# # copy the table form teh R data.frame format, insert as text and then Convert text to table.



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




