
# ----------------
# Create some pre-plots
# ----------------

# selet the some plots
# check if they differ


# 
rm(list = ls())



# ----------------------------------
# start the script: using rgdal library
# ----------------------------------

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


# Read corrected simulated names
# ------------------------------
df<- data.table::fread("C:/MyTemp/myGitLab/windDamage/manuscript_regimes/output/df_sim_windRisk.csv", 
                       data.table=FALSE, 
                       stringsAsFactors = FALSE)


# Read geometry data
# ---------------------
df.geom <- st_read(paste(getwd(), "14.534/14.534/mvj_14.534.shp", sep = "/"))
df.geom <- subset(df.geom, select = c("KUVIO_ID"))
names(df.geom) <- c("id", "geometry")
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




# --------------------------
# Define the plotting 
# --------------------------

# DEfine own palette
# color blind
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# -------------------------------
# Plot differences in wind risk among regimes
# -------------------------------
##p.mean.windRisk.line.npi2 <-
  df %>% 
  ggplot(aes(y = windRisk, 
             x = avohaakut )) + #, 
             #shape = scenSimpl2,     
             #color = scenSimpl2,     
             #linetype = scenSimpl2,  
             #group = scenSimpl2,     
             #fill = scenSimpl2 )) +  
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
          legend.position = "right",
          strip.background =element_rect(fill="white", color = NA))
  


# filter CCF, keep only one, keep all RF
# ---------------------------
unique(df$branching_new)
  
  
# Keep regimes, define them as factors to keep order
# some regimes do not have modification, only thin and not thin
keep.rgms <- c("CCF_1",                      # CCF, no
               "TH",                         # BAU, no
               "TT" ,                        # BAU thin, no
               "TTN" ,                       # BAU GRT,
               "SR5","SRT5",                 # BAU shorten
               "LRT5", "LRT10", "LRT15", "LRT30",    # longer thin
               "THwoTM20",                   # BAU shorten
               "THNS",                       # BAU GRT
               "THwoT",                      # BAU no thin
               "THwoT10",                    # BAU longer
               "LRH5", "LRH10", "LRH15", "LRH30", # BAU longer, no thin
               "SA"  )                       # SA

# Reclassify names of tables:
# continuous, shortening, extecsion, green tree ret
# I already have define if thining occurs or not, so maybe keep this simple
df<- 
  df %>% 
  mutate(modif = case_when(grepl("CCF_", avohaakut) ~ "CCF",
                           grepl("SR", avohaakut)   ~ "shortening",
                           grepl("TTN", avohaakut)  ~ "GTR",
                           grepl("LRT", avohaakut)  ~ "extension",
                           grepl("THwoTM20", avohaakut) ~ "shortening",
                           grepl("THNS", avohaakut) ~ "GTR",
                           grepl("THwoT10", avohaakut) ~ "extension",
                           grepl("LRH", avohaakut)  ~ "extension",
                           grepl("SA", avohaakut)   ~ "SA",
                           TRUE ~ "no"))

# Compare groups between themselves
df %>%   
  ggplot(aes(y = windRisk, 
             x = modif)) + #, 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "right",
        strip.background =element_rect(fill="white", color = NA))

  
# Check what is the development over years?
df %>%   
  ggplot(aes(y = windRisk, 
             x = modif)) + #, 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "right",
        strip.background =element_rect(fill="white", color = NA)) +
  facet_wrap(.~year)
