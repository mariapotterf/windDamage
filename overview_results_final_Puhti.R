rm(list = ls())

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


theme_set(theme_classic())
theme_update(panel.grid.major = element_line(colour = "grey95",
                                             size = 0.1,
                                             linetype = 2))

# Read datasets:
# ----------------------------------------
df.all <- fread("/projappl/project_2003256/windDamage/output/df_sim_windRisk.csv")

# Create two regimes: SA and non-SA"
df.all <- 
  df.all %>% 
  mutate(twoRegm = case_when(avohaakut == "SA" ~ "SA",
                             avohaakut != "SA" ~ "no_SA"))

#  -----------------------------------------
# read stand geometry data
# ------------------------------------------
# stands that are not simulated
stands.remove <-c(13243875, 
                  13243879, 
                  13243881,
                  6685176,     # # H_dom is >150 m 
                  13243960)    #  H_dom is 430



# Subset stands only for normal H_dom values
df.all <- subset(df.all, !id %in% stands.remove)


# stands geometry
df.geom <- st_read("/projappl/project_2003256/windDamage/14.534/14.534/mvj_14.534.shp")
df.geom <- subset(df.geom, select = c("KUVIO_ID"))
names(df.geom) <- c("id", "geometry")
df.geom$area <- st_area(df.geom)
df.geom <- subset(df.geom, id %in% unique(df.all$id))

# Total area of watershed:
tot.area = as.numeric(sum(df.geom$area))


# Read NPI values:
# -------------------------
# get the NPVI&NPI values over scenarios 
df.npi <- fread("/projappl/project_2003256/windDamage/params/MF_NPI.csv")

# reorganize teh data to correspond to simulated scenarios:
df.npi <- 
  df.npi %>% 
  rename(scenario = Type) %>%   # rename Type to scenario
  mutate(scenario = gsub("not_CCF", "RF", scenario)) %>% 
  tidyr::separate(scenario,   # Separate text from the number
            into = c("scenSimpl2", "scenNumb"), 
            sep = "(?<=[A-Za-z])(?=[0-9])") %>% 
  dplyr::select(-TypeSimple) %>% 
  mutate(scenNumb = as.numeric(scenNumb)) %>% 
  mutate(NPI = NPI/tot.area*10)   # change the values to corresponds Kyle's values



# -------------------------------------
# remove excessive columns from simulated data
# add NPI & MF values
# ------------------------------
df <-
  df.all %>% 
  # remove excessive columns
  dplyr::select(-c(V_total_deadwood, 
                DEVEL_CLASS, SC,
                SOIL_CLASS, MAIN_SP, 
                name, THIN2,
                scenario,
                THIN_filled_lagged,
                windSpeed, PEAT.v, tempSum)) %>% 
  left_join(df.npi, by = c("scenSimpl2", "scenNumb"))


# remove original table to save the memory
rm(df.all)



# Complete factors:
# ---------------------
# CHaracterize stands extends:
# ---------------------
length(unique(df.geom$area))
mean(df.geom$area)
max(df.geom$area)
min(df.geom$area)

hist(df.geom$area/10000)


# Calculate the % of SA and add to table:
df.SA_prop <-
  df %>% 
  group_by(scenSimpl2, scenNumb, avohaakut) %>% 
  distinct(id) %>% 
  summarise(stands_n = n()) %>%
  filter(avohaakut == "SA") %>% 
  mutate(SA_prop = 100* (stands_n / 1470)) %>%
  dplyr::select(-c(avohaakut))


# Add SA % (frequency) to the simulated data table
df <- 
  df %>% 
  left_join(df.SA_prop, by = c("scenSimpl2", "scenNumb"))

# export simplified table

fwrite(df, "output/final_df.csv")

