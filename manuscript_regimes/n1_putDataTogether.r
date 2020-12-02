
# -----------------------------------------------
# Recalculate the wind risk values for individual regimes
# not for optimal scenarios
# ---------------------------------------

# Process:
# look up for original simulated tables
# merge data with 
#       geometry
#       environmental variables
# calculate wind risk for each
# plot wind risk for each regime:
# make gradients of the RF shortening, affect of thinning, CCF delay, or
# merge them together to answer questions as:
# does shortening of RF lowers wind risk? how much, what shortening should be?
# does CCF lowers wind risk in general?
# are there higher differences between stands or landscapes?? or these 
# effects are scale-independent?

# -----------------

rm(list = ls())


library(sf)
library(dplyr)
require(data.table)
library(tidyr)




# Read corrected simulated names:
df<- data.table::fread("C:/MyTemp/avohaakut_db/analyzed/simulated_AVK_regimes.csv", 
                           data.table=FALSE, 
                           stringsAsFactors = FALSE)

# Stands geometry
df.geom <- st_read("C:/MyTemp/avohaakut_db/14.534/14.534/mvj_14.534.shp")
df.geom <- subset(df.geom, select = c("KUVIO_ID"))
names(df.geom) <- c("id", "geometry")


# Enviro raster derived data
df.rst <- data.table::fread("C:/MyTemp/myGitLab/windDamage/output/df_glm_raster.csv", 
                            data.table=FALSE)




# Investigate the data: does every stand have all regimes?
# -------------------------------------------
length(unique(df$id))

unique(df$branching_group)  # this has not correct name of regime
# check avohaakut
unique(df$avohaakut)  # avohaakut is correct!!!

# Check how many stands has each regime
df %>% 
  group_by(avohaakut) %>% 
  distinct(id) %>% 
  tally() %>% 
  print(n = 70)

# Some regimes have less rows
# count number of unique stands 

# Filter all stands having teh same regime
df.sub <-
  df %>%
  group_by(id) %>%
  filter(n_distinct(avohaakut) == n_distinct(df$avohaakut))


# Cehck how many regimes I have
df.sub %>% 
  group_by(avohaakut) %>% 
  distinct(id) %>% 
  tally() %>% 
  print(n = 70)



# get vector of stands to filter them out
shared.stands <- unique(df.sub$id)


# Filter simulated data, geometry data and envirodata by vector of ids
# -----------------------------------------
df.geom.sub <-
  df.geom %>% 
  filter(id %in% shared.stands)

df.rst.sub <-
  df.rst %>% 
  rename(id = standid) %>% 
  filter(id %in% shared.stands) 


# Merge datasets together
# ------------------------------------

# Merge the datasets
df.out <- df.sub %>%
  left_join(df.rst.sub, by = ("id")) 


# Interpret THIN

# ----------------------------------
# Interpret of THIN years:
# ------------------------------
# convert 0 to NA
# IN CCF: years area stored as "2016-04-16": 
# keep only first 4 characters to convert this to numeric values
# i.e. "2016-04-16" -> to "2016""
# to calculate yearly differences

df.out1 <- 
  df.out %>%
  mutate(THIN = na_if(THIN, 0))  %>% 
  mutate(THIN2 = substring(THIN,0,4)) %>%  # keep the first 4 characters from CCF regimes, datum in format "2016-04-16" -> to "2016"
  
  group_by(id, avohaakut) %>% 
  mutate(THIN_filled_lagged = lag(THIN2)) %>%
  mutate(THIN_filled_lagged = as.numeric(THIN_filled_lagged)) %>%
  tidyr::fill(THIN_filled_lagged) %>% 
  mutate(difference = year - THIN_filled_lagged) %>% 
  mutate(since_thin = case_when(is.na(difference) | difference < 0 ~ ">10",
                                difference %in% c(0:5) ~ "0-5",
                                difference %in% c(6:10) ~ "6-10",
                                difference > 10 ~ ">10")) %>% 
  # Remove unnecessary columns
  dplyr::select(-branching_group, 
                -regime.x,
                -regime.y)



# # ---------------------------------------
# Test if THIN is correct for CCF and RF???
# -------------------------------------

# test if it ok well calculated??
# Subset two regimes and recalculate teh THIN values:
df.out1 %>% 
  filter(id == 13239722 &  avohaakut == "LRT30") %>%   # (avohaakut == "CCF_3_45" )) %>%  # | avohaakut == "LRT30"
  arrange(year) %>% 
  dplyr::select(id, year, THIN, THIN_filled_lagged, difference, avohaakut, since_thin) #%>%







# Create two regimes: SA and non-SA"
df.all <- 
  df.all %>% 
  mutate(twoRegm = case_when(avohaakut == "SA" ~ "SA",
                             avohaakut != "SA" ~ "no_SA"))














# Dummy example: filter all id by group
group = c("a", "a", "a",
          "b", "b", "b",
          "c", "c")
val = c(1,2,3,
        3,4,5,
        1,3)

dd <- data.frame(group,
                 val)


dd %>% 
  group_by(group) %>% 
  filter(all(val))

# Group by IDs and then check which ids have all groups
dd %>%
  group_by(val) %>%
  filter(n_distinct(group) == n_distinct(dd$group))



#group   val
#<fct> <dbl>
#1 a         3
#2 b         3
#3 c         3



# Ask Kyle if it is somehow possible to have the same regimes for all stands?

# Read and merge enrivromnetal conditions
# --------------------------------------------
