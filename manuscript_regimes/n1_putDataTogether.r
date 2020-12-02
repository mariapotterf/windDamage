
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


# stands that are not simulated
stands.remove <-c(13243875, 
                  13243879, 
                  13243881,
                  6685176,     # # H_dom is >150 m 
                  13243960)    #  H_dom is 430



# Read corrected simulated names:
df<- data.table::fread("C:/MyTemp/avohaakut_db/analyzed/simulated_AVK_regimes.csv", 
                           data.table=FALSE, 
                           stringsAsFactors = FALSE)

df <- subset(df, !id %in% stands.remove)

# Subset stands only for normal H_dom values
df <- subset(df, !id %in% stands.remove)


# stands geometry
df.geom <- st_read("C:/MyTemp/avohaakut_db/14.534/14.534/mvj_14.534.shp")
df.geom <- subset(df.geom, select = c("KUVIO_ID"))
names(df.geom) <- c("id", "geometry")
df.geom$area <- st_area(df.geom)
df.geom <- subset(df.geom, id %in% unique(df$id))



# Investigate the data: does every stand have all regimes?
# -------------------------------------------
length(unique(df$id))

unique(df$branching_group)  # this has not correct name of regime
# check avohaakut
unique(df$avohaakut)  # avohaakut is correct!!!

# Check how many stands has each regime
df.sim %>% 
  group_by(avohaakut) %>% 
  distinct(id) %>% 
  tally() %>% 
  print(n = 70)

# Some regimes have less rows
# count number of unique stands 


# How to make sure that every regime will have the same stands???
# filter ids of TT regimes, check what happens if I filter just those 
stand.ids <- 
  df %>% 
  filter(avohaakut == "LRH30") %>% 
  distinct(id)    # 941 stands
  


# filter the stands to see how many stands i will have by regime
df.sim %>% 
  group_by(avohaakut) %>% 
  filter(id %in% stand.ids$id) %>% 
  distinct(id) %>% 
  tally() %>% 
  print(n = 70)


# check if every regime occurs over all years?
df.sim %>% 
  group_by(avohaakut) %>% 
#  filter(id %in% stand.ids$id) %>% 
  distinct(year) %>% 
  tally() %>% 
  print(n = 70)




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


#group   val
#<fct> <dbl>
#1 a         3
#2 b         3
#3 c         3



# Ask Kyle if it is somehow possible to have the same regimes for all stands?
