
# ------------------------------------------
#    Clean avohaakut simulated data
# to correspond to optiimal scenarios
# --------------------------------


# Need to:
# read the .csv  files from simulated data
# read the optimal solution
# filter the simulated data by solutions
# read the stand geometry, calculate open_edge and wind risk


# Simulated data have wringly indicated branching group name _10 should be _15, _15 -> _20 etc.
# need to first correct those, and then merge full regimes names


rm(list = ls())

library(sf)
library(dplyr)
library(stringr)
library(data.table)


myPathAll = "C:/MyTemp/myGitLab/windDamage/input"
setwd(myPathAll)


# ^ anchor the beginning (if you begin with power, you end up with money (Evan Misshula, ))
# https://r4ds.had.co.nz/strings.html#basic-matches
df.names = list.files(myPathAll,
                        pattern=".csv$") # ends with .csv$, \\. matches .

# Read all dataframes in a loop
my.df.list = lapply(df.names, function(x) {
                     read.csv(x, sep = ";", stringsAsFactors = FALSE)
  }
  )


# What regimes are in each .db?
#lapply(my.df.list, function(df) unique(df$regime))  # they differ in # of manageement regimes

# Every selection cut has 1 to 4 indication in 'branching_group'! (CCF_1 - CCF_4) 
# need to combine with the regime! 
# Why 
#lapply(my.df.list, function(df) unique(df$branching_group))


# Merge tables into one
df = do.call("rbind", my.df.list)


# The branching description is incorrect- the names are shifted 
# needs to be corected by database names first    
# rename CCF_X_40 -> CCF_X_45
unique(df$gpkg)
unique(df$branching_group)

df<- df %>% 
  mutate(gpkg_new = str_replace(gpkg, "RCP45_NEW_", ""))

# How to do it???
ddd <- subset(df, gpkg_new == "CCF_4_15")


# Filter the data only to Selection cut
# and having more characters at the end
# split into two dataframes, and then merge into one

df.selection <- 
  df %>%
  filter(grepl("Selection cut", branching_group) & str_length(branching_group) == 18) %>% 
  mutate(branching_new = str_replace(branching_group,
                                  str_sub(branching_group, start = -2), 
                                  str_sub(gpkg_new, start = -2)))

df.orig <- 
  df %>%
  filter(!grepl("Selection cut", branching_group) | 
           (grepl("Selection cut", branching_group) & str_length(branching_group) != 18)) %>% 
  mutate(branching_new = branching_group) 



# rbind both datasets
df<- rbind(df.selection, 
           df.orig)



# ----------------------------------------------
# Recreate the management regimes to corresponds avohaakut names
# ----------------------------------------------

# Clean up the regimes based on the 'branching group'
# # Correct names are in regimes.csv table 
# Remove the SA_DW_extract or NA management from the all .dbs,
# keep only regime in _SA name specified in .db name ($gpkg)
# Add correct management regimes to the _SA db name


# Need to rename the regimes back to the Avohakkut pois codes
# Read the regime file and use the new regimes names
# how are the CCF scenarios coded?
regim_names <- read.csv("C:/MyTemp/myGitLab/windDamage/regimes_BAU_avohak.csv", 
                        sep = ";", 
                        stringsAsFactors = FALSE)

# Add the avohaakut names of the regimes
df <- df %>%
  left_join(regim_names, 
            by = c("branching_new" = "branching_group"),
            #by.x = "branching_new",
            #by.y = "branching_group", 
            all.x = TRUE)


# DONE !@!!!!

unique(df$avohaakut)
unique(df$branching_group)


# Filter data: remove all with 'df$branching_group', keep only ones that have _SA
# in the gpkg names 
# split in two: SA, no SA, in SA replace brangich group name, drop other
# ------------------------------------------------------------
# Drop all rows that do not 
# have RCP45_NEW_SA and have branching_group = NA
# keep only RCP45_SA as SA scenarios
df.no.sa <- df %>% 
  filter(gpkg != "RCP45_NEW_SA" & !is.na(branching_group)) 

df.sa <-
  df %>% 
  filter(gpkg == "RCP45_NEW_SA")

# rbind data together
df.av <- rbind(df.no.sa, 
               df.sa)


# Export the final table with corrected branching names and avohaakut regimes names
#write.csv(df.av, "C:/MyTemp/myGitLab/windDamage/output/simulated_AVK_regimes.csv")

data.table::fwrite(df.av, "C:/MyTemp/myGitLab/windDamage/output/simulated_AVK_regimes.csv")
# get the unique simulated stands
#my.stands <- unique(df.av$id)




