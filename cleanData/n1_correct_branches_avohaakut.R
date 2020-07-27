
# ------------------------------------------
# Clean avohaakut simulated data
# to correspond to optimal scenarios
# --------------------------------


# Need to:
# read the .csv  files from simulated data
# correct the branching_group names: name _10 should be _15, _15 -> _20 etc.
# add management regimes names (indicated in .csv file)


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

(df.names)   
# Read all dataframes in a loop
my.df.list = lapply(df.names, function(x) {
                     read.csv(x, sep = ";", stringsAsFactors = FALSE)
  }
  )


# Merge simulated data into one df
df = do.call("rbind", my.df.list)


# The branching description is incorrect- the names are shifted 
# needs to be corected by database names first    
# rename CCF_X_40 -> CCF_X_45
unique(df$name)
unique(df$branching_group)

# simplify the .db names
df<- df %>% 
  mutate(name_new = str_replace(name, "RCP45_NEW_", ""))

unique(df$name_new)

# -----------------------------
#     Rename branches by .db name
#     last two strings
# ---------------------------

# Split dataframe in two:
# 
# 1. only to Selection cut & having 18 characters - replace last two characters 
# 2. any other rows - keep unchanged, just add new columns
df.selection <- 
  df %>%
  filter(grepl("Selection cut", branching_group) & str_length(branching_group) == 18) %>% 
  mutate(branching_new = str_replace(branching_group,
                                  str_sub(branching_group, start = -2), 
                                  str_sub(name_new, start = -2)))

# Subset unmodified rows
df.orig <- 
  df %>%
  filter(!grepl("Selection cut", branching_group) |
         (grepl("Selection cut", branching_group) & str_length(branching_group) != 18)) %>% 
  mutate(branching_new = branching_group) 


# Put two dataframes back into one
df<- rbind(df.selection, 
           df.orig)

# Check the new file: branching new

df %>% 
  filter(grepl("Selection cut_1_20", branching_group)) %>% 
  dplyr::select(name_new, branching_group, branching_new) 

# OK, seems correct (the 'name_new' corresponds to 'branching_new')

# ----------------------------
# Filter NA regime in each .db
# ---------------------------
# keep only `_SA` indication in the name name

# remove all with 'df$branching_group' & keep only ones that have _SA
# in the name names 
# split in two: 
#     - SA    - keep
#     - no.SA - filter

# ------------------------------------------------------------
# Drop all rows that do not 
# have RCP45_NEW_SA and have branching_group = NA
# keep only RCP45_SA as SA scenarios
df.no.sa <- df %>% 
  filter(name != "RCP45_NEW_SA" & !is.na(branching_group)) 

df.sa <-
  df %>% 
  filter(name == "RCP45_NEW_SA")


# rbind data together
df.av <- rbind(df.no.sa, 
               df.sa)


# ----------------------------------------------
# Add correct names of regimes 
#    following Avohaakut
# ----------------------------------------------

# Manually corrected & completed scenarios
regim_names <- read.csv("C:/MyTemp/myGitLab/windDamage/params/regimes_BAU_avohak.csv", 
                        sep = ",", 
                        stringsAsFactors = FALSE)

# Add the avohaakut names of the regimes
df.av <- df.av %>%
  left_join(regim_names, 
            by = c("branching_new" = "branching_group"),
            all.x = TRUE)


# One last check:
unique(df.av$avohaakut) # regimes 58
unique(df.av$branching_group) # 54 - corrected in new columns
unique(df.av$branching_new) # 58 - great!!



# Export the final table with corrected branching names and avohaakut regimes names
data.table::fwrite(df.av, "C:/MyTemp/avohaakut_db/analyzed/simulated_AVK_regimes.csv")




