
# ------------------------------------------
# Recreate complex landscape for the optimal solutions
# --------------------------------

# Read .csv files of optimal solutions
# if there is not 1.0 solution: 
# keep only stand that has higher proportion of the regime
# Based on the scenarios, subset the individual simulated stands
# ----------------------------------

# Need to:
# read the .csv  files from simulated data
# read the optimal solution
# filter the simulated data by solutions
# read the stand geometry, calculate open_edge and wind risk



rm(list = ls())

library(sf)
library(dplyr)
library(stringr)


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
lapply(my.df.list, function(df) unique(df$regime))  # they differ in # of manageement regimes

# Every selection cut has 1 to 4 indication in 'branching_group'! (CCF_1 - CCF_4) 
# need to combine with the regime! 
# Why 
lapply(my.df.list, function(df) unique(df$branching_group))

lapply(my.df.list, name)


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
ddd %>% 
  select(branching_group, gpkg_new)

unique(paste(df$branching_group, df$gpkg_new, sep = " "))


# Working example
my.df <- data.frame(wrong  = c("a_1", "a_1_5", "a_1_10", "a_2_10", "a_1_15"),
                    better = c("cc_1", "cc_1_5",  "cc_1_10", "cc_1_15", "cc_2_20"))

my.df %>% 
  filter(str_length(wrong) == 6) %>% 
  mutate(wanted = str_replace(wrong,
                              str_sub(wrong, start = -2), 
                              str_sub(better, start = -2)))
  

#    wrong  better  wanted
# 1    a_1    cc_1    cc_1
# 2  a_1_5  cc_1_5  cc_1_5 
# 3 a_1_10 cc_1_10 cc_1_10
# 4 a_2_10 cc_1_15 cc_2_15
# 5 a_1_15 cc_2_20 cc_1_20


str_sub("cc_2_20", start = -2)  # subset the last two characters

# ----------------------------------------------
# Recreate the management regimes to corresponds avohaakut names
# ----------------------------------------------

# Clean up the regimes based on the 'branching group'
# # Correct names are in regimes.csv table 
# Remove the SA_DW_extract or NA management from the all .dbs,
# keep only regime in _SA name specified in .db name ($gpkg)
# Add correct management regimes to the _SA db name

#df1 <- df %>%
 #   mutate(reg_opt <- case_when(
  #     grepl("Selection cut_", branching_group) ~ "CCF_"))

# do not use previously derived regimes, as now we have new ones
# unique(df$regime)






# Need to rename the regimes back to the Avohakkut pois codes
# Read the regime file and use the new regimes names
# how are the CCF scenarios coded?
regim_names <- read.csv("C:/MyTemp/myGitLab/windDamage/regimes_BAU_avohak.csv", 
                        sep = ";", 
                        stringsAsFactors = FALSE)

# Add the avohaakut names of the regimes
df <- df %>%
  left_join(regim_names, 
            by = "branching_group", 
            all.x = TRUE)


unique(df$avohaakut)
unique(df$branching_group)


# Filter data: remove all with 'df$branching_group', keep only ones that tahve _SA
# in the gpkg names 
# split in two: SA, no SA, in SA replace brangich group name, drop other

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


# get the unique simulated stands
#my.stands <- unique(df.av$id)




# Read stand geometry
# -----------------------------
df.geom.all <- read_sf("C:/MyTemp/avohaakut_db/14.534/14.534/mvj_14.534.shp")

# Subset the stand geometry to simulated stands
df.geom.sub<- subset(df.geom.all, KUVIO_ID %in% my.stands)

# Get the final stand id between stand geometry and simulated data 
my.stands.sub <- unique(df.geom.sub$KUVIO_ID)


# -----------------------------
# Read optimal solution:
# -----------------------------

# Test on one file:
# ---------------------------

# Replace teh characters to correctly read data into 3 columns
#txt <-  readLines("C:/MyTemp/avohaakut_db/solutions/Bundles_2_nocow_NPV_MANAGE_price_three_0_0_1_1_ALL0.csv")
#optim <-read.table(text = gsub("[()\",\t]", " ", txt))

#names(optim) <- c("id", "regime", "proportion")

# keep only the largest proportion by the stand
#optim.max<-
 # optim %>%
  #dplyr::group_by(id) %>%
  #filter(proportion == max(proportion))


# Make a function to read the optimal data correctly and 
# to filter just the prevailing forest management by stand
readOptimal <- function(df.path, ...) {
  
  # read individual lines
  txt <-  readLines(df.path)
  
  # Read table by replacing characters
  optim <-read.table(text = gsub("[()\",\t]", " ", txt))
  
  # Rename the columns of columns names
  names(optim) <- c("id", "regime", "proportion")
  
  # keep only the largest proportion by the stand
  optim.max<-
    optim %>%
    dplyr::group_by(id) %>%
    filter(proportion == max(proportion))
  
  # Return the dataframe with correct columns and filtered 
  # stands having one regime by stand
  return(optim.max)
  
}


# Read all optimal solutions to 
# see applied regimes?? 
# -------------------------------
setwd("C:/MyTemp/avohaakut_db/solutions")
df.optim = list.files(pattern=".csv$",
                      full.names = TRUE) # ends with .csv$, \\. matches .

# Read all dataframes in a loop
df.opt.ls = lapply(df.optim, readOptimal)

# Merge tables into one
dfs.opt = do.call("rbind", df.opt.ls)


# Check what management are there??
sort(unique(dfs.opt$regime))  # 58 regimes
sort(unique(df.av$avohaakut)) 


# some regims area missing from simuylated data?
setdiff(sort(unique(dfs.opt$regime)),
        sort(unique(df.av$avohaakut)) )




write.csv(sort(unique(dfs.opt$regime)), 
          "C:/MyTemp/myGitLab/windDamage/params/regimes_58.csv")

# How to merge the 58 regimes with my databases??? 

unique(df$gpkg) 
unique(df$regime)



# Get to know the data:
# -------------------------------
# how many unique stands?? ~ 1475?? 
length(unique(df$id))  # YES


# how many regimes??
length(unique(df$regime))
# only 24 regimes, why???



# Subset the optimal solution of the simulated data: will this recreate my landscape? 
# will there be any repetitions??
# -----------------------------------
# subset by one .db
df.sa <- subset(df, gpkg == "RCP45_NEW_SA" & id %in% my.stands.sub)

length(unique(df.sa$id))
# 1238

length(unique(df.sa$regime))
# 1  # SA has only one regime

unique(df.sa$year)

# is each stand simulated every years???
table(df.sa$year)  # YES



# Check CCF regime
# ------------------------------------

# subset by one .db
df.ccf_4_45 <- subset(df, gpkg == "RCP45_NEW_CCF_4_45" & id %in% my.stands.sub)

length(unique(df.ccf_4_45$id))
# 1238

length(unique(df.ccf_4_45$regime))
# 1  # SA has only one regime

unique(df.ccf_4_45$year)

# is each stand simulated every years???
table(df.ccf_4_45$year)  # YES




# -----------------------------------------
# Try to subset the optimal scenario:
# -----------------------------------------

# from the whole simulated data : df)
# for the standid I need to subset specific regime

opt.df <- subset(df, id %in% unique(optim.max$id))

