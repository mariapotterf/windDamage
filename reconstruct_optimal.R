
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

# Merge tables into one
df = do.call("rbind", my.df.list)

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

# All regimes are SA, replace SA_DFextract by SA
#if(df$regime == "SA_DWextract", "SA")
# df$regime[df$regime == "SA_DWextract"]  <- "SA" 

# get the unique simulated stands
my.stands <- unique(df$id)


unique(df$gpkg)


# Read stand geometry
# -----------------------------
df.geom.all <- read_sf("C:/MyTemp/avohaakut_db/14.534/14.534/mvj_14.534.shp")

# Subset the stand geometry to simulated stands
df.geom.sub<- subset(df.geom.all, KUVIO_ID %in% my.stands)

# Get the final stand id between stand geometry and simulated data 
my.stands.sub <- unique(df.geom.sub$KUVIO_ID)



# Read optimal solution:
# -----------------------------
# Replace teh characters to correctly read data into 3 columns
txt <-  readLines("C:/MyTemp/avohaakut_db/solutions/Bundles_2_nocow_NPV_MANAGE_price_three_0_0_1_1_ALL0.csv")
optim <-read.table(text = gsub("[()\",\t]", " ", txt))

names(optim) <- c("id", "regime", "proportion")

# keep only the largest proportion by the stand
optim.max<-
  optim %>%
  dplyr::group_by(id) %>%
  filter(proportion == max(proportion))

# ---------------------------


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


# For stand that have multiple FM, keep only the largest one
#subset(optim, V3 != 1) # here, all stands are == 1! 

# For each stand find teh right conditions
#unique(optim$V2)

#table(optim$V2)


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
unique(dfs.opt$regime)  # 58 regimes




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

