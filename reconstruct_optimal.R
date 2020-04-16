
# 
# Recreate complex landscape for the optimal solutions
# 

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


myPathAll = "C:/MyTemp/myGitLab/windDamage/input"
setwd(myPathAll)


# ^ anchor the beginning (if you begin with power, you end up with money (Evan Misshula, ))
# https://r4ds.had.co.nz/strings.html#basic-matches
df.names = list.files(myPathAll,
                        pattern="RCP*.csv$") # ends with .csv$, \\. matches .

# Read all dataframes in a loop
my.df.list = lapply(df.names, function(x) {
                     read.csv(x, sep = ";")
  }
  )

df = do.call("rbind", my.df.list)

# get the unique stands
my.stands <- unique(df$id)


# Read stand geometry
# -----------------------------
# st_read do not return tibble just spatial dataframe
# I can just subset teh stands again by their stand id from the 
# petajavesi gpkg??? Joo!
df.geom.all <- read_sf("C:/MyTemp/2019_mapAvohaakut/raw/MV_Petäjävesi/MV_Petäjävesi.gpkg",
                       layer = "stand")

# subset the stand geometry to simulated stands
df.geom.sub<- subset(df.geom.all, standid %in% my.stands)

# Plot the data
#plot(df.geom.sub["maingroup"])

#plot(df.geom.all["maingroup"])


# Read optimal solution:
optim <- read.csv("C:/MyTemp/avohaakut_db/solutions/Bundles_2_nocow_NPV_MANAGE_price_three_0_0_1_1_ALL0.csv",
                  sep =  "\t",
                  header = F)

optim$V1 <- gsub('(', "", optim$V1)

# Get to know the data:
# -------------------------------
# how many unique stands?? ~ 1475?? 
length(unique(df$id))  # YES


# how many regimes??
length(unique(df$regime))
# only 24 regimes, why???








