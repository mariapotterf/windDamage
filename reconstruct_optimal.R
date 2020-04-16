
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




# Use new dataset
# --------------------------------
myPathAll = paste(getwd(), "input", sep = "/")
setwd(myPathAll)


# Use new values 
# =================
# Read only files containing _NO_ = no wind scenario
# ^ anchor the beginning (if you begin with power, you end up with money (Evan Misshula, ))
# https://r4ds.had.co.nz/strings.html#basic-matches
df.names = list.files(myPathAll,
                        pattern=".csv$") # ends with .csv$, \\. matches .

# Read all dtaframes in a loop
my.df.list = lapply(df.names, function(x) {read.csv(x, sep = ";")})
df = do.call("rbind", my.df.list)

# get the unique stands
my.stands <- unique(df$id)

# Read stand geometry

# st_read do not return tibble just spatial dataframe
#df.geom = st_read("stands.shp")
# I can just subset teh stands again by their stand id from the 
# petajavesi gpkg???
df.geom.all <- read_sf("C:/MyTemp/2019_mapAvohaakut/raw/MV_Petäjävesi/MV_Petäjävesi.gpkg",
                       layer = "stand")


# subset the stand geometry
df.geom.sub<- subset(df.geom.all, standid %in% my.stands)


# Get to know the data:
# -------------------------------
# how many unique stands?? ~ 1475?? 
length(unique(df$id))  # YES


# how many regimes??
length(unique(df$regime))
# only 24 regimes, why???








