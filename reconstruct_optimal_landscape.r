
# ----------------------------------
# Recontruct optimal landscape
# ----------------------------------

# filter simulated data (with correct names from optmal scenarios)
# to create optimal lanscape: 58 regimes, XX landscapes

# Read .csv files of optimal solutions
# if there is not 1.0 solution: 
# keep only stand that has higher proportion of the regime
# Based on the scenarios, subset the individual simulated stands
# ----------------------------------
rm(list = ls())

library(sf)

stands.remove <- c(13243875,
                   13243879,
                   13243881)

# Read corrected simulated names:
df.sim<- data.table::fread("C:/MyTemp/avohaakut_db/analyzed/simulated_AVK_regimes.csv")

df.sim <- subset(df.sim, !id %in% stands.remove)

# Read stand geometry - subset twice
# -----------------------------
df.geom <- st_read("C:/MyTemp/avohaakut_db/14.534/14.534/mvj_14.534.shp")
df.geom <- subset(df.geom, select = c("KUVIO_ID"))
names(df.geom) <- c("standid", "geometry")
df.geom$area <- st_area(df.geom)

df.geom <- subset(df.geom, !standid %in% stands.remove)

# In my data they do not have NULL geometry, but I will remove the stands from 
# simulated, optimal, and geometry data
# ------------------------------------------

# The geometry has more stands as simulated data!!
length(unique(df.geom$standid)) # 1485 !!! I need to subset the simulated data

# need to subset it
df.geom <- subset(df.geom, standid %in% unique(df.sim$id))

# -----------------------------
# Read optimal solution:
# -----------------------------

source("C:/MyTemp/myGitLab/windDamage/myFunctions.r")


# Read all optimal solutions to 
# see applied regimes?? 
# -------------------------------
setwd("C:/MyTemp/avohaakut_db/solutions")
df.optim = list.files(pattern=".csv$",
                      full.names = TRUE) # ends with .csv$, \\. matches .

df.names <- gsub("./Bundles_2_nocow_NPV_MANAGE_price_three_0_0_1_1_", "", df.optim)
df.names <- gsub(".csv", "", df.names)

# Read all dataframes in a loop
df.opt.ls = lapply(df.optim, readOptimal)

# Filter the stands from optimal scenario to exlude corrupted dstands
df.opt.ls <- lapply(df.opt.ls, function(df) subset(df, !id %in% stands.remove ))

# Merge optimal data in one files, filter for incorrect stands
opt.df.all <- do.call(rbind, df.opt.ls)


# Filter the simulated data by optimal scenario:
# Subset individual landscapes  = optimal from simulated df (all in one table)

df.sim.opt <- lapply(df.opt.ls, 
                     # Semi join subset by the stand id and by specific regime 
                     # simulated in single optimal scenario 
                     function(df.optim)  {
                       df.sim %>%
                         semi_join(df.optim,
                                   by = c("id" = "id",
                                          "avohaakut" = "regime")) })


# landscapes  43-63 have only 1474 stands??? 
lapply(df.sim.opt, function(df) length(unique(df$id)))



# ---------------------------------------------------
# Calculate the pairs of neighbors:
# calculate open_edge
# how to interpret thin year??? 
# ---------------------------------------------------
nbrs <- find_nbrs_geom(df.geom)


# Add indication of the scenarios as new column
df.sim.opt <- Map(cbind, 
                  df.sim.opt, 
                  scenario = df.names)

# Convert to single dataframe, 
# will be further split in multiple lists
df.sim.all <- do.call(rbind, df.sim.opt)

# Create new category to group teh data into landscapes:
df.sim.all$landscape <- paste(df.sim.all$year, df.sim.all$scenario, sep = "_")


# Split dataframe into dataframe list
land.ls <- df.sim.all %>% 
  group_by(landscape) %>% 
  group_split()

# calculate on one landscape
open_edge.ls <- lapply(land.ls, function(df) open_edge_by_nbrs(nbrs, df))








# Check how to interpret thinning????
unique(df.sim$THIN) # For example LRT5" or SRT5 = Short rotation thinning 5

# subset one stand to see how does the thinninh year and similated
# year work together
# and how to reclassify to suvanto's values

tb.thin <- subset(df.sim, avohaakut == "LRT5" & id == 13243875,
                  select = c("year", "THIN"))


# How to reaclassify when the thinning has happened? 
#tb.thin <- 
 # mutate(time_since = case_when( )


year = seq(5,45, 5)
event = c(NA, 14,NA, NA, 29, NA, NA, NA, NA)
event.short<- event[!is.na(event)]

my.df <- data.frame(year,
                    event)

my.df

my.df$difference <- c(0,0,1,6,11,1,6,11,16)




         
         









# Check if we can split it in individual landscapes: each regime 
# is in every year
table(df.geom$year)
table(df_opt16$avohaakut)
table(df_opt16$year, df_opt16$avohaakut)

# Seems that is well 
# next need to look throuught optimal scenarios to subset df to recreate 
# 63 different development trajectories

# Now just try to recreate single landscape in 2016
df_2016 <- subset(df_opt16, year == 2016)

head(df_opt16)

# Merge the stand geometry with simulated data for one optimal scenario
df_opt16 <-  
  df.geom %>% 
  left_join(df_opt16, by = c("KUVIO_ID" = "id"))



plot(subset(df_opt16, year == 2016)["H_dom"])
plot(subset(df_opt16, year == 2016)["avohaakut"])








