
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

load("C:/MyTemp/myGitLab/windDamage/.RData")

library(sf)
library(dplyr)
require(data.table)
library(tidyr)



stands.remove <- c(13243875,
                   13243879,
                   13243881)

# Read corrected simulated names:
df.sim<- data.table::fread("C:/MyTemp/avohaakut_db/analyzed/simulated_AVK_regimes.csv", 
                           data.table=FALSE, 
                           stringsAsFactors = FALSE)

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

# Convert to dataframe to have teh same odred of data as open_edge
#land.df.all <- do.call(rbind, land.ls)

# ---------------------------------------------------
# Calculate the pairs of neighbors:
# calculate open_edge
# how to interpret thin year??? 
# ---------------------------------------------------
nbrs <- find_nbrs_geom(df.geom)


# calculate on one landscape
open_edge.ls <- lapply(land.ls, function(df) open_edge_by_nbrs(nbrs, df))

#open_edge.df.all <- do.call(rbind, open_edge.ls)

# change teh standid to id:
for (i in seq_along(open_edge.ls)){
  colnames(open_edge.ls[[i]]) <- c("id", "open_edge")
}


# here is a problem how to merge all data togetehr!!
merged.ls <- Map(merge, land.ls, open_edge.ls, by="id")

merged.df <- do.call(rbind, merged.ls)


#fwrite(merged.df, "C:/MyTemp/myGitLab/windDamage/output/df.sim_open_edge.csv")

#---------------------------------------------------------------------------------
# -----------------------------------------------------------------------------


# Interpretation of THIN years:
# convert 0 to NA
# calculate difference from other years


# Actually, not each stand has to have all regimes, as we have optimal data
#  
# ----------------------------------------------


# Thinning calculation takes forever: calculate time since thinning only for scenarios & stands 
# with THIN included
# convert all 0 to NA

merged.df2 <- merged.df %>% 
  group_by(id, scenario) %>% 
  mutate(THIN = na_if(THIN, 0)) %>% 
  mutate(THIN_filled_lagged = lag(THIN)) %>% # make sure that it is not calculated from previous value??
  mutate(THIN_filled_lagged = as.numeric(THIN_filled_lagged)) %>%
  tidyr::fill(THIN_filled_lagged)  %>%      # fill rows with values  
  mutate(difference = if_else(year > THIN_filled_lagged,   # calculate the difference
                              year - THIN_filled_lagged,
                              NA_real_,
                              missing = 0))  %>%
  mutate(since_thin = case_when(difference < 0 ~ "NA",
                                difference %in% c(0:5) ~ "0-5",
                                difference %in% c(6:10) ~ "6-10",
                                difference > 10 ~ ">11"))


unique(merged.df2$difference)
unique(merged.df2$since_thin)

         

# inscepct the data if the the difference is not calculated between 
# different stands or scenarios
# unsure how can I check for this???


         
# write the table
fwrite(merged.df2, "C:/MyTemp/myGitLab/windDamage/output/df_glm.csv")









