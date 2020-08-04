
# Add this to your R code:
.libPaths(c("/projappl/project_2003256/project_rpackages2", .libPaths()))
libpath <- .libPaths()[1]

# This command can be used to check that the folder is now visible:
.libPaths() # It should be first on the list

# Package installations should now be directed to the project
# folder by default. You can also specify the path, e.g.:
install.packages("fastDummies", lib = libpath)


# ----------------------------------
# Recontruct optimal landscape
# ----------------------------------

# filter simulated data (with correct names from optmal scenarios)
# to create optimal lanscape: 58 regimes, 63 landscapes x 20 time steps

# Read .csv files of optimal solutions
# if there is not 1.0 solution: 
# keep only stand that has higher proportion of the regime
# Based on the scenarios, subset the individual simulated stands
# ----------------------------------
rm(list = ls())


library(sf)
library(dplyr)
require(data.table)
library(tidyr)


stands.remove <- c(13243875,
                   13243879,
                   13243881)

# Read corrected simulated names:
df.sim<- data.table::fread("/projappl/project_2003256/windDamage/output/simulated_AVK_regimes.csv", 
                           data.table=FALSE, 
                           stringsAsFactors = FALSE)

df.sim <- subset(df.sim, !id %in% stands.remove)



# -----------------------------
# Read optimal solution:
# -----------------------------

source("/projappl/project_2003256/windDamage/myFunctions.R")


# Read all optimal solutions to 
# see applied regimes?? 
# -------------------------------
setwd("/projappl/project_2003256/Solutions_4")
df.optim = list.files(pattern=".csv$",
                      full.names = TRUE) # ends with .csv$, \\. matches .


# Replace names indications by simpler way 

df.names <- gsub("./Bundles_2_nocow_NPV_MANAGE_price_three_0_0_1_1_", "", df.optim)
df.names <- gsub(".csv", "", df.names)
df.names <- gsub("not_CCF", "RF", df.names)

(df.names)

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



# ----------------------------------
# Interpret of THIN years:
# ------------------------------
# convert 0 to NA
# IN CCF: years area stored as "2016-04-16": 
# keep only first 4 characters to convert this to numeric values
# i.e. "2016-04-16" -> to "2016""
# to calculate yearly differences

df.sim.all2 <- 
  df.sim.all %>%
  mutate(THIN = na_if(THIN, 0))  %>% 
  mutate(THIN2 = substring(THIN,0,4)) %>%  # keep the first 4 characters from CCF regimes, datum in format "2016-04-16" -> to "2016"
 
   group_by(id, avohaakut, scenario) %>% 
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


# Check:

unique(df.sim.all2$difference)
unique(df.sim.all2$since_thin)


# Create new factors: existence of thinning, simpleScen
# --------------------------------
df.sim.all2 <-
  df.sim.all2 %>% 
  # Differentiate between SA, CCF and RF with and without thinning
  mutate(avoh_Simpl = case_when(   
    str_detect(avohaakut, "SA")   ~ "SA",
    str_detect(avohaakut, "CCF_") ~ "CCF",
    str_detect(avohaakut, "LRH")  ~ "RF_noT",
    str_detect(avohaakut, "LRT")  ~ "RF_T",
    str_detect(avohaakut, "SR5")  ~ "RF_noT",
    str_detect(avohaakut, "SRT5") ~ "RF_T",
    str_detect(avohaakut, "TH")   ~ "RF_noT",
    str_detect(avohaakut, "TT")   ~ "RF_T")) %>% 
  # Simple 3 scenarios: RF, CCF and ALL
  mutate(simpleScen = case_when(
    stringr::str_detect(scenario, "RF") ~ "RF",
    stringr::str_detect(scenario, "ALL") ~ "ALL",
    stringr::str_detect(scenario, "CCF") ~ "CCF"))


# write the table
fwrite(df.sim.all2, "/projappl/project_2003256/windDamage/output/df_sim_opt.csv")










