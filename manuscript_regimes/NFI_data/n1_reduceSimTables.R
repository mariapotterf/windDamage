

# ---------------------------------------------
#     Filter simulated data
# -----------------------------------------

# simulated data has now BE (bionergy) alternative
# to keep only the data that are withou bioenergy 'no_BE'

# Process:

# read data
# filter them to exclude BE alternative
# export data in new file


# Make working example for no climate changes; then calculate values for CC scenario 
rm(list = ls())
gc()


setwd("C:/MyTemp/myGitLab/windDamage")

# Read libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(rgdal)
library(dplyr)
library(spData)
library(sf)
library(stringr)


# get data  --------------------------------------------------------
inPath = "C:/MyTemp/2021_WindRisk_biodiversity/inputData"

in_folder  = "simulated"
out_folder = "sim_filtered"

names = c("rslt_FBE_rcp0", "rslt_FBE_rcp45", "rslt_FBE_rcp85") # 

# Define a function
remove_BE <- function(df_name, ...) {

    # Name input variables
  #print(df_name)
  in_name    = paste0(df_name, ".csv")
  
  # Name output variables
  out_name   = paste0(df_name, "_f", ".csv") # _f as filtered, e.g. no BE scenarios
 
  
  # process the data  -------------------------------------------------
  df <- data.table::fread(paste(inPath, in_folder, in_name, sep = "/"),  # 
                          data.table=TRUE)
  
  # Keep only if contains 'no_BE': how many regimes do i have left?
  dd<- df %>% 
    filter(grepl('no_BE', name)) 
  
  
  # export data -------------------------------------------------------
  data.table::fwrite(dd,
                     paste(inPath, out_folder, out_name, sep = "/"))
  
  
  # Remove the tables -------------------------------------------------
  rm(df)
  rm(dd)
}

lapply(names, remove_BE)















