

# ---------------------------------------------
#     Filter simulated data
# -----------------------------------------

# simulated data has now BE (bionergy) alternative
# to keep only the data that are withou bioenergy 'no_BE'
# split tables into smaller ones by regime name

# Process:

# read data
# filter them to exclude BE alternative
# split data by regimes names to speed up processing
# export data in new file


# Make working example for no climate changes; then calculate values for CC scenario 
rm(list = ls())
gc()


#setwd("C:/MyTemp/myGitLab/windDamage")

# Read libraries
library(dplyr)
library(tidyr)
library(data.table)


# get data  --------------------------------------------------------
inPath = "C:/MyTemp/2021_WindRisk_biodiversity/inputData"

in_folder  = "sim_filtered"
out_folder = "output_csv"

names = c("rslt_FBE_rcp0_f", "rslt_FBE_rcp45_f", "rslt_FBE_rcp85_f") # "rslt_FBE_rcp45_f", "rslt_FBE_rcp85_f" 

# Define a function
remove_BE <- function(df_name, ...) {

    # Name input variables
  #print(df_name)
  #df_name = "rslt_FBE_rcp0_f"
  in_name    = paste0(df_name, ".csv")
  print(in_name)
  
  # Name output variables
  out_name   = gsub("rslt_FBE_", '', df_name)
  out_name   = gsub("_f", '', out_name)
  #out_name   = paste0(df_name, "_f", ".csv") # _f as filtered, e.g. no BE scenarios
 
  print(out_name)
  
  # process the data  -------------------------------------------------
  df <- data.table::fread(paste(inPath, in_folder, in_name, sep = "/"),  # 
                          data.table=TRUE)
  
    # Keep only if contains 'no_BE': how many regimes do i have left?
  #dd<- df %>% 
   # filter(grepl('no_BE', name)) 
  
  
  # split data and export -------------------------------------------------------
  print('export data')
  #(paste(inPath, paste0(out_name, regime,".csv")))
  df[, fwrite(.SD, paste(inPath, out_folder, paste0(out_name, regime,".csv"), sep = "/")), by = regime]
  
  #data.table::fwrite(dd,
   #                  paste(inPath, out_folder, out_name, sep = "/"))
  
  
  # Remove the tables -------------------------------------------------
  rm(df)
  #rm(dd)
}

lapply(names, remove_BE)















