
# ------------------------
# Calculate difference between wind risk value between neighbors
# ------------------------

# get simulated data with calculated wind risk
# get list of neighbors to get the values from teh the simulated data


# Results and methods for paper 

# Read libraries ----
rm(list = ls())


library(data.table)
library(dplyr)




# Set wd ------------------------------------------------------------------

setwd("C:/MyTemp/myGitLab/windDamage/output/even_flow")

# Read input data ----------------------------------------------------

# includes optimal scenario, raster data, windRisk
df <- fread(paste(getwd(), "finalFoPlotting.csv", sep = "/"))

# get list of neighbors: central cell and neighbors, get wind risk values
df_nbr <- data.table::fread(paste(getwd(), "df_nbrs_diff.csv", sep = "/"),
                                  data.table=FALSE, 
                                  stringsAsFactors = FALSE)

# filter df_nbrs to have only central-neighbors pairs:
df_nbr2 <- df_nbr %>% 
  dplyr::select(central_id, nbrs_id) %>% 
  distinct()

# Out data
outTab  = "df_nbrs_diff_risk.csv"  # difference between wind risk values between neighbors


# Filter the data to get values of wind risk between 
# neighbors
# maybe make data shorter?
df_filt <- df %>% 
  dplyr::select(id, landscape, windRisk)

# how to filter the simulated data to get the list 
# maybe just paste central_id and nbrs_id together???


# Merge two dataframes together
df_out<-
  df_filt %>% 
  left_join(df_nbr2, by = c('id' = 'central_id')) %>%
  left_join(df_filt, by = c('landscape' = 'landscape',
                            'nbrs_id' = 'id'),
            suffix = c('', '_nbrs')) 


# write the table
#fwrite(merged.nbrs.df, "C:/MyTemp/myGitLab/windDamage/output/even_flow/df_nbrs_diff.csv")
fwrite(df_out, paste(getwd(), outTab , sep = "/"))  
  
  
  

