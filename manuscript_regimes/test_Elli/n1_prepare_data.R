
# ----------------------------------------
# process Ellinoora data for no CC, and climate change included
# ----------------------------------------

# steps:
# Get input data:
#   - get stand geometry from GPKG - already derived from previous project
#   - select one watershed from South and one from North
#   - get simulated regimes: for no change, CC4.5, CC8.5
# Select which regimes to use
#     need the most stands with regimes
#     have the same stand id for geometry and for simulated data
#     if unreal values - replace by mean?
#   - use SA values in 2016 to have the same beginning for data under all regimes

# Calculate wind risk values - prepare data
#   - make landscapes by year
#   - calculate open edge and neighbours - maybe enought with TRE and False values at the beginning
#   - calculate thinning values
#   -


# How to prepare continous landscape??? if there is a missing regime, 
# i can fill in values from the closest regime for the particular stand?



# Make working example for no climate changes; then calculate values for CC scenario 
rm(list = ls())


setwd("C:/MyTemp/myGitLab/windDamage")

# Read libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(rgdal)
library(ggspatial)
library(rgeos)
library(raster)
library(dplyr)
library(spData)
library(sf)
library(RColorBrewer)


# Correct order of variables, factors & levels
# need to have same names of columns?? YES
# when data are sourced (source()), they are all available in my script
source("C:/MyTemp/myGitLab/windDamage/myFunctions.R")



# get data
# ----------------------
inPath = "C:/MyTemp/myGitLab/windDamage/manuscript_regimes"
inFolder = "input_CC"
outFolder = 'output_CC'


names = c("Korsnas", 'Simo', 'Raasepori')
#names = c("Raasepori")

getFiles <- function(myName, ...) {
  
  #myName =  c("Korsnas")  #c("Raasepori")
  
  source("C:/MyTemp/myGitLab/windDamage/myFunctions.R")
  print(myName) 
  
  fileNameNO     = paste( "without_MV_", myName, '_rsu.csv', sep = "")
  fileNameCC45   = paste( "CC45_MV_",    myName, '_rsu.csv', sep = "")
  fileNameCC85   = paste( "CC85_MV_",    myName, '_rsu.csv', sep = "")
  fileNameRST    = paste( "df_raster_",  myName, '.csv', sep = "")
  
  # Read files
  df.no <- data.table::fread(paste(inPath, inFolder, fileNameNO,  sep = "/"),  # 
                             data.table=FALSE, stringsAsFactors = FALSE)
  df.cc45 <- data.table::fread(paste(inPath, inFolder, fileNameCC45, sep = "/"),
                               data.table=FALSE, stringsAsFactors = FALSE)
  df.cc85 <- data.table::fread(paste(inPath, inFolder, fileNameCC85, sep = "/"),
                               data.table=FALSE, stringsAsFactors = FALSE)
  # Get values for wind speed and temps sum
  df.raster <-   data.table::fread(paste(inPath, inFolder, "raster", fileNameRST, sep = "/"),
                                   data.table=FALSE, stringsAsFactors = FALSE)
  # filter the stands:
  # keep  only the overlapping standid
  shared.stands = Reduce(intersect, 
                         list(unique(df.no$id),
                              unique(df.raster$standid),
                              unique(df.cc45$id),
                              unique(df.cc85$id)))
  
  
  # Subset df.raster data to only simulated stands
  df.raster <- df.raster %>% 
    filter(standid %in% shared.stands)
  
  
  # add missing SOIL_CLASS data to no clim change data
  df.soil.class <- df.cc45 %>% 
    dplyr::select(id, SOIL_CLASS) %>% 
    distinct()
  
  
  # Add this to no CC scenario
  df.no <- df.no %>% 
    right_join(df.soil.class, by = c('id'))
  
  
  # List data frames
  df.ls <- list(df.no, df.cc45, df.cc85)
  
  
  # Add raster values: temp sum and avgWind speed
  # indication for raster values
  # add temperature and wind speed values to each table by id
  df.ls <- lapply(df.ls, function(df) df %>%  right_join(df.raster, 
                                                         by = c('id' = 'standid')))
  
  # Remove files
  rm(df.no, df.cc45, df.cc85, df.raster)
  print('removed original tables')
  
  # Get initial year: 2015 based on SA values in 2016 ------------------------
  df.ls.ini <- lapply(df.ls, addInitialYear)
  
  # Test data: in CC visible in Raasepori???
  # lapply(df.ls.ini, function(df) df %>% group_by(name) %>%  
  #         summarise(my_m = mean(H_dom, na.rm = TRUE)))
  
  # Classify thinning values ---------------------------------------------
  df.ls.thin = lapply(df.ls.ini, classifyTHIN)
  
  # Indicate climate change category; add vector to df
  clim.cat = c("no", "cc45", "cc85")
  df.ls.thin = mapply(cbind, 
                      df.ls.thin, 
                      "climChange"= clim.cat, 
                      SIMPLIFY=F)
  
  # Get vector of columns names to keep for statistics
  glm.colnames <- c("species",
                    "H_dom",
                    "time_thinning",
                    "windSpeed",
                    "open_edge",
                    "soilType",
                    "soilDepthLess30",
                    "siteFertility",   # siteFertility
                    "tempSum")
  
  
  # remove all lists
  rm(df.ls, df.ls.ini)
  
  # Format the table ------------------------------
  df.ls.glm<- lapply(df.ls.thin, formatWindRiskTable)
  
  
  # Calculate wind risk ------------------------------
  
  # apply the glm formula to calulate wind risk 
  df.risk.ls <- lapply(df.ls.glm, function(df) {
    df$windRisk <- predict.glm(windRisk.m,
                               subset(df, select = glm.colnames),
                               type="response")
  return(df)
    })
  
  # merge data into one ----------------------
  # Merge optimal data in one files, filter for incorrect stands
  df.out <- do.call(rbind, df.risk.ls)
  
  # add indication of name as new column
  df.out$siteName <- myName
  
  
  # Filter the rown that have na values??? why?
  #df.out %>% 
   # filter(!is.na(H_dom)) %>% 
    #ungroup() %>% 
    #distinct(id) %>% 
    #nrow()
  
  # Export simplified table ----------------------------------------------
  outName = paste(myName, ".csv", sep = "")
  data.table::fwrite(df.out, paste(getwd(), 'manuscript_regimes', outFolder, outName, sep = "/"))
  
 
}


#for (i in seq_along(names)) {print (i)}
df.ls2 <- lapply(names, getFiles)
  














# For single file ---------------------------------------------------------




# 
# # Read all simulated data
# dfs_input <- list.files(paste(inPath, inFolder, sep = "/"),
#                        pattern = ".csv$")
# (dfs_input)
# # Change name to add place indicator:
# dfs_names <- gsub("_rsu.csv", "", dfs_input)
# dfs_names <- gsub("_MV_", "_", dfs_names) # actually, names are already included
# 
# # Read individual .csv
# df.list <- lapply(dfs_input, function(name) data.table::fread(paste(inPath, inFolder, name, sep = "/"),
#                                                data.table=FALSE, 
#                                                stringsAsFactors = FALSE))



# Read corrected simulated names: Korsnas
# Unclear how to automate this, as needs to complete SOIL CLASS info?
#name = "Korsnas"
#fileName = paste( "without_MV_", name, '_rsu.csv', sep = "")
#paste(inPath, inFolder, "without_MV_Korsnas_rsu.csv", sep = "/")
# 
df.no <- data.table::fread(paste(inPath, inFolder, "without_MV_Korsnas_rsu.csv",  sep = "/"),  # 
                       data.table=FALSE, 
                       stringsAsFactors = FALSE)
df.cc45 <- data.table::fread(paste(inPath, inFolder, "CC45_MV_Korsnas_rsu.csv", sep = "/"),
                           data.table=FALSE, 
                           stringsAsFactors = FALSE)
df.cc85 <- data.table::fread(paste(inPath, inFolder, "CC85_MV_Korsnas_rsu.csv", sep = "/"),
                             data.table=FALSE, 
                             stringsAsFactors = FALSE)

# Get values for wind speed and temps sum
df.raster <-   data.table::fread(paste(inPath, inFolder, "raster/df_raster_Korsnas.csv", sep = "/"),
                                 data.table=FALSE, 
                                 stringsAsFactors = FALSE)

#length(unique(df.raster$standid))  # 302 

# filter the stands:
# keep  only the overlapping standid; consider all input data
shared.stands = Reduce(intersect, 
                       list(unique(df.no$id),
                            unique(df.raster$standid),
                            unique(df.cc45$id),
                            unique(df.cc85$id)))

# Subset df.raster data to only simulated stands
df.raster <- df.raster %>% 
  filter(standid %in% shared.stands)


# add missing SOIL_CLASS data to no clim change data;
# no clim change data is missing SOIL_CLASS data
df.soil.class <- df.cc45 %>% 
  dplyr::select(id, SOIL_CLASS) %>% 
  distinct()


# Add this to no CC scenario
df.no <- df.no %>% 
  right_join(df.soil.class, by = c('id'))


# include the respective SA values in 2016 to other data as 2015 to have a common start
# SA values need to be according to CCF;
# make as function to first extract values and then it to original dataset;
# need to expand it for all regimes



# Make list of input df (with and without CC) -----------------------------------
df.ls <- list(df.no, df.cc45, df.cc85)


# Add raster values: temp sum and avgWind speed
# indication for raster values
# add temperature and wind speed values to each table by id
df.ls <- lapply(df.ls, function(df) df %>%  right_join(df.raster, 
                                                        by = c('id' = 'standid')))


# Get initial year: 2015 based on SA values in 2016 ------------------------







# Update a function
addInitialYear <- function(df, ...) {
  # Add first year to simulated data, getting the SA values
  # for all regimes
  # change it for 2015
  library(tidyr)
  
#  df <- df.ls[[1]] 
  
  df.sa <- df %>% 
    filter(regime == "SA_DWextract" & year == 2016) %>% 
    mutate(year = 2015) %>%    # change the year indication
    dplyr::select(-c(regime))  # remove column regime
  
  # get indication of column order in original data
  col_names = names(df)
  
  # get vector of regimes  
  regime_v <- 
    df %>% 
    distinct(regime) %>% 
    pull(regime)
  
  # rename the column and reorder columns in sa_2015
  #df.sa.out <- 
  df.out <-
    df.sa %>% 
    crossing(regime_v) %>% # merge dataframe with vector of regime names 
    rename(regime = regime_v) %>%
    dplyr::bind_rows(df)  # merge data to new column, colums sort out by names
 
  return(df.out)
}


#aa<- addInitialYear(df.ls[[1]])


df.ls.ini <- lapply(df.ls, addInitialYear)


#df.ls.ini[[1]] %>% 
  #group_by(name) %>% 
 # dplyr::select(name, H_dom) %>% 
  #summary()


# Classify thinning values ---------------------------------------------
df.ls.thin = lapply(df.ls.ini, classifyTHIN)

# Indicate climate change category; add vector to df
clim.cat = c("no", "cc45", "cc85")
df.ls.thin = mapply(cbind, df.ls.thin, "climChange"= clim.cat, SIMPLIFY=F)



# calculate wind risk for individual regimes for 3 CC  -----------------------------------------------

# Get vector of columns names to keep for statistics
glm.colnames <- c("species",
                  "H_dom",
                  "time_thinning",
                  "windSpeed",
                  "open_edge",
                  "soilType",
                  "soilDepthLess30",
                  "siteFertility",   # siteFertility
                  "tempSum")



# Format the table ------------------------------
df.ls.glm<- lapply(df.ls.thin, formatWindRiskTable)



# Calculate wind risk ------------------------------

# apply the glm formula to calulate wind risk 
df.risk.ls <- lapply(df.ls.glm, function(df) {df$windRisk <- predict.glm(windRisk.m,
                                                           subset(df, select = glm.colnames),
                                                           type="response")
                  return(df)})

# inspect values
#lapply(df.risk.ls, function(df) range(df$windRisk, na.rm = T))
# Make a testing function: do I see differences in tree heights??
lapply(df.risk.ls, function(df) df %>% 
         # group_by(regime) %>% 
         dplyr::select(H_dom, windRisk) %>% 
         summary())



# merge data into one ----------------------
# Merge optimal data in one files, filter for incorrect stands
df.out <- do.call(rbind, df.risk.ls)


# Export simplified table ----------------------------------------------
#data.table::fwrite(df.out2, paste(getwd(), 'manuscript_regimes', outFolder, outTab, sep = "/"))




# Testing -------------------------------------------------------------------

# do differences in climate change exist here already??
# yes, input data are correct. need to filter for excessive values
#df.test <- do.call(rbind, df.ls)

df.ls[[3]] %>% 
  #group_by(name) %>% 
  dplyr::select(name, H_dom) %>% 
  summary()



# which stands have unrealistic H_dom values?
df.test %>% 
  #filter(H_dom > 45430)
  filter(H_dom > 100) %>% 
  distinct(id, name, regime)




  