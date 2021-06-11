
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
  
  #name = c("Raasepori")
  
  source("C:/MyTemp/myGitLab/windDamage/myFunctions.R")
  print(myName) 
  
  fileNameNO   = paste( "without_MV_", myName, '_rsu.csv', sep = "")
  fileNameCC45   = paste( "CC45_MV_",    myName, '_rsu.csv', sep = "")
  fileNameCC85   = paste( "CC85_MV_",    myName, '_rsu.csv', sep = "")
  fileNameRST    = paste( "df_raster_",  myName, '.csv', sep = "")
  
  # Read files
  df.no <- data.table::fread(paste(inPath, inFolder, fileNameNO,  sep = "/"),  # 
                             data.table=FALSE, stringsAsFactors = FALSE)
  df.cc45 <- data.table::fread(paste(inPath, inFolder, fileNameCC45, sep = "/"),
                               data.table=FALSE, stringsAsFactors = FALSE)
  df.cc85 <- data.table::fread(paste(inPath, inFolder, fileNameCC45, sep = "/"),
                               data.table=FALSE, stringsAsFactors = FALSE)
  # Get values for wind speed and temps sum
  df.raster <-   data.table::fread(paste(inPath, inFolder, "raster", fileNameRST, sep = "/"),
                                   data.table=FALSE, stringsAsFactors = FALSE)
  # filter the stands:
  # keep  only the overlapping standid
  shared.stands = intersect(unique(df.no$id), unique(df.raster$standid))
  
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
# keep  only the overlapping standid
shared.stands = intersect(unique(df.no$id), unique(df.raster$standid))

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
df.ls.ini <- lapply(df.ls, addInitialYear)


# Classify thinning values ---------------------------------------------
df.ls.thin = lapply(df.ls.ini, classifyTHIN)

# Indicate climate change category; add vector to df
clim.cat = c("no", "cc45", "cc85")
df.ls.thin = mapply(cbind, df.ls.thin, "climChange"=clim.cat, SIMPLIFY=F)


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



# merge data into one ----------------------
# Merge optimal data in one files, filter for incorrect stands
df.out <- do.call(rbind, df.risk.ls)


# classify for type of modification ------------------

# Classify the type of regime, type of adjustement (extension or shortening)
# and change in time (how many years)
df.out2 <- 
  df.out %>% 
  mutate(modif = case_when(
    grepl('_m5' , regime) ~ 'shorten',
    grepl('_m20', regime) ~ 'shorten',
    grepl('_5'  , regime) ~ 'extended',
    grepl('_10' , regime) ~ 'extended',
    grepl('_15' , regime) ~ 'extended',
    grepl('_30' , regime) ~ 'extended',
    TRUE~ 'normal')) %>% 
  mutate(change_time = case_when(
    grepl("_15", regime)  ~ "15",
    grepl("_5",  regime)  ~ "5",
    grepl("_10", regime)  ~ "10",
    grepl("_30", regime)  ~ "30",
    grepl("_20", regime)  ~ "20",
    grepl("_m5", regime)  ~ "-5",
    grepl("_m20", regime) ~ "-20",
    TRUE~'0')) %>% 
 # mutate(change_time = replace_na(change_time, 0)) %>% 
  mutate(mainType = case_when(
    grepl("SA", regime) ~ "SA",
    grepl("BAU", regime) ~ "BAU",
    grepl("CCF", regime) ~ "CCF")) %>% 
  mutate(thinning = case_when(
    grepl("wG|wT", regime) ~ "thin_YES",
    grepl("woT", regime) ~ "thin_NO",
    TRUE~'thin_NO'))
    
    
# head(df.out2)
# 
# unique(df.out2$modif)
# unique(df.out2$change_time)
# unique(df.out2$mainType)
# unique(df.out2$thinning)
# HOw does modification of management regime affect wind risk?

# Change order of change time---------------------------------------
df.out2$change_time <-factor(df.out2$change_time, 
                         levels = c("-20", "-15", "-10", "-5", "0",  "5",  "10", "15","20", "25", "30"))

# Export simplified table ----------------------------------------------
data.table::fwrite(df.out2, paste(getwd(), 'manuscript_regimes', outFolder, outTab, sep = "/"))


  