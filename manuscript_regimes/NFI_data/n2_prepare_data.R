
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
#   - set all open_edge = TRUE
#   - calculate thinning values
#   -



# Make working example for no climate changes; then calculate values for CC scenario 
rm(list = ls())


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


# Correct order of variables, factors & levels
# need to have same names of columns?? YES
# when data are sourced (source()), they are all available in my script
source("C:/MyTemp/myGitLab/windDamage/myFunctions.R")



# get data
# ----------------------
inPath = "C:/MyTemp/2021_WindRisk_biodiversity/inputData/simulated/rslt_FBE_rcp0"



#inFolder = "input_CC"
#outFolder = 'output_CC'


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

# To do:
# Check if : 
#  have all regimes? e.g. selection cut
#  have all columns needed?
# merge simulated data with XY coordinates: 
#   create a unique id number for simulated data to fit XY data
#   XY data contains raster characteristics: made in script 'NFI_data/getRaster_values.R'
# Reclassify regimes names


# Convert csv to db: will it reads faster?? --------------------------

# Make working example for no climate changes; then calculate values for CC scenario 
rm(list = ls())


#setwd("C:/MyTemp/myGitLab/windDamage")

# Read libraries
library(dplyr)
library(tidyr)
library(data.table)
library(stringr)


# Correct order of variables, factors & levels
# need to have same names of columns?? YES
# when data are sourced (source()), they are all available in my script
source("C:/MyTemp/myGitLab/windDamage/myFunctions.R")




# get data
# ----------------------
inPath = "C:/MyTemp/2021_WindRisk_biodiversity"

# Storage info 
in_folder  = "inputData/output_csv"
out_folder = "output/windRisk_csv"

# get vector of file names
#names = c("rcp0xxx8") #.csv

# Get raster values and filetr the raster vals
# - values for wind speed and temps sum
df.raster <-   data.table::fread(paste(inPath, "inputData/spatial/df_raster_XY.csv", sep = "/"),
                                 data.table=TRUE)

# Filter raster values
df.raster <-  df.raster %>% 
  dplyr::select(OBJECTID, 
                ID, 
                X_10, #Y, 
                Stand, 
                avgTemp, 
                windSpeed) %>% 
  mutate(ID = as.character(ID))


# Get list of classified regimes with classified adaptation
df.regimes <- data.table::fread("C:/MyTemp/myGitLab/windDamage/params/regimes_windRisk.csv")


#df_name = "rslt_FBE_rcp0_f"

# ------------------------------------
#           Process data
# ------------------------------------

# Get an empty list to store tables
#my_ls <- list()
df_name <- list.files(path = paste(inPath, in_folder, sep = "/"),
                       pattern=".csv$",
                       full.names = F) # ends with .csv$, \\. matches .

df_name <- gsub(".csv", '', df_name)

#df_name = c("rcp0BAUwT", "rcp85xxx8")

lapply(df_name, processAll) 

processAll <- function(df_name, ...) {
  
  
  # Declare variables 
  in_name   = paste0(df_name, '.csv')
  (out_name = paste0(df_name, '_rsk.csv'))
  
  # DEfine functions:
  readFiles <- function(df_name, ...) {

    # Read simulated data
    df <- data.table::fread(paste(inPath, in_folder, in_name, sep = "/"),  #
                            data.table=TRUE)
   # print(head(df))

    return(df)
  }
  getUniqueID_sim <- function(df, ...) {
    # 
    #    # ========================================== #
    #    #      Create unique ID for simulated data   # --------------------------------
    #    #            to merge them with XY           # --------------------------------
    #    # ========================================== #
    # 
    #    # Steps:
    #    # 1. 'Zero paddling': add zeros to the beginning of 'id' ,
    #    #     eg. fill in 00000XX values to have always 8 characters starting with 0
    #    # 2. rename grid cells ('k3') into consecutive numbers ('1'...)
    #    # 3. Add numeric grid indication at the beginning of the 8 digit paddled 'id'
    #    # 4. voila! done
    # 
    df <-
      df %>%
      mutate(zero_id = formatC(id,
                               width = 8,
                               format = "d",
                               flag = "0")) %>%
      mutate(cell = str_sub(name,-2))  %>%
      mutate(nb = case_when(
        cell == "k3" ~ "1",
        cell == "k4" ~ "2",
        cell == "l2" ~ "3",
        cell == "l3" ~ "4",
        cell == "l4" ~ "5",
        cell == "l5" ~ "6",
        cell == "m3" ~ "7",
        cell == "m4" ~ "8",
        cell == "m5" ~ "9",
        cell == "n3" ~ "10",
        cell == "n4" ~ "11",
        cell == "n5" ~ "12",
        cell == "n6" ~ "13",
        cell == "p3" ~ "14",
        cell == "p4" ~ "15",
        cell == "p5" ~ "16",
        cell == "p6" ~ "17",
        cell == "q3" ~ "18",
        cell == "q4" ~ "19",
        cell == "q5" ~ "20",
        cell == "r4" ~ "21",
        cell == "r5" ~ "22",
        cell == "s4" ~ "23",
        cell == "s5" ~ "24",
        cell == "t4" ~ "25",
        cell == "t5" ~ "26",
        cell == "u4" ~ "27",
        cell == "u5" ~ "28",
        cell == "v3" ~ "29",
        cell == "v4" ~ "30",
        cell == "v5" ~ "31",
        cell == "w3" ~ "32",
        cell == "w4" ~ "33",
        cell == "w5" ~ "34",
        cell == "x4" ~ "35",
        cell == "x5" ~ "36")) %>%
      mutate(ID = paste0(nb, zero_id)) %>%
      mutate(ID = as.character(ID))
    
    return(df)
  }
  
  # Run the flow ------------------------
  # Read files
  df <- readFiles(df_name)
 #
  # Apply the uniqueID function over the list
  dd<-getUniqueID_sim(df)

  # Join raster values -------------------------------------------------
  dd <- dd %>%  inner_join(df.raster,
                           by = c('ID'))

  # Remove excessive columns -------------------------------------------
  dd <- dd %>%
    dplyr::select(-c(id, zero_id, nb,
                     X_10, OBJECTID,
                     Stand)) %>%
    rename(id = ID)  # new_name = old_name

  # Add regimes ---------------------------------------------
  dd <- dd %>% inner_join(df.regimes, by = c("branching_group"))

  # Identify thinning values -------------------------------
  dd <- classifyTHIN(dd)

  # Calculate wind damage risk -----------------------------
  risk_vector <- formatWindRiskTable(dd)

  # Add risk values to original table ----------------------
  dd <- dd %>%
    cbind(windRisk = as.vector(risk_vector))

  # Export final csv ---------------------------------------
  print(paste(inPath, out_folder, out_name, sep = "/"))
  data.table::fwrite(dd, paste(inPath, out_folder, out_name, sep = "/"))

 rm(dd)

}


 # !!!!!!!


#----------------------------------
# Example ------------------------
# ---------------------------------


readFiles <- function(df_name, ...) {
  
  
  # Read simulated data 
  df <- data.table::fread(paste(inPath, in_folder, in_name, sep = "/"),  # 
                          data.table=TRUE)
  
  return(df)
}

# Create a unique ID
getUniqueID_sim <- function(df, ...) {
   
  # ========================================== #
  #      Create unique ID for simulated data   # --------------------------------
  #            to merge them with XY           # --------------------------------
  # ========================================== #
  
  # Steps: 
  # 1. 'Zero paddling': add zeros to the beginning of 'id' , 
  #     eg. fill in 00000XX values to have always 8 characters starting with 0
  # 2. rename grid cells ('k3') into consecutive numbers ('1'...)
  # 3. Add numeric grid indication at the beginning of the 8 digit paddled 'id'
  # 4. voila! done
  #df <- my_ls[[1]]
    
  df <- 
    df %>% 
    mutate(zero_id = formatC(id, 
                             width = 8, 
                             format = "d", 
                             flag = "0")) %>% 
    mutate(cell = str_sub(name,-2))  %>% 
    mutate(nb = case_when( 
      cell == "k3" ~ "1",
      cell == "k4" ~ "2",
      cell == "l2" ~ "3",
      cell == "l3" ~ "4",
      cell == "l4" ~ "5",
      cell == "l5" ~ "6",
      cell == "m3" ~ "7",
      cell == "m4" ~ "8",
      cell == "m5" ~ "9",
      cell == "n3" ~ "10",
      cell == "n4" ~ "11",
      cell == "n5" ~ "12",
      cell == "n6" ~ "13",
      cell == "p3" ~ "14",
      cell == "p4" ~ "15",
      cell == "p5" ~ "16",
      cell == "p6" ~ "17",
      cell == "q3" ~ "18",
      cell == "q4" ~ "19",
      cell == "q5" ~ "20",
      cell == "r4" ~ "21",
      cell == "r5" ~ "22",
      cell == "s4" ~ "23",
      cell == "s5" ~ "24",
      cell == "t4" ~ "25",
      cell == "t5" ~ "26",
      cell == "u4" ~ "27",
      cell == "u5" ~ "28",
      cell == "v3" ~ "29",
      cell == "v4" ~ "30",
      cell == "v5" ~ "31",
      cell == "w3" ~ "32",
      cell == "w4" ~ "33",
      cell == "w5" ~ "34",
      cell == "x4" ~ "35",
      cell == "x5" ~ "36")) %>% 
    mutate(ID = paste0(nb, zero_id)) %>% 
    mutate(ID = as.character(ID))
  
 return(df)
}

# Apply the uniqueID function over the list
dd<-getUniqueID_sim(df)


# Join raster values -------------------------------------------------
dd <- dd %>%  inner_join(df.raster, 
                   by = c('ID'))


# Remove excessive columns -------------------------------------------
dd <- dd %>%
   dplyr::select(-c(id, zero_id, nb, 
                    X_10, OBJECTID, 
                    Stand)) %>%
                   rename(id = ID)  # new_name = old_name

# Add regimes ---------------------------------------------
dd <- dd %>% inner_join(df.regimes, by = c("branching_group"))

# Identify thinning values -------------------------------
dd <- classifyTHIN(dd)

# Calculate wind damage risk -----------------------------
risk_vector <- formatWindRiskTable(dd)

# Add risk values to original table ----------------------
dd <- dd %>%
  cbind(windRisk = as.vector(risk_vector))


# Export final csv ---------------------------------------
data.table::fwrite(dd, paste(inPath, out_folder, out_name, sep = "/"))


