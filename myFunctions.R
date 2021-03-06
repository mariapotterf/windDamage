

# My functions:

# Find neighbors by geometry (): find_nbrs_geom
# define open edge by neighbors: open_edge_by_nbrs

# combine neighbors search and open_edge in one fucntion: on of
# findOpenEdge_sf


# Define own palette, color blind
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# Calculate NPV

calculate_NPV <- function(df, ...) {
  
  # get indication of the climChange scenario
  df <- df  %>% 
    mutate(climChange = case_when(
      grepl("RCP0", name)  ~ "REF",
      grepl("RCP45", name) ~ "RCP45",
      grepl("RCP85", name) ~ "RCP85"))
  
  # Calculate discounted income and PV 
  df$disc_income = df$cash_flow/(1.03^(df$year-2016))
  df$disc_PV     = df$PV/(1.03^(df$year-2016))
  
  # Replace the disc_PV value by 0 if less year < 2111
  df1 <- df %>% 
    mutate(disc_PV = case_when(year < 2111 ~ 0,
                               year >= 2111 ~ disc_PV))
  # Calculate sums 
  df1 <- df1 %>% 
    group_by(id, branching_group, climChange) %>% #branching_group
    summarize(sum_dist_PV     = sum(disc_PV, na.rm = T),
              sum_dist_income = sum(disc_income, na.rm = T),
              NPV = sum_dist_PV + sum_dist_income)
  
  return(df1) 
}





# ------------------------------------------
#  Add initial year based on SA values
# ------------------------------------------

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
  
  # rename the column and add data by col names
  df.out <-
    df.sa %>% 
    crossing(regime_v) %>% # merge dataframe with vector of regime names 
    rename(regime = regime_v) %>%
    dplyr::bind_rows(df)  # merge data to new column, colums sort out by names
  
  return(df.out)
}





# -------------------------------
# Find neighbors by geometry
# -------------------------------

# Get the geometry and neighbors of stands
# export ad a list of dataframes
# function loops throught stands

find_nbrs_geom <- function(sf, ...) {
  
  require(sf)
  require(dplyr)
  
  # create output to store the results
  nbrs.df.ls <- list()
  
  for (i in 1:nrow(sf)) {
    #sf <- df.geom
   # i = 5
    one  = sf[i, ]
    left = sf[-i,]
    
    # Create buffer and intersectb buffer with neighbors: evalues if any are left?
    buff = st_buffer(one, 40) # distance, 5*16 = 80, 40 is radius
    
    # Subset the polygons that overlaps with the buffer
    nbrs.buff <- left[st_intersects(buff, left, sparse =  FALSE),]
    
    # Add `one` to `neighbors` and dissolve (union) inner boundaries  
    u <- st_union(rbind(nbrs.buff, one))
    
    # Erase existing stands from the buffer
    int.buff.one = st_difference(st_geometry(buff), st_geometry(u)) 
    
    # Calculate area of intersected data
    int.buff.one.area = st_area(int.buff.one)
    gap.area <- as.numeric(int.buff.one.area)
    
    # Create output dataframe
    central   <- one$id
    nbrs      <- if(nrow(nbrs.buff) == 0)  (0) else (nbrs.buff$id) 
    open_area <- if (length(gap.area) == 0) (0) else (round(gap.area,1))
    
    # Create dataframes
    nbrs.df <- data.frame(central, nbrs, open_area)
    
    # add datyaframe to output dataframe list
    nbrs.df.ls[[i]] <- nbrs.df
    
  }
  return(nbrs.df.ls)
}


# ----------------------------------------------
# Define open_edge based on neighbors list
# ----------------------------------------------


# Use couple central-neighbors to compare tree heights or set open_edge = T

# this lopps over teh couples oine - neighbors 
# and estimate the open_edge value for each stand
# output is a matrix
open_edge_by_nbrs <- function(nbrs, df.sim,...) {
  require(sf)
  require(dplyr)
  
  # Create list to store the outputs
  nbrs_edge <- list()
  
  #nbrs <- nbrs[[5]]
  #df.sim <- land.ls[[5]]
  
  for (i in seq_along(nbrs)) {
  #  i = 1
    # get ids of the central stand and neighbors
    central_id <- as.character(unique(nbrs[[i]]$central))
    nbrs_id    <- unique(nbrs[[i]]$nbrs)
    
    # Get the stand height from simulated data for central stand and 
    # neighbors 
    central_H = rep(subset(df.sim, id %in% central_id)$H_dom, 
                    length(nbrs_id))
    nbrs_H    = subset(df.sim, id %in% nbrs_id)$H_dom
    
    # Evaluate if the stand has open gap near by
    if (unique(nbrs[[i]]$open_area) > 16*16) {
      nbrs_edge[[i]] <- c(central_id,"TRUE")
      
    } else {
      # Get the differences between the neighbouring stands
      difference = central_H - nbrs_H
      
      # if any difference is more then 5
      if(any(difference > 5, na.rm = T)) {
        
        nbrs_edge[[i]] <- c(central_id,"TRUE")

        
        # No forest edge    
      } else {
        nbrs_edge[[i]] <- c(central_id,"FALSE")

      }
    }
  }
  # Merge into single matrix
  df<- as.data.frame(do.call("rbind", nbrs_edge))
  names(df) <- c("id", "open_edge")
  return(df)
}






# ------------------------------------------------
# Format wind risk Table
# ------------------------------------------------
# function to adjust table ---------------
# gegenartes the same open_edge values!!!

# 
formatWindRiskTable <- function(df, ...) {
  
  library(dplyr)
  
  #df <- dd
  
  # subset only important columns for wind risk model
  df.sub <- df %>% 
    dplyr::select(id, year, Age, SC,
                  SOIL_CLASS, PEAT, MAIN_SP, 
                  H_dom, windSpeed, avgTemp, since_thin, regime) # exclude regime?
  
  
  #   df<- df.cc45_2
  # add new column
  df.all <- df.sub %>% mutate(open_edge = "TRUE")

  # Recalssify values
  # # Reclassify values:
  df.all<-
    df.all %>% 
    mutate(PEAT.v = case_when(PEAT == 0 ~ "mineral soil",
                              PEAT == 1 ~ "peat"))  %>%
    mutate(SC.v = case_when(SC %in% 1:3 ~ "fertile",
                            SC %in% 4:7 ~ "poor")) %>%                 # added 7? COMPLETE SOIL CALSS to get mineral coarse/fine??
    mutate(soil_depth_less30 = ifelse(SOIL_CLASS == 1, TRUE,FALSE)) %>% 
    dplyr::select(-c(PEAT, SC))
  
  # continue the table to split it in chunks
  df.all <- df.all %>%
    mutate(soilType = case_when(SOIL_CLASS == 0 ~ "organic",
                                SOIL_CLASS %in% 1:4 ~ "mineral coarse",
                                SOIL_CLASS %in% 5:7 ~ "mineral fine")) %>% 
    mutate(species = case_when(MAIN_SP == 1 ~ "pine",
                               MAIN_SP == 2 ~ "spruce",
                               TRUE ~ "other")) %>% 
    mutate(H_dom = replace_na(H_dom, 0.0001)) %>%  # no possible to get log(0) or log(NA)  
    mutate(H_dom = H_dom * 10) %>%           # Susanne values are in dm instead of meters
    mutate_if(is.character, as.factor)  %>%    # convert all characters to factor
    dplyr::select(-c(SOIL_CLASS,MAIN_SP ))
    #head()
  
  # try if I can calculate the preidtcion based on subset data
  # to make sure that data did not get randomly reorganized
  df.all <-
    df.all %>% 
    dplyr::rename(time_thinning   = since_thin,             # new.name = old.name
                  soilDepthLess30 = soil_depth_less30,
                  siteFertility   = SC.v,
                  tempSum         = avgTemp)
  
  # Correct order the factor levels 
  # for categoric variables  ------------------------------------
  df.all<- df.all %>% 
    mutate(species = factor(species, levels = c("pine", 
                                               "spruce", 
                                               "other")))
  df.all<- df.all %>% 
    mutate(time_thinning = factor(time_thinning, 
                                  levels = c("0-5", 
                                             "6-10", 
                                             ">10"))) 
  df.all<- df.all %>% 
    mutate(open_edge = factor(open_edge,
                              levels = c("FALSE", 
                                       "TRUE")))
  
  df.all<- df.all %>% 
    mutate(soilType= factor(soilType,
                                    levels = c("mineral coarse", 
                                               "mineral fine",
                                               "organic")))
  df.all<- df.all %>% 
    mutate(soilDepthLess30 = factor(soilDepthLess30, 
                                    levels = c("FALSE", 
                                               "TRUE")))
  df.all<- df.all %>% 
    mutate(siteFertility = factor(siteFertility,
                                    levels = c("poor", 
                                               "fertile")))
  df.all<- df.all %>% 
    mutate(tempSum = tempSum/100)   # according to Susane
  
  # Calculate wind risk ----------------------------
  windRisk.v = calculate_windRisk(df.all)
  
 # df.all <- df.all %>% 
  #  cbind(windRisk = windRisk.v)
  
  
  #df.all %>% 
   # filter(is.na(windRisk)) %>% 
    #select()
  
  return(windRisk.v)
}




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




# ------------------------------------------------
# Classify THIN
# ------------------------------------------------

classifyTHIN <- function(df, ...) {
  library(dplyr)
  df.out <- 
    df %>% 
    mutate(THIN = na_if(THIN, 0))  %>% 
    mutate(THIN2 = substring(THIN,0,4)) %>%  # keep the first 4 characters from CCF regimes, datum in format "2016-04-16" -> to "2016"
    group_by(id, regime) %>% 
    mutate(THIN_filled_lagged = lag(THIN2)) %>%
    mutate(THIN_filled_lagged = as.numeric(THIN_filled_lagged)) %>%
    tidyr::fill(THIN_filled_lagged) %>% 
    mutate(difference = year - THIN_filled_lagged) %>% 
    mutate(since_thin = case_when(is.na(difference) | difference < 0 ~ ">10",
                                  difference %in% c(0:5) ~ "0-5",
                                  difference %in% c(6:10) ~ "6-10",
                                  difference > 10 ~ ">10")) 
  return(df.out)
}







# --------------------------
# Subset by polygon (watershed)
# --------------------------

# watershged is a shp
# forest is a stand geometry from GPKG

subsetByPolygon <- function(watershed, forest) {
  
  library(rgeos)
  
  # subset the stands within the watershed
  # do not touch the watershed border
  outForest = forest[which(rgeos::gContains(watershed,
                                            forest,
                                            byid=TRUE)),]
  
  return(outForest)
}




# Plot line details -------------------------------------------------------

plot_line_details <- function() {
  list(
    geom_line(    size = 0.9),
    facet_wrap(.~Management), 
    ggtitle(""),
    scale_linetype_manual(values = c( "dotted", 
                                      "solid",  
                                      'dashed')),
    scale_color_manual(values = cbp1),
    scale_fill_manual(values = cbp1),
    labs(shape = "Scenario",
         color = "Scenario",
         linetype = "Scenario",
         fill = "Scenario"),
    theme(axis.title  = element_text(size = 10, face="plain", family = "sans"),
          axis.text.x = element_text(angle = 90, vjust = 0.5, face="plain", size = 9, family = "sans"),
          axis.text.y = element_text(face="plain", size = 9, family = "sans"),
          legend.position = "right",
          strip.background =element_rect(fill="white", 
                                         color = NA))
  )
}


plot_line_details2 <- function() {
  list(
    geom_line(    size = 0.9),
  #  facet_wrap(.~Management), 
    ggtitle(""),
    scale_linetype_manual(values = c( "dotted", 
                                      "solid",  
                                      'dashed')),
    scale_color_manual(values = cbp1),
    scale_fill_manual(values = cbp1),
    labs(shape = "Scenario",
         color = "Scenario",
         linetype = "Scenario",
         fill = "Scenario"),
    theme(axis.title  = element_text(size = 10, face="plain", family = "sans"),
          axis.text.x = element_text(angle = 90, vjust = 0.5, face="plain", size = 9, family = "sans"),
          axis.text.y = element_text(face="plain", size = 9, family = "sans"),
          legend.position = "right",
          strip.background =element_rect(fill="white", 
                                         color = NA))
  )
}





# -------------------------
# Correct read optimal scenarios
# -------------------------
# Make a function to read the optimal data correctly and 
# to filter just the prevailing forest management by stand
# function also keep just one stand with the gighest proportion of management
readOptimal <- function(df.path, ...) {
  
  require(dplyr)
  # read individual lines
  txt <-  readLines(df.path)
  
  # Read table by replacing characters
  optim <-read.table(text = gsub("[()\",\t]", " ", txt), 
                     stringsAsFactors = F)
  # Rename the columns of columns names
  names(optim) <- c("id", "regime", "proportion")
  print(length(unique(optim$id)))
  
  # keep only the largest proportion by the stand
  optim.max<-
    optim %>%
    dplyr::group_by(id) %>%
    filter(proportion == max(proportion))
  
  print(length(unique(optim.max$id)))
  # return only data.frame
  opt.df<- data.frame(optim.max, 
                      stringsAsFactors = FALSE)
  # Return the dataframe with correct columns and filtered 
  # stands having one regime by stand
  return(opt.df)
  
}











# LIst my functions:
findOpenEdge_sf <- function(sf, H_dom, distance = 40, pixel.width = 16, ...) {
  
  # loop through the dataframe
  sf$open_edge <- FALSE
  
  for (i in 1:nrow(sf)) {
    
    # define stands and leftover forest
    one  = sf[i, ]
    left = sf[-i,]
    
    # Create buffer and intersectb buffer with neighbors: evalues if any are left?
    buff = st_buffer(one, distance) # distance
    
    # Subset the polygons that overlaps with the buffer
    nbrs.buff <- left[st_intersects(buff, left, sparse =  FALSE),]
    
    
    # If conditions to determine if the forest has open edge or not
    if (nrow(nbrs.buff) == 0) {
      sf[i,]$open_edge <- TRUE  
      
    } else {  # neighbors are smaller than the stands
      
      # Compare the height of the stands: 
      height.one  = rep(one$H_dom, nrow(nbrs.buff))
      height.nbrs = nbrs.buff$H_dom
      
      # Get the differences between the neighbouring stands
      difference = height.one - height.nbrs
      
      # compare here the tree heights of stands
      if(any(difference > 5)) {
        sf[i,]$open_edge <- TRUE
        
        # Check if there is a big gap in neighborhood    
      } else {                     
        
        # Get the difference between two shapefiles???
        # Add `one` to `neighbors` and dissolve (union) inner boundaries  
        u <- st_union(rbind(nbrs.buff, one))
        
        # Erase existing stands from the buffer
        int.buff.one = st_difference(st_geometry(buff), st_geometry(u)) 
        
        # check if gaps exists 
        if (length(int.buff.one) > 0 ) {
          
          # Calculate area of intersected data
          int.buff.one.area = st_area(int.buff.one)
          
          if (as.numeric(int.buff.one.area) > pixel.width^2)  {
            sf[i,]$open_edge <- TRUE
          }
        }
      }
    }
  }
  return(sf) 
} 




# ===================================
# Calculate daily temperatures
# from raster data
#--------------------------------------


# Make a function
calculateDailyMeans <- function(sf, gridNames, ...) {
  
  # Inputs:
  # sf = simple feature class
  # gridnames = list of the bricks in working directory
  
  # Get the centroid from polygons:
  # change the projection to raster CSC
  sf.t <- st_transform(sf, st_crs(r.grds[[1]]))
  
  # calculate convex hull
  border = st_union(sf.t, by_feature = FALSE)
  
  # Calculate centroids
  centroids <- st_centroid(border, byid = TRUE)
  
  # process rasters to extract the rastervalues
  r.grds <- lapply(gridNames, brick)
  
  # Get the mean temparature value per day per stand (stand contains multiple vectors)
  ls.means<- lapply(r.grds, function(r) {
    raster::extract(r, 
                    as_Spatial(centroids))  # fun=mean,na.rm=TRUE, df=TRUE
  })
  
  # Remove the base temperatture = 5 c from each daily mean
  ls.diff <- lapply(ls.means, function(df) df - 5)
  
  # calculate the difference with base value
  ls.posit <- lapply(ls.diff, function(df) {
    df[df<0] <- 0
    return(df)
  })
  
  # Sum up the positive difference value by year
  ls.sum <- lapply(ls.posit, rowSums)
  
  
  # Calculate the means for each row in a DF list
  # add it as a new attribute to stands
  dailyMean  <- rowMeans(do.call(cbind, ls.sum))
  return(dailyMean)
  
}




# Wind risk

# =========================
# fake data
# Check if the model works???
# ---------------------

calculate_windRisk <- function(df, ...) {
  
  # Create fake data --------------------------------
  
  set.seed(42)
  
  row.num = 80
  
  species         <- factor(rep(c("pine", "spruce", "other"),
                                each = row.num), 
                            levels = c("pine", "spruce", "other"))
  H_dom          <- c(runif(row.num, min = 10, max = 200),
                      runif(row.num, min = 0, max = 100),
                      runif(row.num, min = 30, max = 150))
  time_thinning   <-  factor(sample(c("0-5", "6-10", ">10"),
                                    length(species), replace = TRUE), 
                             levels = c("0-5", "6-10", ">10"))
  windSpeed       <- runif(length(species), min = 10, max = 30)
  open_edge      <- ifelse(rbinom(length(species), 1, 0.9), "FALSE", "TRUE") 
  soilType        <- sample(c("mineral coarse", "mineral fine",
                              "organic"),
                            length(species), replace = TRUE)
  soilDepthLess30 <- ifelse(rbinom(length(species), 1, 0.5),
                            "FALSE","TRUE")
  siteFertility   <- factor(sample(c("poor", "fertile"),
                                   length(species),
                                   replace = TRUE),
                            levels = c("poor", "fertile"))
  tempSum         <- runif(length(species), min = 6, max = 16)
  wind_damage     <- rbinom(length(species), 1, 0.4)
  
  
  # put data together
  df.fake<-data.frame(species, 
                      H_dom,
                      time_thinning,
                      windSpeed,
                      open_edge,
                      soilType,
                      soilDepthLess30,
                      siteFertility,
                      tempSum,
                      wind_damage)
  
  
  
  # get suvanto coefficients
  # replace them in the fake model
  # calculate predicted values * ar the same as manual values?
  
  fake.m <- glm(formula = wind_damage ~ species + log(H_dom)  +  
                  time_thinning + 
                  log(windSpeed) + open_edge + soilType +
                  soilDepthLess30 + 
                  siteFertility + tempSum +
                  log(H_dom):species, 
                data = df.fake, 
                family = "binomial") # family = binomial(link = "logit")
  
  
  
  # ----------------------------
  # Get Suvanto's coefficients
  # ---------------------------
  # Suvanto's coefficients (more accurate from Susanne code): 20 digits
  intercept                     = - 14.690374506245104769
  b1.spec.spruce                = - 8.494158565180855547
  b2.spec.other                 = - 9.314355152502169943
  b3.log.H_dom                  = + 1.660897636823469137   # log
  b4.last_thinning.6.10         = - 0.298186071853962231
  b5.last_thinning.over.10      = - 0.844019963540904472
  b6.log.wind                   = + 0.748957880201017501   # log
  b7.open_edge_border           = + 0.310378186345018792
  b8.soil_min.fine              = - 0.355681075669793900
  b9.soil_organic               = - 0.216004202249671151
  b10.soil_depth.less.30cm      = + 0.214100256449853671
  b11.site_fertility            = - 0.425096042510456240
  b12.temperature_sum           = + 0.095854694562656148
  b13.spec.spruce.X.log.H_dom   = + 1.634359050870280550 
  b14.spec.other.X.log.H_dom    = + 1.624775941830151726
  
  
  # Put coefficients in a vector, to replace the coefficients in a formula
  suvantoCoeffs<-c(intercept, 
                   b1.spec.spruce,
                   b2.spec.other,
                   b3.log.H_dom,
                   b4.last_thinning.6.10,
                   b5.last_thinning.over.10,
                   b6.log.wind,
                   b7.open_edge_border,
                   b8.soil_min.fine,
                   b9.soil_organic,
                   b10.soil_depth.less.30cm,
                   b11.site_fertility,
                   b12.temperature_sum,
                   b13.spec.spruce.X.log.H_dom,
                   b14.spec.other.X.log.H_dom)
  
  
  # -------------------------------
  # Replace the dummy coefficients 
  #    by the real ones
  # -------------------------------
  # First, copy the model
  windRisk.m <- fake.m
  
  
  # replace coefficients:
  windRisk.m$coefficients<-suvantoCoeffs
  
  
  # Normally predicted values - converted from logit values
  predicted<-predict.glm(windRisk.m,
                            df, 
                            type="response")
  return(predicted)
  
}



# -------------------------------------
# Manual calculate wind risk
# -------------------------------------


# -------------------------------
# Manually calculate y
# require steps:
#    - convert cegorical variables to binary
#    - correctly order teh columns & create interactions
#    - multiply the columns by coefficients
#    - sum up the rows
#    - convert sum(row) values to logit to get the probability estimation
# -------------------------------


# Crtl+Shift+C = un/comment

# # convert my categorial variables into binary classes
# # create vector of colnames with categorical variables
# categVars <- c("species", 
#                "time_thinning", 
#                "open_edge",
#                "soilType", 
#                "soilDepthLess30",
#                "siteFertility")
# 
# 
# library(fastDummies)
# 
# df.fake.bin <- fastDummies::dummy_cols(df.fake,
#                                        select_columns = categVars, # only categorical
#                                        remove_first_dummy = TRUE)  # remove reference category
# 
# # remove the original variables and observed value of damages
# df.fake.bin<-df.fake.bin[ , !(names(df.fake.bin) %in% c(categVars,
#                                                         "wind_damage"))]
# 
# 
# # complete the dataframe by interactions 
# # to have the same amount of columns as number of coefficients
# # add logarithm in a formula
# # add columnf for intersectp => fill with 1
# df.fake.bin$interc <- 1
# 
# df.fake.bin$log_H_dom  <- log(df.fake.bin$H_dom)
# df.fake.bin$log_Wspeed <- log(df.fake.bin$windSpeed)
# 
# # add interactions
# df.fake.bin$spec.spruce.X.log.H_dom <- df.fake.bin$species_spruce * df.fake.bin$log_H_dom
# df.fake.bin$spec.other.X.log.H_dom  <- df.fake.bin$species_other  * df.fake.bin$log_H_dom
# 
# 
# # to put it in correct order
# colnames.ordered<-c("interc",
#                     "species_spruce",
#                     "species_other",
#                     "log_H_dom",
#                     "time_thinning_6-10", 
#                     "time_thinning_>10",
#                     "log_Wspeed",
#                     "open_edge_TRUE",
#                     "soilType_mineral fine",
#                     "soilType_organic",
#                     "soilDepthLess30_TRUE",
#                     "siteFertility_fertile",
#                     "tempSum",
#                     "spec.spruce.X.log.H_dom",
#                     "spec.other.X.log.H_dom" )
# 
# # order the dataframe to corresponds columnwise to coefficients
# # keep only specified columns
# df.fake.ord<-df.fake.bin[colnames.ordered]
# 
# 
# # Multiply the dataframe columns by vector of coefficients
# 
# # calculate partial df.fake 
# # the final column need to be summed up
# part.df.fake <- sweep(df.fake.ord, 2, suvantoCoeffs, "*")
# 
# # sum by rows and add intercept value
# df.fake.ord$pred.manual <- logit2prob(rowSums(part.df.fake))
# 
# # Convert logit to probabilities
# logit2prob <- function(logit){
#   odds <- exp(logit)
#   prob <- odds / (1 + odds)
#   return(prob)
# }
# 
# 
# 
# # sum by rows and add intercept value
# df.fake.ord$pred.manual <- logit2prob(rowSums(part.df.fake))
# 
# 
# # Calculate teh probability based on model: 
# # the manual calculation (with logits) and this should be the same!!
# # Calcutate probability values given the example data
# df.fake.ord$predict.wind <- predict.glm(windRisk.m, 
#                                         df.fake, 
#                                         type = "response")
# 
# 
# # Compare manual probability estimation and 
# # calculated using the model
# df.fake.ord$diff<- round(df.fake.ord$pred.manual,6) - round(df.fake.ord$predict.wind,6)
# 
# range(df.fake.ord$diff)
# 
# # ----------------------
# # conclusions! easier to recreate model and just replace the coefficeint for it, 
# # no need to convert to binary, get logits etlc...
# # need to check it on one more data??







