
# ------------------------
# Calculate open edge
# ------------------------

# input:
#    - stand geometry
#    - simulated optimal data including H_dom
# output: 
#    - df wit indication of standid, scenario, H_dom...
# to be able to merge it back to simulated optimal data (if those woould change)

rm(list = ls())


library(sf)
library(dplyr)
require(data.table)
library(tidyr)


stands.remove <- c(13243875,
                   13243879,
                   13243881)



# read optimal simulated data
df<- data.table::fread("C:/MyTemp/myGitLab/windDamage/output/even_flow/df_sim_opt.csv",
                       data.table=FALSE, 
                       stringsAsFactors = FALSE)



#df<- data.table::fread("C:/MyTemp/myGitLab/windDamage/output/df_sim_opt.csv",
 #                      data.table=FALSE, 
  #                     stringsAsFactors = FALSE)



df <- subset(df, !id %in% stands.remove)


keep_id <- unique(df$id)

# Read stand geometry - subset twice
# -----------------------------
df.geom <- st_read("C:/MyTemp/avohaakut_db/14.534/14.534/mvj_14.534.shp")
df.geom <- subset(df.geom, select = c("KUVIO_ID"))
names(df.geom) <- c("id", "geometry")
# df.geom$area <- st_area(df.geom)
df.geom <- subset(df.geom, !id %in% stands.remove)
df.geom <- subset(df.geom, id %in% keep_id)



# Replace H_dom if NA = 0 --------
df <- df %>% 
  dplyr::mutate(H_dom  = replace_na(H_dom, 0)) %>% 
  mutate(scenario = gsub("./Bundles_2_nocow_INCOME_MANAGE_price_three_0_0_1_1", '', scenario)) %>% 
  mutate(landscape = gsub("./Bundles_2_nocow_INCOME_MANAGE_price_three_0_0_1_1", '', landscape)) #%>% 
  





# Calculate open_edges
# ---------------------------------
source("C:/MyTemp/myGitLab/windDamage/myFunctions.r")

# Split dataframe into dataframe list
#land.ls.all <- df %>% 
 # dplyr::select(id, year, scenario, avohaakut, landscape, H_dom) %>% 
 # group_by(landscape) %>% 
 # group_split()

#object.size(land.ls.all)

land.ls <- df %>% 
  dplyr::select(id, landscape, H_dom) %>% 
  group_by(landscape) %>% 
  group_split()


# CHeck how many stands I have stands I have at each landscape? are stands id consistent over time for scenario???
# Get number of of unique id by landscape
for (i in seq_along(land.ls)) {print(length(unique(land.ls[[i]]$id)))}


# Is the number of stands consisent by scenarios?
df %>% 
  group_by(scenario, year) %>% 
  tally() %>% 
  print(n = 1500)


# Get number of scenarios
length(unique(df$scenario))





# object.size(land.ls.sel) # object with subesetted columns is 12 times smalled than with all columns!!!
# ---------------------------------------------------
# Calculate the pairs of neighbors:
# calculate open_edge
# ---------------------------------------------------
nbrs <- find_nbrs_geom(df.geom)


# calculate on one landscape
open_edge.ls <- lapply(land.ls, function(df) open_edge_by_nbrs(nbrs, df))

# merge open edge data with the landscapes and ids 
merged.ls <- Map(merge, land.ls, open_edge.ls, by="id")

# Merge partial dfs into one
merged.df <- do.call(rbind, merged.ls)


# CHeck the number of stands by landscape
merged.df %>% 
  group_by(landscape) %>% 
  tally() %>% 
  print(n = 1500)

# 1472, seems correct


# write the table
fwrite(merged.df, "C:/MyTemp/myGitLab/windDamage/output/even_flow/df_landscape_open_edge.csv")



# Create a table to record landscape type, centrail id, central_h, neighbors and neighbors_H

# ---------------
# continue form here to get the height differences between stands
df.sim = land.ls[[5]]
nbrs[[2]]



# loop over both features and get the central id
get_nbrs_H <- function(nbrs, df, ...) {
  
}



get_nbrs_H <- function(nbrs, 
                       df.sim,...) {
  require(sf)
  require(dplyr)
  
  # Make a list to store outputs
  nbrs_ls <- list()
  
  
  for (i in seq_along(nbrs)) {
  #  i=1299
   # i = 100
    print(i)
    # get ids of the central stand and neighbors
    central_id <- as.character(unique(nbrs[[i]]$central))
    # Get neighbors
    nbrs_id    <- unique(nbrs[[i]]$nbrs)
    
    # Define variables for output df:
    landscape = unique(df.sim$landscape)
    nn        = length(nbrs_id)
    central_H = rep(subset(df.sim, id %in% central_id)$H_dom, nn)
    
     
    # What if no neighbors are there?
    if (nbrs_id == 0) {
      #print("no neigbors")

      # Get table
      nbrs_ls[[i]] <- data.frame(landscape = rep(landscape, nn),
                                 central_id = rep(central_id, nn),
                                 central_H = central_H,
                                 nbrs_id = NA,
                                 nbrs_H = NA)
    } else {
      nbrs_H    = subset(df.sim, id %in% nbrs_id)$H_dom

      # Get the stand height from simulated data for central stand and
      # neighbors
      nbrs_ls[[i]] <- data.frame(landscape = rep(landscape, nn),
                                 central_id = rep(central_id, nn),
                                 central_H = central_H,
                                 nbrs_id = nbrs_id,
                                 nbrs_H = nbrs_H)

    }
  
   
  }
  df<- as.data.frame(do.call("rbind", nbrs_ls))
  #  names(df) <- c("id", "open_edge")
  return(df)
 
}



# the function is super slow, likely due to the warning. can be updated later!










# run example on one stand
#ddd <- get_nbrs_H(nbrs, df.sim)


# example ifelse
a = c(5,9,2,9)
ifelse(a == 2, 'TRUE', "False")





# get difference between neighbors on one landscape ----------------------
nbrs.ls <- lapply(land.ls, function(df) get_nbrs_H(nbrs, df))
  
# Merge partial dfs into one
merged.nbrs.df <- do.call(rbind, nbrs.ls)
  
# write the table
fwrite(merged.nbrs.df, "C:/MyTemp/myGitLab/windDamage/output/even_flow/df_nbrs_diff.csv")
  
  
  
  

