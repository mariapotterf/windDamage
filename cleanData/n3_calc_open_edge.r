
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
df<- data.table::fread("C:/MyTemp/myGitLab/windDamage/output/df_sim_opt.csv",
                       data.table=FALSE, 
                       stringsAsFactors = FALSE)

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

# write the table
fwrite(merged.df, "C:/MyTemp/myGitLab/windDamage/output/df_landscape_open_edge.csv")

