
# ----------------------------------
# Recontruct optimal landscape
# ----------------------------------

# filter simulated data (with correct names from optmal scenarios)
# to create optimal lanscape: 58 regimes, XX landscapes

# Read .csv files of optimal solutions
# if there is not 1.0 solution: 
# keep only stand that has higher proportion of the regime
# Based on the scenarios, subset the individual simulated stands
# ----------------------------------
rm(list = ls())

library(sf)

# Read corrected simulated names:
df.sim<- data.table::fread("C:/MyTemp/avohaakut_db/analyzed/simulated_AVK_regimes.csv")

# Read stand geometry
# -----------------------------
df.geom <- st_read("C:/MyTemp/avohaakut_db/14.534/14.534/mvj_14.534.shp")
df.geom <- subset(df.geom, select = c("KUVIO_ID"))
names(df.geom) <- c("standid", "geometry")
df.geom$area <- st_area(df.geom)

stands.remove <- c(13243875,
                   13243879,
                   13243881)

# Check if those stands have 0 ha area??
subset(df.geom, KUVIO_ID %in% stands.remove)$area

# In my data they do not have NULL geometry, but I will remove the stands from 
# simulated, optimal, and geometry data
# ------------------------------------------

# The geometry has more stands as simulated data!!
length(unique(df.geom$standid)) # 1485 !!! I need to subset the simulated data

# need to subset it
df.geom <- subset(df.geom, standid %in% unique(df.sim$id))

# -----------------------------
# Read optimal solution:
# -----------------------------

source("C:/MyTemp/myGitLab/windDamage/myFunctions.r")


# Read all optimal solutions to 
# see applied regimes?? 
# -------------------------------
setwd("C:/MyTemp/avohaakut_db/solutions")
df.optim = list.files(pattern=".csv$",
                      full.names = TRUE) # ends with .csv$, \\. matches .

df.names <- gsub("./Bundles_2_nocow_NPV_MANAGE_price_three_0_0_1_1_", "", df.optim)
df.names <- gsub(".csv", "", df.names)

# Read all dataframes in a loop
df.opt.ls = lapply(df.optim, readOptimal)

# Add indication of the name:
df.opt.ls <- mapply(cbind, 
                    df.opt.ls, 
                    df.names, 
                    SIMPLIFY = FALSE)

opt.df.all <- do.call(rbind, df.opt.ls)

# ----------------------------
# Check if I have same data simulated and optimized
# ----------------------------
# seems that id are the same among the simulated and optimal data
opt.ids <- unique(opt.df.all$id)
sim.ids <- unique(df.sim$id)

# Compare teh stand id between optimal data and simulated data
setdiff(opt.ids, sim.ids)

# Check if tehy all have the 1475 stands???
lapply(df.opt.ls, function(df) length(unique(df$id)))
# YES

# How does teh table looks like?
# lapply(df.opt.ls, function(df) table(df$regime))

# Subset the original table as one by one???
# Subset individual landscapes  = optimal from simulated df (all in one table)

df.sim.opt <- lapply(df.opt.ls, 
                     # Semi join subset by the stand id and by specific regime 
                     # simulated in single optimal scenario 
                     function(df.optim)  {
                       df.sim %>%
                         semi_join(df.optim,
                                   by = c("id" = "id",
                                          "avohaakut" = "regime")) })


# landscapes  43-63 have only 1474 stands??? 
lapply(df.sim.opt, function(df) length(unique(df$id)))

# SEmi join does not work as expected
a.63 <-  df.sim %>%
  semi_join(df.opt.ls[[63]],
            by = c("id" = "id",
                   "avohaakut" = "regime"))


a.10 <-  df.sim %>%
  semi_join(df.opt.ls[[10]],
            by = c("id" = "id",
                   "avohaakut" = "regime"))
length(unique(a.10$id))

# compare two vectors to find missing stand: 
setdiff(unique(a.10$id), unique(a.63$id)) 
# missing stand is 13243875 - however, it is included in 
# optmial scenarios

df.names[43:63] # - all are RF

#[1] "CCF_1_10" "CCF_2_10" "CCF_3_10" "CCF_4_10" "CCF_1_15" "CCF_2_15"
#[7] "CCF_3_15" "CCF_4_15" "CCF_1_20" "CCF_2_20" "CCF_3_20" "CCF_4_20"
#[13] "CCF_1_25" "CCF_2_25" "CCF_3_25" "CCF_4_25" "CCF_1_30" "CCF_2_30"
#[19] "CCF_3_30" "CCF_4_30" "CCF_1_35" "CCF_2_35" "CCF_3_35" "CCF_4_35"
#[25] "CCF_1_40" "CCF_2_40" "CCF_3_40" "CCF_4_40" "CCF_1_45" "CCF_2_45"
#[31] "CCF_3_45" "CCF_4_45" "CCF_1"    "CCF_2"    "CCF_3"    "CCF_4"   
#[37] "CCF_1_5"  "CCF_2_5"  "CCF_3_5"  "CCF_4_5"  "TT"       "TTN"     
#[43] "SRT5"     "LRT5"     "LRT10"    "LRT15"    "LRT30"    "THwoTM20"
#[49] "SR5"      "TH"       "THNS"     "THwoT"    "SA"  


# Subset teh df.sim data for stand 13243875
stand.sub <- subset(df.sim, id == 13243875)

length(unique(stand.sub$year))
length(unique(df.sim$id))


length(unique(a$id))

length(unique(df.opt.ls[[63]]$id))

# How many stands are in filtered data? optimal scenarios 46-63
lapply(df.sim.opt, function(df) length(unique(df$id)))
# some scenarios have 1474?

# How many stands in uniquely in simulated data??
lapply(df.opt.ls, function(df) length(unique(df$id)))


table()

# Need to further split this tables by individual years to have 
# one stand over one time
require(dplyr)

df5 <-df.sim.opt[[5]] %>% 
  group_split(year) 

# Check how many regimes I have in every table? 
lapply(df.sim.opt, function(df) length(unique(df$avohaakut)))


head(df.sim.opt[[2]])


# ---------------------------------------------------
# Calculate the pairs of neighbors:
# calculate open_edge
# how to interpret thin year??? 
# ---------------------------------------------------

nbrs <- find_nbrs_geom(df.geom)

df.sim<- df5[[1]]

oo <- open_edge_by_nbrs(nbrs, df5[[1]])

#lapply(   function(df, )open_edge_by_nbrs()

i = 11
# get ids of the central stand and neighbors
central_id <- as.character(unique(nbrs[[i]]$central))
nbrs_id    <- unique(nbrs[[i]]$nbrs)

# Get the stand height from simulated data for central stand and 
# neighbors 
central_H = rep(subset(df.sim, id %in% central_id)$H_dom, 
                length(nbrs_id))
nbrs_H    = subset(df.sim, id %in% nbrs_id, select = c("H_dom"))$H_dom

# Why I have subsettted only 8 values from 9?
subset(df.sim, id %in% nbrs_id)

sort(subset(df.sim, id %in% nbrs_id, select = c("id", "H_dom"))$id)


subset(df.geom, standid %in% nbrs_id)



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




# Check how to interpret thinning????
unique(df.sim$THIN) # For example LRT5" or SRT5 = Short rotation thinning 5

# subset one stand to see how does the thinninh year and similated
# year work together
# and how to reclassify to suvanto's values

tb.thin <- subset(df.sim, avohaakut == "LRT5" & id == 13243875,
                  select = c("year", "THIN"))


# How to reaclassify when the thinning has happened? 
#tb.thin <- 
 # mutate(time_since = case_when( )


year = seq(5,45, 5)
event = c(NA, 14,NA, NA, 29, NA, NA, NA, NA)
event.short<- event[!is.na(event)]

my.df <- data.frame(year,
                    event)

my.df

my.df$difference <- c(0,0,1,6,11,1,6,11,16)




         
         









# Check if we can split it in individual landscapes: each regime 
# is in every year
table(df.geom$year)
table(df_opt16$avohaakut)
table(df_opt16$year, df_opt16$avohaakut)

# Seems that is well 
# next need to look throuught optimal scenarios to subset df to recreate 
# 63 different development trajectories

# Now just try to recreate single landscape in 2016
df_2016 <- subset(df_opt16, year == 2016)

head(df_opt16)

# Merge the stand geometry with simulated data for one optimal scenario
df_opt16 <-  
  df.geom %>% 
  left_join(df_opt16, by = c("KUVIO_ID" = "id"))



plot(subset(df_opt16, year == 2016)["H_dom"])
plot(subset(df_opt16, year == 2016)["avohaakut"])








