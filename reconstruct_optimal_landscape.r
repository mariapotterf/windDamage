
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


# Read corrected simulated names:
df<- data.table::fread("C:/MyTemp/avohaakut_db/analyzed/simulated_AVK_regimes.csv")

# Read stand geometry
# -----------------------------
df.geom <- read_sf("C:/MyTemp/avohaakut_db/14.534/14.534/mvj_14.534.shp")



# -----------------------------
# Read optimal solution:
# -----------------------------

# Test on one file:
# ---------------------------

# Replace teh characters to correctly read data into 3 columns
#txt <-  readLines("C:/MyTemp/avohaakut_db/solutions/Bundles_2_nocow_NPV_MANAGE_price_three_0_0_1_1_ALL0.csv")
#optim <-read.table(text = gsub("[()\",\t]", " ", txt))

#names(optim) <- c("id", "regime", "proportion")

# keep only the largest proportion by the stand
#optim.max<-
# optim %>%
#dplyr::group_by(id) %>%
#filter(proportion == max(proportion))


# Make a function to read the optimal data correctly and 
# to filter just the prevailing forest management by stand
# function also keep just one stand with the gighest proportion of management
readOptimal <- function(df.path, ...) {
  
  # read individual lines
  txt <-  readLines(df.path)
  
  # Read table by replacing characters
  optim <-read.table(text = gsub("[()\",\t]", " ", txt), stringsAsFactors = F)
  
  # Rename the columns of columns names
  names(optim) <- c("id", "regime", "proportion")
  
  # keep only the largest proportion by the stand
  optim.max<-
    optim %>%
    dplyr::group_by(id) %>%
    filter(proportion == max(proportion))
  
  # Return the dataframe with correct columns and filtered 
  # stands having one regime by stand
  return(optim.max)
  
}


# Read all optimal solutions to 
# see applied regimes?? 
# -------------------------------
setwd("C:/MyTemp/avohaakut_db/solutions")
df.optim = list.files(pattern=".csv$",
                      full.names = TRUE) # ends with .csv$, \\. matches .

# Read all dataframes in a loop
df.opt.ls = lapply(df.optim, readOptimal)


# Check if tehy all have the 1475 stands???
lapply(df.opt.ls, function(df) length(unique(df$id)))

# How does teh table looks like?
# lapply(df.opt.ls, function(df) table(df$regime))

# Subset the original table as one by one???
opt_sol <- df.opt.ls[[16]]

# Filter data by regime and by id
df_opt16 <- df %>% 
  semi_join(opt_sol, 
            by = c("id" = "id", 
                   "avohaakut" = "regime")) 


# Check if we can split it in individual lans\dscapes: each regime 
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



# Merge tables into one
dfs.opt = do.call("rbind", df.opt.ls)


# Check what management are there??
sort(unique(dfs.opt$regime))  # 58 regimes
sort(unique(df.av$avohaakut)) 


# some regims area missing from simuylated data?
setdiff(sort(unique(dfs.opt$regime)),
        sort(unique(df.av$avohaakut)) )




#write.csv(sort(unique(dfs.opt$regime)), 
#         "C:/MyTemp/myGitLab/windDamage/params/regimes_58.csv")

# How to merge the 58 regimes with my databases??? 

unique(df$gpkg) 
unique(df$regime)



# Get to know the data:
# -------------------------------
# how many unique stands?? ~ 1475?? 
length(unique(df$id))  # YES


# how many regimes??
length(unique(df$regime))
# only 24 regimes, why???



# Subset the optimal solution of the simulated data: will this recreate my landscape? 
# will there be any repetitions??
# -----------------------------------
# subset by one .db
df.sa <- subset(df, gpkg == "RCP45_NEW_SA" & id %in% my.stands.sub)

length(unique(df.sa$id))
# 1238

length(unique(df.sa$regime))
# 1  # SA has only one regime

unique(df.sa$year)

# is each stand simulated every years???
table(df.sa$year)  # YES



# Check CCF regime
# ------------------------------------

# subset by one .db
df.ccf_4_45 <- subset(df, gpkg == "RCP45_NEW_CCF_4_45" & id %in% my.stands.sub)

length(unique(df.ccf_4_45$id))
# 1238

length(unique(df.ccf_4_45$regime))
# 1  # SA has only one regime

unique(df.ccf_4_45$year)

# is each stand simulated every years???
table(df.ccf_4_45$year)  # YES




# -----------------------------------------
# Try to subset the optimal scenario:
# -----------------------------------------

# from the whole simulated data : df)
# for the standid I need to subset specific regime

opt.df <- subset(df, id %in% unique(optim.max$id))

