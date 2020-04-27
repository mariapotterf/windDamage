
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
df.geom <- subset(df.geom, select = c("KUVIO_ID"))


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

df.names <- gsub("./Bundles_2_nocow_NPV_MANAGE_price_three_0_0_1_1_", "", df.optim)
df.names <- gsub(".csv", "", df.names)

# Read all dataframes in a loop
df.opt.ls = lapply(df.optim, readOptimal)

# Add indication of the name:
# deoes not work???
# !!!!
Map(cbind, df.opt.ls, bundle = df.names)
#Map(cbind, df.opt.ls, SampleID = names(df.opt.ls))


Map(cbind, df.opt.ls, SampleID = names(df.opt.ls))


library(dplyr)
library(purrr)

purrr::map2(df.opt.ls, df.names, ~cbind(.x, SampleID = .y))


# vector of values you wish to add
years <- c("a", "b", "d")     # changed to plural to indicate vector of values rather than single value

# make dummy list of dataframes
data1 <- data.frame(var1 = c(1:100))
data2 <- data.frame(var1 = c(1:10))
data3 <- data.frame(var1 = c(1:25))
my.ls <- list(data1 = data1, data2 = data2, data3 = data3)

# Loop through list of dataframes and to each dataframe add a new column
out<- Map(cbind, my.ls, year=years)















# Check if tehy all have the 1475 stands???
#lapply(df.opt.ls, function(df) length(unique(df$id)))

# How does teh table looks like?
# lapply(df.opt.ls, function(df) table(df$regime))

# Subset the original table as one by one???
# one file example

opt_sol <- df.opt.ls[[16]]

# Filter data by regime and by id using semi_join
df_opt16 <- df %>% 
  semi_join(opt_sol, 
            by = c("id" = "id", 
                   "avohaakut" = "regime")) 


# Filter the simulated data by the optimal scenarios:
df.opt.filt <- lapply(df.opt.ls, function(df.filter) semi_join(df,
                                                               df.filter,
                                                               by = c("id" = "id", 
                                                                      "avohaakut" = "regime")))

# Check how many regimes I have in every table? 
lapply(df.opt.filt)



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








