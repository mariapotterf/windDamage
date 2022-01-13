# ----------------------------------
# Filter datasets to make a final table
# ----------------------------------


# Read data and make tables and plots
rm(list = ls())

# get the common location:
source('myPath.R') 


setwd(paste(myPath, "r_winddamage", sep = "/"))

# Read libraries
library(dplyr)
library(tidyr)
library(data.table)
library(stringr)
library(ggplot2)
library(ggpubr)

# Correct order of variables, factors & levels
# need to have same names of columns?? YES
# when data are sourced (source()), they are all available in my script
#source("C:/Users/ge95bag/Documents/git/windDamage/myFunctions.R")
source(paste(myPath, 'r_winddamage', 'myFunctions.R', sep = "/"))


# Get data ------------------------------------------------------------------------------
inPath    = myPath#"C:/Users/ge95bag/Documents/Projects/2021_WindRisk_biodiversity"
inFolder  = "output/windRisk_csv"
outFolder = 'output/plotting'
outName   = 'df_filt.csv'


# Select only regimes of interest:   pattern="xx1|xxx2", 3 CC
df.names = list.files(paste(inPath, inFolder, sep = "/"), 
                      pattern = "CCF|x4|x3|x2|x1|BAUwoT_|BAUwGTR_|BAU_") # .csv$
(df.names)

# Read dataframes
df.ls <- lapply(df.names, function(name, ...) data.table::fread(paste(inPath, inFolder, name,  sep = "/"),  # 
                                                           data.table=TRUE, 
                                                           stringsAsFactors = FALSE,
                                                           integer64="character"))


# Duplicate the data
df.ls2 <- df.ls

# remove unnecessary columns
cl_keep <- c(
  "year",
  "branching_group",
  "Age",
  #"PV",
  #"cash_flow",
  #"BA",
  "V",
  "Harvested_V_log" ,
  "Harvested_V_pulp",
  "V_total_deadwood",
  #"DEVEL_CLASS"
  # "SC"                      
  #"SOIL_CLASS"
  # "THIN"  
  # "PEAT"
    "H_dom" ,
  # "D_gm"
  "MAIN_SP",
  "CAPERCAILLIE",
  "HAZEL_GROUSE" ,
  "THREE_TOED_WOODPECKER",
  "LESSER_SPOTTED_WOODPECKER",
  "LONG_TAILED_TIT",
  "SIBERIAN_FLYING_SQUIRREL",
  "COMBINED_HSI",
   "name",
  "cell",
  "id",
  #"avgTemp"
  #"windSpeed"
  "regime",
  "adapt",
  "magnit" ,
  #"THIN2"                     "THIN_filled_lagged"
  #"difference"
  "since_thin",
  "windRisk"
)


# Cells less then 4-16 are in the south

# keep only needed columns
df.ls2 <- lapply(df.ls2, function(df) df %>% 
         dplyr::select(cl_keep))


# Merge data together -----------------------------------------------
df.out <- do.call(rbind, df.ls2)



# Get summary stats for V and deadwood volume before filetring ------------


summary(df.out$V, na.rm = T)

#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00   41.87  134.80  146.51  211.40 1239.23 

summary(df.out$V_total_deadwood , na.rm = T)

#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.00      7.47     13.03     17.83     23.25 191014.61 


rm(df.ls)

# Classify climate change ------------------------------------------------
df.out <- df.out %>% 
  mutate(climChange = case_when(
    grepl("RCP0", name)  ~ "REF",
    grepl("RCP45", name) ~ "RCP45",
    grepl("RCP85", name) ~ "RCP85"))


# Correct regimes names --------------------------------------------------
df.out <- df.out %>%
  mutate(regime = case_when(
    branching_group == 'Tapio harvest'                    ~ 'BAU', 
    branching_group == 'Tapio harvest nature scene'       ~ 'GTR', 
    branching_group == 'Tapio harvest without thinnings'  ~ 'noThin', 
    branching_group == 'Long rotation harvest 10 p'       ~ 'ext_10', 
    branching_group == 'Long rotation harvest 30 p'       ~ 'ext_30', 
    branching_group == 'Short rotation harvest 30 n'      ~ 'short_30', 
    branching_group == 'Short rotation harvest 10 n'      ~ 'short_10',
    branching_group == 'Selection cut'                    ~ 'CCF'))


# Change order of change time---------------------------------------------
df.out$climChange <-factor(df.out$climChange, 
                            levels = c("REF", "RCP45", "RCP85"))


# Order the regimes by 'intensity': from the most intensive to the least intensive: 
# a bit questionnable if my order is correct? CCF can be very intensive n terms of thinning
# ordered in terms of timing of final cut
df.out <- df.out %>% 
  mutate(regime = factor(regime, 
                         levels = c("short_30", 
                                    "short_10",
                                    "BAU", 
                                    "noThin", 
                                    "ext_10", 
                                    "ext_30", 
                                    "GTR", 
                                    "CCF")))




# Classify into short and long term effects; (short term: 30 years, until 2046)
df.out <- df.out %>% 
  mutate(timeEffect = case_when(
    year  > 2046 ~ 'long-term',
    year <= 2046 ~ 'short-term'
  ))



# ------------------------------------------------------
#                  Data filtering
# ------------------------------------------------------


# Filter 1: crazy values
# Filter 2: keep consistent regimes and clim CHange scenarios


# ------------------
# FILTER 1 :  # Filter first all ids that have unrealistic values

# deadwood volume seems incredible high for GTR: check for median and max values!
range(df.out$V)

#[1]    0.000 1239.226
# Max volume is 600 m3/ha

range(df.out$V_total_deadwood)
# 0.0 191014.6
# max deadwood volume is for each species 300m3/ha


range(df.out$H_dom, na.rm = T)
# [1]  0.5869817 41.2494774

# which stands are crazy??

# Check deadwood
crazyID_DW <- df.out %>% 
  filter(V_total_deadwood > 300) %>% 
  distinct(id) %>% # regime,
  pull()

# Crazy volume
crazyID_V<- df.out %>% 
  filter(V > 700) %>% 
  distinct(id) %>% # regime,
  pull()

# Union two vectors, keep all unique values
(crazy_union = union(crazyID_DW, crazyID_V))


# remove the crazy id from the table
df.out2 <- df.out %>% 
  filter(!(id %in% crazy_union))



# was filtering sussesfull? need to change my quantiles!
# Yes, changes
summary(df.out2$V, na.rm = T)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00   41.64  132.32  142.55  205.70  800.00 

summary(df.out2$V_total_deadwood , na.rm = T)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   7.394  12.932  17.034  23.027 396.942 


# Which stands still have weird numbers??
df.out2 %>% 
  filter(V_total_deadwood > 250) %>%
  distinct(id, regime, climChange) %>% 
  
# example: 
  # 2200506200      BAU        REF
#  181: 1100302400 short_10      RCP85
#182: 2200506200 short_10      RCP85

df.out2 %>% 
  filter(id == '2200506200' & regime == "BAU" & climChange == "REF" ) %>%
  select(id, year, Age, V_total_deadwood, V, H_dom)

# Some stands have higher deadwood volume that maximal volume:   e.g. 2200506200      BAU        REF
# maybe I can filter all stands that have max deadwood volume larger then max V??

# Filter V vs DW volume

crazy_id2 <- 
  df.out2 %>% 
  group_by(id, regime, climChange) %>% 
  summarize(max_DW = max(V_total_deadwood, na.rm = T),
            max_V  = max(V, na.rm = T)) %>% 
  mutate(diff_V_DW = max_V - max_DW) %>% 
  filter(diff_V_DW < 0) %>% 
    ungroup() %>% 
  distinct(id) %>% 
  pull()


# Fiulter values:
# remove the crazy id from the table
df.out2 <- df.out2 %>% 
  filter(!(id %in% crazy_id2))


summary(df.out2$V)
summary(df.out2$V_total_deadwood)



# Filter 2 : Keep only stands that have all regimes and all climate change scenarios???

# example" 
# I can split the data by the cell indicator, and then try this:
n1 <- df.out2 %>% 
  filter(cell == 'k3')  # k4 has only 47000 rows 'n4'

# need to filter first by regime, then by climate change
length(unique(n1$id))
n2 <- n1 %>%
  group_by(id) %>%
  filter(n_distinct(regime) == n_distinct(n1$regime))

n3 <- n2 %>%
  group_by(id) %>%
  filter(n_distinct(climChange) == n_distinct(n2$climChange))


length(unique(n1$id))
length(unique(n2$id))
length(unique(n3$id))


 

# make a function to filter the values----------------------------
group_filter <- function(df, ...) {
  df2 <- df %>%
    group_by(id) %>%
    filter(n_distinct(regime) == n_distinct(df$regime))
  
  df3 <- df2 %>%
    group_by(id) %>%
    filter(n_distinct(climChange) == n_distinct(df2$climChange))
  
  return(df3)
}

# test function
nn3 <-group_filter(n1)


# Split dataframe in a list of dfs based on cell value; and filter the data by the 
# consistent id and climChange over regimes
ls1 <-  df.out2 %>% 
  group_split(cell)


# run the filtering id over the list
ls2 <- lapply(ls1, group_filter)

# put filtered items in back in df
df.filt <- do.call("rbind", ls2)


# Investigate if the stands have been filtered
length(unique(df.filt$id))  # 34062 # filtered for consistency in regime and clim change
length(unique(df.out2$id))  # 41274  # filtered crazy vlues




# Check if my new data have a same number of regimes and climate changes scenarios?
df.filt %>% 
  group_by(id, regime, climChange) %>% 
  tally() %>% 
  filter(n<20)

# seems to be correctly filtered! each id has 20 alternatives! 


# Export table
data.table::fwrite(df.filt, paste(inPath, outFolder, outName, sep = "/"))



# Remove everything from here down !!!!!


