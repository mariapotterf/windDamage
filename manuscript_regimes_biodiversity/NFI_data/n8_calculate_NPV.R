
# ---------------------------------------
#     Calculate NPV values  
# ---------------------------------------


# calculate NPV for individual stands over time
# over id, climchange and regime 

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

source(paste(myPath, 'r_winddamage', 'myFunctions.R', sep = "/"))



# Get data ------------------------------------------------------------------------------
inPath    = myPath
inFolder  = "output/windRisk_csv"
outFolder = 'output/plotting'
outName   = 'df_NPV.csv'


# Select only regimes of interest:   pattern="xx1|xxx2", 3 CC
df.names = list.files(paste(inPath, inFolder, sep = "/"), 
                      pattern = "CCF|x4|x3|x2|x1|BAUwoT_|BAUwGTR_|BAU_") # .csv$
(df.names)

# Read dataframes
df.ls <- lapply(df.names, function(name, ...) data.table::fread(paste(inPath, inFolder, name,  sep = "/"),  # 
                                                                data.table=TRUE, 
                                                                stringsAsFactors = FALSE,
                                                                integer64="character"))

# remove unnecessary columns
cl_keep <- c(
  "year",
  "branching_group",
  "Age",
  "PV",
  "cash_flow",
  "name",
  "cell",
  "id"
)


# keep only needed columns
df.ls2 <- lapply(df.ls, function(df) df %>% 
                   dplyr::select(cl_keep))

# Calculate the NPV
df.ls2 <- lapply(df.ls2, calculate_NPV)


# Merge data together -----------------------------------------------
df.out <- do.call(rbind, df.ls2)


# Correct regimes names --------------------------------------------------
df.out <- df.out %>%
  ungroup() %>% 
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



# Check final table -------------------------------------------------------
df.out <- df.out %>% 
  ungroup() %>% 
  select(-c(branching_group))



# Plot: is the PV (present value of standing trees) different between noThin and BAU?




# Export final table ------------------------------------------------------
data.table::fwrite(df.out, paste(inPath, outFolder, outName, sep = "/"))





# Set themes ----
theme_set(theme_classic())
theme_update(panel.grid.major = element_line(colour = "grey95",  # background gridlines
                                             size = 0.1,
                                             linetype = 2),
             strip.background = element_rect(color="white",      # headings for facets
                                             fill="white",
                                             size=0.1, 
                                             linetype="solid"),
             panel.background = element_rect(fill = "white",     # individual plots
                                             colour = "grey50",
                                             linetype = "solid"))

