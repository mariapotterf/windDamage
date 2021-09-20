
# ---------------------------------
# Sensitivity analysis:
# ---------------------------------

# How does the variation between variables changes given teh number of subsetted IDs?
# Check H_dom
# check wind risk
# variable as boxplot, x = number of sampled IDs

# Process:
# Read data and make plots
rm(list = ls())

setwd("C:/MyTemp/myGitLab/windDamage")

# Read libraries
library(dplyr)
library(tidyr)
library(data.table)
library(stringr)
library(ggplot2)

# Correct order of variables, factors & levels
# need to have same names of columns?? YES
# when data are sourced (source()), they are all available in my script
source("C:/MyTemp/myGitLab/windDamage/myFunctions.R")




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





# Get data ------------------------------------------------------------------------------
inPath = "C:/MyTemp/2021_WindRisk_biodiversity"
inFolder = "output/windRisk_csv"
#outFolder = 'output_CC'


# Select only regimes of interest:   pattern="xx1|xxx2", 3 CC
df.names = list.files(paste(inPath, inFolder, sep = "/"), 
                      pattern = "CCF|x4|x3|x2|x1|BAUwoT_|BAUwGTR_|BAU_") # .csv$
(df.names)

# Read dataframes
df.ls <- lapply(df.names, function(name, ...) data.table::fread(paste(inPath, inFolder, name,  sep = "/"),  # 
                                                                data.table=TRUE, 
                                                                stringsAsFactors = FALSE,
                                                                integer64="character"))
# Test for BAU: df.ls[[1]]
# test for CCF df.ls[[20]]

# 
# remove unnecessary columns
cl_keep <- c(
  "year",
  "branching_group",
  "Age",
  "H_dom" ,
  "COMBINED_HSI",
  "name",
  "id",
  #"avgTemp"
  #"windSpeed"
  "windRisk"
)

# Make a function to subset specific number of IDS
my_ids  <- unique(df.ls[[20]]$id)

sampleIDs <- function(x, ...) {
 
  # Sample the specific IDs
  sub_ids <- sample(my_ids, x) 
  
  # convert from integer64 to numeric:
  sub_ids <- as.numeric(sub_ids)
  
  # subset the same stands from each regime
  df2 <- df.ls[[1]] %>% filter(id %in% sub_ids)
  
  # keep only needed columns
  df2 <- df2 %>% dplyr::select(cl_keep)
  
  # add indication of teh sample
  df2$sample <- rep(x, nrow(df2))
  
  # return the dataframe
  return(df2)

}

dd<-sampleIDs(1000)

length(unique(dd$id))


# test it for a vector of values
sample_size <- c(50, 100, 1000, 5000, 10000, 25000, 40000)

df.ls2 <- lapply(sample_size, sampleIDs)


# Not sure hor to loop over different databases: 
# let's check how does it look like over bau given various sample size


# Merge data together -----------------------------------------------
df.out <- do.call(rbind, df.ls2)


# plot data
df.out %>% 
ggplot(aes(x = as.factor(sample),
           y = windRisk, #H_dom,
           group = sample)) +
  geom_boxplot()



