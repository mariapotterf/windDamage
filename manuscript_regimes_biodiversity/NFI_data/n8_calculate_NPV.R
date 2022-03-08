

# Calculate NPV values

# calculate NPV for individual stands over time

# from Kyle:

# import pandas as pd
# path = "/scratch/project_2003638/MAYA_risk/"
# files = ["rcp0BAUwT_GTR_rsk"]
# for file in files:
#   t = pd.read_csv(path + file + ".csv")
# t["disc_income"] = t['cash_flow']/((1.03)**(t['year']-2016))
# t['disc_PV']=t['PV']/((1.03)**(t['year']-2016))
# 
# def age_groups(vec):
#   x = vec[0]
# y = vec[1]
# if y < 2111:
#   return 0
# else:
#   return x
# t['disc_PV'] = t[['disc_PV','year']].apply(age_groups,axis = 1)
# pd.DataFrame(t.groupby(['id','regime']).sum()['disc_PV']+t.groupby(['id','regime']).sum()['disc_income']).rename(columns={0: "NPV"}).to_csv(path+file+"_NPV.csv")




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
source(paste(myPath, 'r_winddamage', 'myFunctions.R', sep = "/"))



# Get data & final table ------------------------------------------------------------------------------
inPath   = myPath
inFolder = "output/windRisk_csv"
inName   = 'rcp0BAUwT_GTR_rsk.csv'

# Input table
df <- data.table::fread(paste(inPath, inFolder, inName,  sep = "/"),  # 
                            data.table=TRUE, 
                            stringsAsFactors = FALSE)
                            

# Make a function to calculate NPV:

calculate_NPV <- function(df, ...) {
  # Calculate discounted income and PV 
  df$disc_income = df$cash_flow/(1.03^(df$year-2016))
  df$disc_PV = df$PV/(1.03^(df$year-2016))
  
  # Replace the disc_PV value by 0 if less year < 2111
  df <- df %>% 
    mutate(disc_PV = case_when(year < 2111 ~ 0,
                                year >= 2111 ~ disc_PV))
  # Calculate sums 
  df <- df %>% 
    group_by(id, regime) %>% 
    summarize(NPV = sum(disc_PV, na.rm = T)+
                sum(disc_income, na.rm = T))
  
  return(df) 
}

df1<-calculate_NPV(df.out)
