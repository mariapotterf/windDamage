

# -----------------------------------------
#   Plot data for publication
# -----------------------------------------


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


# Get data & final table ------------------------------------------------------------------------------
inPath   = myPath
outFigs  = 'manuscript_regimes_biodiversity/NFI_data/Figs'
inFolder = "output/plotting"
inName   = 'df_filt.csv'
inNPV    = 'df_NPV.csv'
outHabitat  = 'df_HSI.csv'

# Input data table witha all data
df.out <- data.table::fread(paste(inPath, inFolder, inName,  sep = "/"),  # 
                                                                data.table=TRUE, 
                                                                stringsAsFactors = FALSE,
                                                                integer64="character")
# Input table with NPV values
df.NPV <- data.table::fread(paste(inPath, inFolder, inNPV,  sep = "/"),  # 
                            data.table=TRUE, 
                            stringsAsFactors = FALSE,
                            integer64="character")

# do they have a different number of stands??
stand_id = unique(df.out$id)


# Filter data for df.NPV, same ids as the filtered data:
df.NPV <- df.NPV %>% 
  filter(id %in% stand_id)



# Order factors levels ----------------------------------------------------

# Regimes
regime_levels = c("short_30", 
                "short_10", 
                "noThin",
                "BAU", 
                "ext_10", 
                "ext_30", 
                "GTR", 
                "CCF")

clim_levels = c("REF",
                "RCP45", 
                "RCP85")

# Climate change
df.out$climChange <-factor(df.out$climChange, 
                           levels = clim_levels)


df.out <- df.out %>% 
  mutate(regime = factor(regime, 
                         levels = regime_levels))


# order the NPV values ------------------------------------------------------------
df.NPV$climChange <-factor(df.NPV$climChange, 
                           levels = clim_levels)


df.NPV <- df.NPV %>% 
  mutate(regime = factor(regime, 
                         levels = regime_levels))


# Create plots  ----------------------------------------------------------------------------------------

# Define labels:
lab_manag = c("Regime adaptation")




# Get stats for the NPV ---------------------------------------------------
df.NPV %>% 
  group_by(regime, climChange) %>% 
  summarise(mean_NPV = mean(NPV, na.rm = T)#,
            #sd_NPV = sd(NPV, na.rm = T)
            ) %>% 
  pivot_wider(names_from = c(regime),
              values_from = mean_NPV)





# (not run) Why the NPV can be the same between BAU and noThin?  -------------------------
# changes in tree species (getting towards spruce instead or pine? shortened rotation compared to 
# species change)
# check frequency of dom tree species between regimes?
# or H_dom?
# or age at harvest? age should be ollder at no thinning
head(df.out)

# Get individual tree species:
#df.out
mutate(species = case_when(MAIN_SP == 1 ~ "pine",
                           MAIN_SP == 2 ~ "spruce",
                           TRUE ~ "other")) %>% 


# The tree species remains the same: no effect on NPV
df.out %>% 
  filter(climChange == 'REF' & (regime == 'BAU' | regime == 'noThin')
         & year > 2080) %>% 
  mutate(species = case_when(MAIN_SP == 1 ~ "pine",
                             MAIN_SP == 2 ~ "spruce",
                             TRUE ~ "other")) %>% 
  group_by(regime, species) %>% 
  tally()


# Check the average age at the harvests? is it later at the noThin?
# not visible at the 5-yr time resolution










# Explore the data basic statistics --------------------------------------------------------


# Get histograms ----------------------------------------------------------------
#windows()
hist(df.out$V)
hist(df.out$V_total_deadwood)

median(df.out$V)  # 131 m3/ha
median(df.out$V_total_deadwood) # 13 m3/ha


# Importance of the year! chenck DW in 2016!
df.out %>% 
  filter(year == 2016) %>% 
  summarize(mean_V = mean(V, na.rm = T),
            mean_DW = mean(V_total_deadwood, na.rm = T ))


#mean_V  mean_DW
#1 114.995 3.648799

# Why no thinning has low effects? ---------------------------------------------

# Does it depend on the initial age of the stands???
# test for old, young and medium age in 2016
# old        = no effect
# young      = little effect, theye is slightly more deadwood in o thinned stands than in thinned stands
# medium age = little effect


# Get boxplot of DW volumes by climChange and regimes
df.out %>% 
  filter(regime == 'BAU' | regime == "noThin") %>% 
  
  ggplot(aes(y = V_total_deadwood,
             x = cell,
             fill = regime)) +
  geom_boxplot() + 
  facet_grid(climChange~timeEffect)
# no differenece, BAU has very high DW

# Ge old stands

df.out %>% 
  filter(Age > 150 & year == 2016) %>%
  distinct(id)   # > 90: 3100201305

# > 150: 3100202108

df.out %>% 
  filter(Age < 30 & year == 2016) %>%
  distinct(id)   # < 30: 3100000304


df.out %>% 
  filter(id == '3100000304') %>% 
  filter(regime == 'BAU' | regime == "noThin") %>% 
  ggplot(aes(y = V_total_deadwood,
             x = regime,
             fill = climChange)) +
  geom_boxplot() 


df.out %>% 
  filter(regime == 'BAU' | regime == "noThin") %>% 
  group_by(regime, year, climChange) %>% 
  summarise(DW_mean = mean(V_total_deadwood, na.rm = T)) %>% 
  ggplot(aes(y = DW_mean,
             x = regime,
             color = climChange)) +
  geom_line()





# Check deadwood volume between BAu and noThin for young stands in 2016?? e.g. 1101306006 (<10)
df.out %>% 
  filter(cell == 'n4' & climChange == "REF" & id == 1101306006) %>%  # & year == 2016
  filter(regime == "BAU" | regime == "noThin") %>% 
  #filter(Age < 10) %>%
  #distinct(id)
  dplyr::select(id, year, Age, V_total_deadwood, regime, climChange) %>% 
  arrange(year) %>% 
  print(n = 40)



# Check deadwood volume between BAu and noThin for mediaum age stands?  (~30) in 2016?? e.g. 1101000109
df.out %>% 
  filter(cell == 'n4' & climChange == "REF" & id == 1101000109) %>%  # & year == 2016 1101000010 1101000100  1101306009
  #filter(cell == 'n4' & climChange == "REF" & year == 2016) %>%  # & 
  filter(regime == "BAU" | regime == "noThin") %>% 
  #filter(Age > 30 &Age < 45) %>%
  #distinct(id) 
  dplyr::select(id, year, Age, V_total_deadwood, regime, climChange) %>% 
  arrange(year) %>% 
  print(n = 40)  %>% 
  ggplot(aes(x = year, 
             y =  V_total_deadwood,#V, #V_total_deadwood, #V, #Age, #V,#windRisk, #,
             color = regime)) +
  geom_line() +
  facet_grid(.~climChange)



# select stand in souths: cell 11 = n4 is Jyvaskyla: id:1101000002
df.out %>% 
  filter(cell == 'n4'& id == 1101000002 & climChange == "REF" ) %>%  # & year == 2016
  filter(regime == "BAU" | regime == "noThin") %>% 
  #filter(Age > 90) %>%
  dplyr::select(id, year, Age, V,  V_total_deadwood, regime, climChange) %>% 
  arrange(year) %>% 
  print(n = 40) %>% 
  ggplot(aes(x = year, 
             y =  V_total_deadwood,#V, #V_total_deadwood, #V, #Age, #V,#windRisk, #,
             color = regime)) +
  geom_line() +
  facet_grid(.~climChange)


#distinct(climChange)


# Check stand 1101000002
df.out %>% 
  filter(id == '1100004405') %>% #   '1100105001') %>% 
  filter(regime == "BAU" | regime == "noThin") %>%
  select()
ggplot(aes(x = year, 
           y = V_total_deadwood, #V, #Age, #V,#windRisk, #,
           color = regime)) +
  geom_line() +
  facet_grid(.~climChange)


# maybe because stands are very young? check oit the development in old
# stands:
# Select stands that are old in 2016:
df.out %>% 
  filter(year == 2016 & Age > 200)  %>%
  distinct(id)

# stands over 200 years: 3100000303

df.out %>% 
  filter(id == '3100000303') %>% 
  # distinct(regime) #   '1100105001') %>% 
  filter(regime == "BAU" | regime == "noThin") %>% 
  ggplot(aes(x = year, 
             y =  Age,#V, #V_total_deadwood, #V, #Age, #V,#windRisk, #,
             color = regime)) +
  geom_line() +
  facet_grid(.~climChange)



# the climate change scenarios are missing????? 
# coulds bem, at some regimes are possible only under climate chage in the north
# select id of the old stand:
df.out %>% 
  filter(year == 2016) %>% 
  #filter(year == 2021 & (regime == "BAU" | regime == 'noThin')) %>%
  # filter(Age > 10) %>% 
  filter(id == '101001201') %>% 
  # distinct(id) 
  distinct(climChange) 


# Why the extended rotation does not further increase in wind risk?
df.out %>% filter(regime == 'ext_30') %>% 
  distinct(id)

# make a plot of wind risk
df.out %>% 
 # filter(id == 2800601003 & regime == 'ext_30') %>% 
  ggplot(aes(x = year,
             y = windRisk,
             color = climChange)) +
  geom_line()

# Example ids:                id
#1:  101000107
#2:  101000302
#3:  101000406
#4:  101000407
#5:  101001201
# ---           
# 11144: 2800004307  # only RCP85
# 11145: 2800501303
# 11146: 2800501306
# 11147: 2800501307
# 11148: 2800601003





# Test hypotheses:  --------------------------------------------------------

# select only regimes to test my hypotheses: RF, adaptation, w/wo thinning, CCF
# keep climate change
# biodindicator






# change all values to mean!!! 


# H1: 
# We suggest that shorter rotation length will reduce wind damage risk
# and conflict with biodiversity (HSI, old trees). 
# This effect will decrease with more sever climate change.

# Calculate the differences between mean values of wind risk 
# for each scenarios, BAU is a reference


# -----------------------------------------
#    Final plots                          #
# -----------------------------------------


# Barplot differenes in wind damage risk compared to BAU by climate change  --------------

# Make barplots with CI around it!
# group by ID ad then calculate teh differences?

# df.out %>% 
#   group_by(id, climChange, regime) %>% # modif, #geo_grad, id, 
#  # head()
#   summarise(windRisk_median = median(windRisk, na.rm = T),
#             HSI_median = median(COMBINED_HSI, na.rm = T)) %>% 
#   mutate(BAU_HSI          = HSI_median[match('BAU', regime)],
#          perc_change_HSI  = HSI_median/BAU_HSI * 100 - 100,
#          BAU_risk         = windRisk_median[match('BAU', regime)],
#          perc_change_risk = windRisk_median/BAU_risk * 100 - 100) #%>%
#   dplyr::select(c(climChange, regime, 
#                   perc_change_risk,
#                   perc_change_HSI)) %>%
#   pivot_longer(!c(regime, climChange), #everything(vars = NULL),
#                names_to = "Indicator", 
#                values_to = "perc_ch")  %>%
#   



# Differences in  timber volume REF - RCP85  ------------------------------------------------
# 
# Calculate the differences in harvested timber volume between reference and RCP 85:
# https://www.mathsisfun.com/numbers/percentage-change.html

# percentage change: change is a percent of teh old values: divide by the old values and mulltiply by 100%
# eg change from 5 to 7:
# first get teh difference: 7-5 = 2, divide by the old value: 
# percentage change from 5 to7 is 2/5 = 0.4 = 40%


df_timb <- df.out %>% 
  filter(climChange != 'RCP45') %>%   # remove the 'medium' scenario, keep only extremes to calculate the differences
  group_by(id, climChange, regime) %>% # modif, #geo_grad,
  summarise(sum_V_log     = sum(Harvested_V_log, na.rm = T),
            sum_V_pulp    = sum(Harvested_V_pulp, na.rm = T))  %>%
  ungroup() %>% 
  group_by(climChange, regime) %>% 
  summarise(sum_V_log     = mean(sum_V_log, na.rm = T),
            sum_V_pulp    = mean(sum_V_pulp, na.rm = T)) # %>%


# Split into two datasets and then merge by columns

df_timb_ref <- df_timb %>%
  filter(climChange == 'REF') %>% 
  rename(REF_V_log  = sum_V_log,
         REF_V_pulp = sum_V_pulp) %>% 
  ungroup() %>% 
  dplyr::select(-climChange)


df_timb_rcp85 <- df_timb %>%
  filter(climChange == 'RCP85') %>% 
  rename(RCP85_V_log  = sum_V_log,
         RCP85_V_pulp = sum_V_pulp) %>% 
  ungroup() %>% 
  dplyr::select(-climChange)


# join by columns and calculate the % change between timber volume between climate change
# Report this into a table??? !!!!

#df_timb_out <- 
df_timb_ref %>% 
  left_join( df_timb_rcp85) %>% 
  select(regime   ,
         REF_V_log, RCP85_V_log, 
         REF_V_pulp, RCP85_V_pulp)   %>% 
  mutate(Vlog_change         = RCP85_V_log  - REF_V_log,
         Vpulp_change        = RCP85_V_pulp - REF_V_pulp,
         #Vlog_perc_change    = RCP85_V_log/REF_V_log*100-100,
         #Vpulp_perc_change   = RCP85_V_pulp/REF_V_pulp*100-100,
         Vlog_perc_change    = Vlog_change/REF_V_log*100,
         Vpulp_perc_change   = Vpulp_change/REF_V_pulp*100) %>% 
  select(regime,
         REF_V_log, 
         RCP85_V_log, 
         Vlog_change, 
         Vlog_perc_change, 
         REF_V_pulp, 
         RCP85_V_pulp,
         Vpulp_change,
         Vpulp_perc_change) #%>%
#spread()




# apply a basic stat_summary
# df.out, but filetr to 1000 values


# Get plots with bar errors using 'stat_summary'  ----------------------------------

# First, merge all data into single long df
# needs to have normalized windrisk, log, pulp 
# Calculate first mean sums of harvested timber 
df_harv <- df.out %>% 
  group_by(id, climChange, regime) %>% # modif, #geo_grad,
  summarise(sum_V_log     = sum(Harvested_V_log, na.rm = T),
            sum_V_pulp    = sum(Harvested_V_pulp, na.rm = T))


# Try first on log and pulp
df_log <- df_harv %>% 
  group_by(climChange) %>% 
  mutate(norm_log = (sum_V_log /mean(sum_V_log [regime == "BAU"]))-1)  %>% 
  select(id, climChange, regime, norm_log) %>% 
  ungroup()



df_pulp <- df_harv %>% 
  group_by(climChange) %>% 
  mutate(norm_pulp = (sum_V_pulp /mean(sum_V_pulp [regime == "BAU"]))-1)  %>% 
  select(id, climChange, regime, norm_pulp) %>% 
  ungroup()



# Wind risk
df_wind <- 
  df.out %>% 
  group_by(climChange, id, regime) %>% 
  filter(!is.na(windRisk))  %>% 
  summarize(mean_wind = mean(windRisk, na.rm = T)) %>%  
  mutate(norm_risk = (mean_wind /mean(mean_wind [regime == "BAU"])) - 1)  %>%
  select(id, climChange, regime, norm_risk) %>% 
  ungroup()


# Change in NPV:
# this is all estimated over 100 years
df_NPV <- 
  df.NPV %>% 
  group_by(climChange, id, regime) %>% 
  summarize(mean_NPV              = mean(NPV, na.rm = T),
            mean_sum_dist_PV      = mean(sum_dist_PV , na.rm = T),
            mean_sum_dist_income  = mean(sum_dist_income, na.rm = T)) %>%  
  mutate(norm_NPV    = (mean_NPV /mean(mean_NPV[regime == "BAU"])) - 1,
         norm_PV     = (mean_sum_dist_PV /mean(mean_sum_dist_PV[regime == "BAU"])) - 1,
         norm_income = (mean_sum_dist_income /mean(mean_sum_dist_income[regime == "BAU"])) - 1) %>%
  select(id, climChange, regime, norm_NPV, norm_PV, norm_income) %>% 
  ungroup()
  

# Get mean values of norm_NPV
df_NPV %>% 
  group_by(climChange, regime) %>% 
  summarize(mean_normNPV = mean(norm_NPV, na.rm = T),
            sd_normNPV = sd(norm_NPV, na.rm = T))

# Make a simple table to check the results: -------------------------------------------------------
# First calculate mean over all years: summarize, reduce the number of rows
# add value for the BAU to each row
# !!!!!!!!!!!!!!!

df.NPV %>% 
  #filter(id == 1000500200) %>%
  group_by(climChange, regime) %>% 
  summarize(mean_NPV              = mean(NPV, na.rm = T)) %>% 
  mutate(   mean_NPV_BAU          = mean_NPV[regime == "BAU"],
            norm_NPV              = (mean_NPV /mean(mean_NPV[regime == "BAU"])) - 1) %>%
  print(n = 40) 


  ggplot(aes(x = regime, 
             fill = climChange,
             y = norm_NPV*100)) +
  geom_bar(stat="identity",  position=position_dodge())
  
  
# Calculate the varues: grouped by the ID
  
# grouped by regimes: 
  
  
  
# The plot corresponds to Kyle's values: varies between +-20% decrease/increas from BAU

# why do I have so much variability in NPV using each id numbers?


# Check calculation of % change (comment from Kyle:)


#I'm wondering about these values: 
#CCF = 6306, 
#BAU-10 = 5719, 
#EXT_10 = 4627, 
#EXT_30 = 3186 and 
#BAU = 5227 (for no CC) -- 
#so 6306/5227=1.201 (20% increase), 
# so 5719/5227=1.095 (9.5% increase), 
#so 4627/5227=0.886 (-11% decrease), so 3186/5227=0.6099 (-39% decrease), Unless I'm mixed up -- which is possible.

# Check very high NPV:
1000500407 

df.NPV %>% 
  filter(id == 1000500407) 
# filetr the rowns with high increase:
df_NPV %>%
  filter(norm_NPV > 0.6) %>% 
  distinct(id)# eg. 1000500402  increases by 100%


df_NPV %>% 
  filter(id == 1000500200)  %>% 
  filter(regime == 'BAU' | regime == 'CCF' ) %>% 
  print(n = 40)# %>% 
  filter(norm_NPV > 0.6) 

  


# Bar plot: wind risk, log and pulp -------------------------------------


windows(height = 7, width= 7)

# JOin the data and make a plot
p2 <-
  left_join(df_log, 
          df_pulp) %>% 
  left_join(df_wind) %>% 
  left_join(df_NPV) %>% 
  pivot_longer(!c(id, regime, climChange), #everything(vars = NULL),
               names_to = "Indicator", 
               values_to = "perc_ch")  %>%
  mutate(Indicator = factor(Indicator, 
                            levels = c('norm_risk', 
                                       'norm_NPV',
                                       'norm_PV',
                                       'norm_income',
                                       'norm_log', 
                                       'norm_pulp'),
                            labels = c('Wind damage risk',
                                       'NPV',
                                       'Discounted PV',
                                       'Discounted Income',
                                       'Log timber', 
                                       'Pulp timber'))) %>% 
  
  filter(regime != 'BAU') %>% 
  ggplot(aes(x = regime,
             y = perc_ch*100,
             fill = climChange)) + 
  stat_summary(geom = 'bar', 
               fun = 'mean',
              # width = .4,
               position = 'dodge') +
  stat_summary(geom = 'errorbar', 
               #width = .4,
               fun.data = mean_cl_normal,# mean_sdl, #, 
               fun.args=list(mult = 3), 
               position = 'dodge') +
  geom_hline(yintercept = 0) +
  coord_flip() +
  facet_wrap(.~ Indicator, scales = 'free') +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"), 
                    name="Climate change") +
  ylab("Difference from BAU scenario [%]") +
  xlab(lab_manag) +
  theme_bw()  + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
       #legend.position = c(.83, .2), # legend position within the plot, x, y
       legend.position = 'bottom', 
       legend.title = element_text(size=10),
        legend.text  = element_text(size=8),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.box.background = element_rect(colour = "black")) 

#"manuscript_regimes_biodiversity/NFI_data/Figs"
#p
ggsave(filename = 'Fig_2.pdf',
       #plot = p2, #last_plot(),
       device = 'pdf',
       path = paste(getwd(), outFigs, sep = '/'),
       width = 7,
       height = 7,
       units = c("in"),
       dpi = 300#,
)


# Plot combined HSI and biodiversity -------------------------------------------


# Make partial tables:

df_comb_HSI <- 
  df.out %>% 
  group_by(climChange, id, regime) %>% 
 # filter(!is.na(COMBINED_HSI ))  %>% 
  summarize(mean_HSI = mean(COMBINED_HSI, na.rm = T)) %>%  
  mutate(norm_HSI = (mean_HSI /mean(mean_HSI [regime == "BAU"])) - 1)  %>%
  select(id, climChange, regime, norm_HSI) %>% 
  ungroup()


df_DW <- 
  df.out %>% 
  group_by(climChange, id, regime) %>% 
  filter(!is.na(V_total_deadwood ))  %>% 
  summarize(mean_DW = mean(V_total_deadwood, na.rm = T)) %>%  
  mutate(norm_DW = (mean_DW /mean(mean_DW [regime == "BAU"])) - 1)  %>%
  select(id, climChange, regime, norm_DW) %>% 
  ungroup()



# Join tables for Combined HSI and DW values -----------------------------------


windows(height = 3.5, width=7)

p3 <- left_join(df_comb_HSI, df_DW) %>% 
  pivot_longer(!c(id, regime, climChange), #everything(vars = NULL),
               names_to = "Indicator", 
               values_to = "perc_ch")  %>%
  mutate(Indicator = factor(Indicator, 
                            levels = c('norm_DW', 'norm_HSI'),
                            labels = c('Deadwood volume', 'Combined HSI' ))) %>% 
  filter(regime != 'BAU') %>% 
  ggplot(aes(x = regime,
             y = perc_ch*100,
             fill = climChange)) + 
  stat_summary(geom = 'bar', 
               fun = 'mean',
               # width = .4,
               position = 'dodge') +
  stat_summary(geom = 'errorbar', 
               #width = .4,
               fun.data = mean_cl_normal,# mean_sdl, #, 
               fun.args=list(mult = 3), 
               position = 'dodge') +
  geom_hline(yintercept = 0) +
  coord_flip() +
  facet_grid(.~ Indicator, scales = 'free') +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"), 
                    name="Climate change") +
  ylab("Difference from BAU scenario [%]") +
  xlab(lab_manag) +
  theme_bw()  + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        #legend.position = 'bottom',
        legend.position = c(.40, .22),  # legend position within the plot, x, y
        legend.title = element_text(size=10),
        legend.text  = element_text(size=8),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.box.background = element_rect(colour = "black")) 



ggsave(filename = 'Fig_3.pdf',
       #plot = p2, #last_plot(),
       device = 'pdf',
       path = paste(getwd(), outFigs, sep = '/'),
       width = 7,
       height = 3.5,
       units = c("in"),
       dpi = 300#,
)








# Plot for Combined HSI and Deadwood volume



# ------------------------------------
# Economic consequences:
# ------------------------------------







# Make bar plot of changes in biodiversity indicators given regime and climate change --------
df.ind.diff <-  
df.out %>% 
  group_by(climChange, regime) %>% # modif, #geo_grad,
  summarise(mean_risk    = mean(windRisk, na.rm = T),
            mean_CAPER   = mean(CAPERCAILLIE, na.rm = T),
            mean_HAZ     = mean(HAZEL_GROUSE, na.rm = T),
            mean_THREE   = mean(THREE_TOED_WOODPECKER, na.rm = T),
            mean_LESSER  = mean(LESSER_SPOTTED_WOODPECKER, na.rm = T),
            mean_TIT     = mean(LONG_TAILED_TIT, na.rm = T),
            mean_SQIRR   = mean(SIBERIAN_FLYING_SQUIRREL, na.rm = T),
            mean_DW      = mean(V_total_deadwood, na.rm = T),
            mean_HSI     = mean(COMBINED_HSI, na.rm = T)
  )  %>%
  
  mutate(#control_risk    = mean_risk[match('BAU', regime)],
    control_CAPER   = mean_CAPER[match('BAU', regime)],
    p_change_CAPER  = mean_CAPER /control_CAPER * 100 - 100,
    #p_change_risk   = mean_risk/control_risk * 100 - 100,
    control_HAZ     = mean_HAZ[match('BAU', regime)],
    p_change_HAZ    = mean_HAZ /control_HAZ * 100 - 100,
    control_THREE   = mean_THREE[match('BAU', regime)],
    p_change_THREE  = mean_THREE /control_THREE * 100 - 100,
    control_LESSER  = mean_LESSER[match('BAU', regime)],
    p_change_LESSER = mean_LESSER /control_LESSER * 100 - 100,
    control_TIT     = mean_TIT[match('BAU', regime)],
    p_change_TIT    = mean_TIT /control_TIT * 100 - 100,
    control_SQIRR   = mean_SQIRR[match('BAU', regime)],
    p_change_SQIRR  = mean_SQIRR /control_SQIRR * 100 - 100,
    control_DW      = mean_DW[match('BAU', regime)],
    p_change_DW     = mean_DW /control_DW * 100 - 100,
    control_HSI     = mean_HSI[match('BAU', regime)],
    p_change_HSI    = mean_HSI /control_HSI * 100 - 100) %>% 
  # print(n = 40) 
  filter(regime != "BAU")  %>%    # remove BAU from teh table
  dplyr::select(c(climChange, regime,
                  p_change_CAPER, 
                  p_change_HAZ,
                  p_change_THREE,
                  p_change_LESSER, 
                  p_change_TIT,
                  p_change_SQIRR,
                  p_change_DW)) %>%
  pivot_longer(!c(regime, climChange), #everything(vars = NULL),
               names_to = "Indicator", 
               values_to = "perc_ch")  #%>%

# Bar plot for indicators
#windows()
df.ind.diff %>% 
  ggplot(aes(y=perc_ch, 
             x=regime,
             fill = Indicator)) + 
  geom_bar(position="dodge", 
           stat="identity") +
  facet_grid(.~ climChange) +
  # scale_fill_manual(values=c("#E69F00", "#56B4E9"), 
  #                  name="Timber quality",
  #                 breaks=c("perc_change_log", "perc_change_pulp"),
  #                labels=c("Log", "Pulp")) +
  ylab("Difference in indicator [%]") +
  xlab(lab_manag) + 
 # ylim(-1000,10000) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = 'bottom')# +



# -----------------------------------------------
# Summary table risk, HSI, harvested volume      
# -----------------------------------------------

# Need to update harvested volume to first calculate the harvested volume from stand-by id (measure by m3/ha)
# then calculate their means

# Have three steps: 

# Calculate first sums and means for harvested volume:




df.timber <- df.out %>% 
  group_by(id, climChange, regime) %>% # modif, #geo_grad,
  summarise(sum_V_log     = sum(Harvested_V_log, na.rm = T),
            sum_V_pulp    = sum(Harvested_V_pulp, na.rm = T))  %>%
  ungroup() %>% 
  group_by(climChange, regime) %>% 
  summarise(mean_sum_V_log     = round(mean(sum_V_log, na.rm = T),0),
            sd_sum_V_log       = round(sd(sum_V_log, na.rm = T),0),
            mean_sum_V_pulp    = round(mean(sum_V_pulp, na.rm = T),0),
            sd_sum_V_pulp      = round(sd(sum_V_pulp, na.rm = T), 0)) 


# Second, get the summary table:

df_summary_main <-
  df.out %>% 
  left_join(df.NPV) %>%   # df.NPV if a raw data! for all 100 year
  group_by(climChange, regime) %>% 
  summarise(windRisk_mean = round(mean(windRisk, na.rm = T)*100, digits = 1),
            windRisk_sd   = round(sd(windRisk, na.rm = T)*100, digits = 1),
            HSI_mean      = round(mean(COMBINED_HSI, na.rm = T), digits = 1),
            HSI_sd        = round(sd(COMBINED_HSI, na.rm = T), digits = 1),
            DW_mean       = round(mean(V_total_deadwood, na.rm = T), digits = 1),
            DW_sd         = round(sd(V_total_deadwood, na.rm = T), digits = 1),
            NPV_mean       = round(mean(NPV, na.rm = T), digits = 0),
            NPV_sd         = round(sd(NPV, na.rm = T), digits = 0)) %>% 
  left_join(df.timber)

# Get the mean values for NPV: !!!
df.NPV %>% 
  group_by(climChange, regime) %>% 
  summarise(NPV_mean       = round(mean(NPV, na.rm = T), digits = 0),
            NPV_sd         = round(sd(NPV, na.rm = T), digits = 0)) %>%
  print(n = 40)
  



# Third, format output table
formated_df_main <- 
  df_summary_main %>% 
  mutate(WindDamage    = stringr::str_glue("{windRisk_mean}±{windRisk_sd}"),
         Combined_HSI  = stringr::str_glue("{HSI_mean}±{HSI_sd}"),
         Deadwood      = stringr::str_glue("{DW_mean}±{DW_sd}"),
         Harvested_log = stringr::str_glue("{mean_sum_V_log}±{sd_sum_V_log}"),
         Harvested_pulp= stringr::str_glue("{mean_sum_V_pulp}±{sd_sum_V_pulp}"),
         NPV           = stringr::str_glue("{NPV_mean}±{NPV_sd}"),
  ) %>%  #,  {scales::percent(sd_height)}
  dplyr::select(regime, climChange, WindDamage, Deadwood,
                Combined_HSI, Harvested_log, Harvested_pulp, NPV)



# -----------------------------------------------------------------------------
# Get mean stand-level total volume harvested over Finland given scenario and climate change
df.out %>% 
  group_by(climChange, regime) %>% # modif, #geo_grad,
  summarise(sum_V_log     = sum(Harvested_V_log, na.rm = T),
            sum_V_pulp    = sum(Harvested_V_pulp, na.rm = T),
            sum_V = sum_V_pulp + sum_V_pulp) %>% 
  ggplot(aes(y = sum_V/1000000,
             x = regime,
             group = climChange,
             color = climChange)) +
  geom_line()



# Make barplots with +- sd for some indicators from teh summary table??? -------
# log:
windows(7,3)
df_summary_main %>% 
  ggplot(aes(x = regime,
             y = mean_sum_V_log,
             fill = climChange,
             color = climChange)) + 
  geom_col(position="dodge2") +
  geom_errorbar(aes(x=regime, 
                    ymin=mean_sum_V_log-sd_sum_V_log, 
                    ymax=mean_sum_V_log+sd_sum_V_log), 
                width=0.2, colour="grey10", 
                alpha=0.9, size=0.5,
                position=position_dodge(.9))# +
#geom_point() #+


# Pulp:
windows(7,3)
df_summary_main %>% 
  ggplot(aes(x = regime,
             y = mean_sum_V_pulp,
             fill = climChange,
             color = climChange)) + 
  geom_col(position="dodge2") +
  geom_errorbar(aes(x=regime, 
                    ymin=mean_sum_V_pulp-sd_sum_V_pulp, 
                    ymax=mean_sum_V_pulp+sd_sum_V_pulp), 
                width=0.2, colour="grey10", 
                alpha=0.9, size=0.5,
                position=position_dodge(.9))# +
#geom_point() #+




# Summary table biodiversity Indicators ------------------------------------------------------
df.out %>% 
  group_by(climChange, regime) %>% # modif, #geo_grad,
  summarise(mean_CAPER   = round(mean(CAPERCAILLIE, na.rm = T), 1),
            # sd_CAPER     = round(sd(CAPERCAILLIE, na.rm = T), 1),
            mean_HAZ     = round(mean(HAZEL_GROUSE, na.rm = T), 1),
            #sd_HAZ       = round(sd(HAZEL_GROUSE, na.rm = T), 1),
            mean_THREE   = round(mean(THREE_TOED_WOODPECKER, na.rm = T),  1),
            #  sd_THREE       = round(sd(THREE_TOED_WOODPECKER, na.rm = T), 1),
            mean_LESSER  = round(mean(LESSER_SPOTTED_WOODPECKER, na.rm = T), 1),
            #  sd_HAZ       = round(sd(LESSER_SPOTTED_WOODPECKER, na.rm = T), 1),
            mean_TIT     = round(mean(LONG_TAILED_TIT, na.rm = T), 1),
            # sd_HAZ       = round(sd(LONG_TAILED_TIT, na.rm = T), 1),
            mean_SQIRR   = round(mean(SIBERIAN_FLYING_SQUIRREL, na.rm = T), 1)#,
            #sd_HAZ       = round(sd(SIBERIAN_FLYING_SQUIRREL, na.rm = T), 1),
            #mean_DW      = round(mean(V_total_deadwood, na.rm = T), 1),
            #sd_DW        = round(sd(V_total_deadwood, na.rm = T), 1)
  ) 



# Get geom_tile:
windows(7, 3)
df.out %>% 
  group_by(climChange, regime) %>% # modif, #geo_grad,
  summarise(mean_CAPER   = round(mean(CAPERCAILLIE, na.rm = T), 1),
            mean_HAZ     = round(mean(HAZEL_GROUSE, na.rm = T), 1),
            mean_THREE   = round(mean(THREE_TOED_WOODPECKER, na.rm = T),  1),
            mean_LESSER  = round(mean(LESSER_SPOTTED_WOODPECKER, na.rm = T), 1),
            mean_TIT     = round(mean(LONG_TAILED_TIT, na.rm = T), 1),
            mean_SQIRR   = round(mean(SIBERIAN_FLYING_SQUIRREL, na.rm = T), 1)#,
            #mean_DW      = round(mean(V_total_deadwood, na.rm = T), 1),
            #sd_DW        = round(sd(V_total_deadwood, na.rm = T), 1)
  ) %>%  
  gather(key="HSI",
         value="value",mean_CAPER:mean_SQIRR)   %>%
  ggplot(aes(x = regime,
             y = HSI)) + 
  geom_tile(aes(fill = value)) +
  #scale_fill_viridis_c(limits = c(0,0.5), direction = -1) +
  scale_fill_gradient(low = "white", 
                      high = "red", 
                      na.value = NA,
                      name = "HSI") +
  facet_grid(.~climChange) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))





# bar plot for indicators means:
df.ind.diff2 <- 
  df.out %>% 
  group_by(climChange, regime) %>% # modif, #geo_grad,
  summarise(mean_risk    = mean(windRisk, na.rm = T),
            mean_CAPER   = mean(CAPERCAILLIE, na.rm = T),
            mean_HAZ     = mean(HAZEL_GROUSE, na.rm = T),
            mean_THREE   = mean(THREE_TOED_WOODPECKER, na.rm = T),
            mean_LESSER  = mean(LESSER_SPOTTED_WOODPECKER, na.rm = T),
            mean_TIT     = mean(LONG_TAILED_TIT, na.rm = T),
            mean_SQIRR   = mean(SIBERIAN_FLYING_SQUIRREL, na.rm = T),
            # mean_DW      = mean(V_total_deadwood, na.rm = T),
            mean_HSI     = mean(COMBINED_HSI, na.rm = T)
  )  %>%
  #filter(regime != "BAU")  %>%    # remove BAU from teh table
  dplyr::select(c(climChange, regime,
                  mean_CAPER, 
                  mean_HAZ,
                  mean_THREE,
                  mean_LESSER, 
                  mean_TIT,
                  mean_SQIRR)) %>%
  pivot_longer(!c(regime, climChange), #everything(vars = NULL),
               names_to = "Indicator", 
               values_to = "perc_ch")  #%>%

# Bar plot for indicators
df.ind.diff2 %>% 
  ggplot(aes(y=perc_ch, 
             x=regime,
             fill = Indicator)) + 
  geom_bar(position="dodge", 
           stat="identity") +
  facet_grid(.~ climChange) +
  ylab("Indicator [mean]") +
  xlab(lab_manag) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = 'bottom')# +








# ------------------------------------------------------------------------------
# 
#                      Habitat availability
# 
# ------------------------------------------------------------------------------



# What is habitat availability for each species ??  ----------------------------

# predict the area of each stand across Finland based on its location in tile

# The extend of tiles: 12 tiles from N to S
# size of the stand: 
# N = 4*4 pixels, 16 pixels in total , 16 m resolution
# S = 3*3 pixels, 9pixels, in total, 16 m resolution

n_area = (4*16)^2/10000
s_area = (3*16)^2/10000
tiles = 13  # 13 includes k and x tiles

# Interpolate the value between north and souths: from 2 ha (South) to 6.5 ha (North)
increase = (n_area-s_area)/tiles

# Check calculation: correct! 
2+increase*tiles

# get a vector of increasing values:
vec = 1:(tiles-2)
s_area+increase*vec

# Predict approaximate stand size for each tiles:
# the area is in hectares
df_area = data.frame(text = c('k',
                              'l',
                              'm',
                              'n',
                              'p',
                              'q',
                              'r',
                              's',
                              't',
                              'u',
                              'v',
                              'w',
                              'x'),
                     stand_area = c(s_area, 
                                    s_area+increase*vec, 
                                    n_area))


# DO NOT RUN!!! 

# Add predcited stand area to the original table
# first separate the letters from numbers in 'cell' field;
# then join the 
df.habit <- df.out %>%
 # dplyr::select(-c(magnit, since_thin, timeEffect)) %>% 
  separate(cell, 
           into = c("text", "num"), 
           sep = "(?<=[A-Za-z])(?=[0-9])"
  )


# RUN FROM HERE:

# Read table
df.habit <- data.table::fread(paste(inPath, inFolder, outHabitat, sep = "/"))

# Export table
#data.table::fwrite(df.habit, paste(inPath, inFolder, outHabitat, sep = "/"))

# Join tables together
df.habit <- df.habit %>%
  left_join(df_area)



# Order by regimes & climate change -----------------------------------------------------
df.habit <- df.habit %>% 
  mutate(regime = factor(regime, 
                         levels = regime_levels))

# Climate change
df.habit <- df.habit %>% 
  mutate(climChange = factor(climChange, 
                           levels = clim_levels))




# Get sum of forest covered by the NFI data: 
# Calculate how much forests covered by NFI grid we have? ~8.000 ha
# Get sum of forest for the all years
(sum_forest_all = 
    df.habit %>%
    filter(# year == '2016' & 
              climChange == 'RCP85' & 
              regime == 'CCF')  %>%  # filter only one regime #  &
    summarize(sum_forest = sum(stand_area)) %>% 
    pull())

# In total there is 161.000 hectares of forest over 10 years


# get counts of stand numbers per regimes
# df.habit %>% 
#   group_by(regime, climChange, year) %>%
#   summarize(area = sum(stand_area) )  %>% 
#   #distinct(id) %>% 
#   #tally() %>% 
#   print(n = 800)
# 
# Seems that CCF and ext_30 have more stands than BAU: CCF under RCP85: 8092, BAu under REf: 7891



# Calculate sum of all HSI >0.7 for CCF
#df.habit %>% 
#  filter(regime == "CCF"  & year == 2016 & climChange == 'REF' & COMBINED_HSI > 0.0) # 


# get Combined HSI for 0.0 and 0.7
# FOR HSI > 0
df.combHSI <- 
  df.habit %>% 
  mutate(comb_HSI00  = ifelse(COMBINED_HSI > 0,   1, 0),
         comb_HSI07  = ifelse(COMBINED_HSI > 0.7, 1, 0)) %>% 
  select(c(id, 
           year, 
           regime, 
           climChange, 
           comb_HSI00,
           comb_HSI07,                      #stand_area,
           text)) #%>% 
   # group_by()


df.combHSI %>% 
  group_by(comb_HSI00, climChange, regime) %>% 
  tally() %>% 
  print(n = 60)


# check historrams for CCF HSI values?  CCF has less of the 0 values for HSI than BAU:

df.habit %>% 
  filter(regime == "CCF" | regime == "BAU") %>%
  ggplot(aes(x = COMBINED_HSI,
             color = factor(year))) +
  geom_density() + 
  facet_grid(climChange~regime)
  




# Get the range of HSI : filter only for classified HSI = 1
df.combHSI.long <-
  df.combHSI %>% 
  pivot_longer(!c(id, 
                  year,
                  regime, 
                  climChange, 
                  text),   #everything(vars = NULL),
               names_to = "Indicator", 
               values_to = "HSI")  %>%
  left_join(df_area) %>%    # join area table
  mutate(HSI_area = stand_area*HSI) %>%
  filter(HSI == 1) #%>% 
 # mutate(Indicator = gsub('_', ' ', Indicator))



# Calculate the area once more, this is for all years: for the HSI 0 and HSI >0, HSI > 0.7 -----------------------------
total_area = 
  df.combHSI %>%
  left_join(df_area) %>%   
  filter(regime == "CCF" & climChange == 'RCP85') %>% # this seting has the highest area
   # group_by(Indicator ) %>% 
  summarize(sum_stand_area = sum(stand_area, na.rm = T)) %>% 
  pull()

# Sum total forest area (area covered by NFI plots over 20 years):
# 161831.5 ha over 20 years

# Get summary of area per indicator, this is already filtered for classified HSI == 1  
df.combHSI.long %>%
  group_by(regime, climChange, Indicator) %>% 
    #filter(regime == "CCF" & climChange == 'REF' & Indicator == 'comb_HSI00') %>% 
  #group_by(HSI) %>% 
    #filter(HSI == 1) %>% 
    summarize(sum_HSI = sum(HSI_area, na.rm = T))   %>% 
  print(n = 60)

# For BAU, REF:  
# comb_HSI00: 107791 ha
# comb_HSI07:  34585 ha
  

# For CCF, REF: 
# comb_HSI00: 159559 ha
# comb_HSI07:  59076 ha
# Make a plot for the Comb_HSI

# Create facet labels: first make labels, than link them with their original 'names'
# https://www.datanovia.com/en/blog/how-to-change-ggplot-facet-labels/#:~:text=(dose%20~%20supp)-,Change%20the%20text%20of%20facet%20labels,labeller%20function%20label_both%20is%20used.
comb_HSI_labs = c('Combined HSI > 0.0', 'Combined HSI > 0.7')
names(comb_HSI_labs) <- c("comb_HSI00", "comb_HSI07")


windows(width = 7, height = 3.5)
#p_comb_HSI <-
  df.combHSI.long %>% 
  group_by(regime, climChange, Indicator) %>% 
  summarize(sum_HSI = sum(HSI_area))  %>%
  # print(n = 60)
  ggplot(aes(x = regime,
             y = sum_HSI/20, #/sum_forest_all*100, #/20, #/sum_forest*100,
             fill = climChange)) +
  # ylab('Available habitat [mean, %]') + 
  #ggtitle('a) HSI > 0.0') +
  pt_fill_cols() +
 
  scale_y_continuous(
    "Available habitats [ha]", 
    sec.axis = sec_axis(~ . /total_area*100*20,  # multiply by 20 because it links directly to the y1 axis
                        name = "Available habitat [%]")
  ) +
    theme(legend.position = 'bottom') +
    facet_wrap(.~Indicator, scales = 'free', 
               labeller = labeller(Indicator =comb_HSI_labs )) # +


  
  
  
  
# Classify habitat suitability: HSI > 0.7 is suitable
# the calculate, how much habitat is available for particullar species:

#"CAPERCAILLIE"              "HAZEL_GROUSE"              "THREE_TOED_WOODPECKER"    
#"LESSER_SPOTTED_WOODPECKER" "LONG_TAILED_TIT"           "SIBERIAN_FLYING_SQUIRREL" 


# get wtwo thresholds: HSi = 0, HSI = 0.7

# FOR HSI > 0
df.habit00 <- df.habit %>% 
  mutate( CAPERCAILLIE              = ifelse(CAPERCAILLIE > 0, 1, 0),
          HAZEL_GROUSE              = ifelse(HAZEL_GROUSE > 0, 1, 0),
          THREE_TOED_WOODPECKER     = ifelse(THREE_TOED_WOODPECKER > 0, 1, 0),
          LESSER_SPOTTED_WOODPECKER = ifelse(LESSER_SPOTTED_WOODPECKER > 0, 1, 0),
          LONG_TAILED_TIT           = ifelse(LONG_TAILED_TIT > 0, 1, 0),
          SIBERIAN_FLYING_SQUIRREL  = ifelse(SIBERIAN_FLYING_SQUIRREL > 0, 1, 0)) %>% 
  select(c(regime, climChange, 
           CAPERCAILLIE,
           HAZEL_GROUSE,
           THREE_TOED_WOODPECKER,
           LESSER_SPOTTED_WOODPECKER, 
           LONG_TAILED_TIT, 
           SIBERIAN_FLYING_SQUIRREL, 
           #stand_area,
           text))



# FOR HSI > 0.7

df.habit07 <- df.habit %>% 
  mutate( CAPERCAILLIE              = ifelse(CAPERCAILLIE > 0.7, 1, 0),
          HAZEL_GROUSE              = ifelse(HAZEL_GROUSE > 0.7, 1, 0),
          THREE_TOED_WOODPECKER     = ifelse(THREE_TOED_WOODPECKER > 0.7, 1, 0),
          LESSER_SPOTTED_WOODPECKER = ifelse(LESSER_SPOTTED_WOODPECKER > 0.7, 1, 0),
          LONG_TAILED_TIT           = ifelse(LONG_TAILED_TIT > 0.7, 1, 0),
          SIBERIAN_FLYING_SQUIRREL  = ifelse(SIBERIAN_FLYING_SQUIRREL > 0.7, 1, 0)) %>% 
  select(c(regime, climChange, 
           CAPERCAILLIE,
           HAZEL_GROUSE,
           THREE_TOED_WOODPECKER,
           LESSER_SPOTTED_WOODPECKER, 
           LONG_TAILED_TIT, 
           SIBERIAN_FLYING_SQUIRREL, 
           #stand_area,
           text))




# Convert from wide to long;
# replace the "_ " by the space
df.habit.long00 <- df.habit00 %>% 
  pivot_longer(!c(regime, climChange, text), #everything(vars = NULL),
               names_to = "Indicator", 
               values_to = "HSI")  %>%
  left_join(df_area) %>% # join area table
  mutate(HSI_area = stand_area*HSI) %>%
  filter(HSI == 1) %>% 
  mutate(Indicator = gsub('_', ' ', Indicator))


# for HSI > 0.7
df.habit.long07 <- df.habit07 %>% 
  pivot_longer(!c(regime, climChange, text), #everything(vars = NULL),
               names_to = "Indicator", 
               values_to = "HSI")  %>%
  left_join(df_area) %>% # join area table
  mutate(HSI_area = stand_area*HSI) %>%
  filter(HSI == 1) %>% 
  mutate(Indicator = gsub('_', ' ', Indicator))



# Get the total forest area for each regime abnd climChange:
# this value is for all stands across 20 years and 
sum_forest = df.habit %>%
  filter( year == '2016' & climChange == 'RCP85' & regime == 'CCF')  %>%  # filter only one species #  &
  summarize(sum_forest = sum(stand_area)) %>% 
  pull()

# Final forest cover: 22157000 ha (22.157.000 ha)
# sum for 1 year : 57105.45   # /22157000
# sum for 20 years: 

# Summarize the table by categories, make plots: for HSI > 0.0, for HSI > .7
pt_fill_cols <- function() {
  list(  
    geom_col(position = "dodge"),
    scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"), 
                             name="Climate change"), 
    facet_wrap(.~Indicator, scales = 'free', 
               labeller = label_wrap_gen(10)), # +
    xlab(lab_manag),#  +
    theme_bw(), #  + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                 legend.title = element_text(size=10),
                 legend.text  = element_text(size=8),
                 legend.background = element_rect(fill = "white", color = "white"),
                 legend.box.background = element_rect(colour = "white")) 
  )
}



#windows()
p_00 <-
  df.habit.long00 %>% 
  group_by(regime, climChange, Indicator, HSI) %>% 
  summarize(sum_HSI = sum(HSI_area, na.rm = T)) %>%
  ggplot(aes(x = regime,
             y = sum_HSI/20, #/sum_forest*100,
             fill = climChange)) +
  ggtitle('a) HSI > 0.0') +
  pt_fill_cols() +
  scale_y_continuous(
    "Available habitats [ha]", 
    sec.axis = sec_axis(~ . / sum_forest*100, 
                        name = "Available habitat [%]")
  )
  
  

  

p_07 <-
  df.habit.long07 %>% 
  group_by(regime, climChange, Indicator, HSI) %>% 
  summarize(sum_HSI = sum(HSI_area)) %>%
  ggplot(aes(x = regime,
             y = sum_HSI/20, 
             fill = climChange)) +
  ggtitle('b) HSI > 0.7') +
  pt_fill_cols() +
  scale_y_continuous(
    "Available habitat [ha]", 
    sec.axis = sec_axis(~ . / sum_forest*100, 
                        name = "Available habitat [%]")
  )



windows(7.5, 12)
ggarrange(p_00, p_07, 
           ncol = 1, #nrow = 2,
          common.legend = TRUE)








# 
# wind damage risk  vs individual species across all climate scenarios !-------------------------

# CAPERCAILLIE  
# HAZEL_GROUSE
# LESSER_SPOTTED_WOODPECKER
# SIBERIAN_FLYING_SQUIRREL
# LONG_TAILED_TIT
# THREE_TOED_WOODPECKER


windows(width = 7, height = 3)

df.species.means <- 
  df.out %>% 
  group_by(regime) %>% # modif, #geo_grad,
  summarise(mean_risk    = mean(windRisk, na.rm = T),
            mean_CAPER   = mean(CAPERCAILLIE, na.rm = T),
            mean_HAZ     = mean(HAZEL_GROUSE, na.rm = T),
            mean_THREE   = mean(THREE_TOED_WOODPECKER, na.rm = T),
            mean_LESSER  = mean(LESSER_SPOTTED_WOODPECKER, na.rm = T),
            mean_TIT     = mean(LONG_TAILED_TIT, na.rm = T),
            mean_SQIRR   = mean(SIBERIAN_FLYING_SQUIRREL, na.rm = T)
  )  %>%
  mutate(control_risk    = mean_risk[match('BAU', regime)],
         p_change_risk   = mean_risk/control_risk * 100 - 100,
         
         control_CAPER   = mean_CAPER[match('BAU', regime)],
         p_change_CAPER  = mean_CAPER /control_CAPER * 100 - 100,
        
         control_HAZ     = mean_HAZ[match('BAU', regime)],
         p_change_HAZ    = mean_HAZ /control_HAZ * 100 - 100,
         
         control_THREE   = mean_THREE[match('BAU', regime)],
         p_change_THREE  = mean_THREE /control_THREE * 100 - 100,
         
         control_LESSER  = mean_LESSER[match('BAU', regime)],
         p_change_LESSER = mean_LESSER /control_LESSER * 100 - 100,
         
         control_TIT     = mean_TIT[match('BAU', regime)],
         p_change_TIT    = mean_TIT /control_TIT * 100 - 100,
         
         control_SQIRR   = mean_SQIRR[match('BAU', regime)],
         p_change_SQIRR  = mean_SQIRR /control_SQIRR * 100 - 100) %>% 
  dplyr::filter(regime != 'BAU')





# plot XY scatter plots by regimes and fill with species --------------------

# Make my own gradient scheme:

#breaks <- c(levels(df.out$regime))

# !!!!! check if need to update a point plot for the climChange effect on individual species??
pt_details <- function() {
  list(
    ylab(''),
    xlab(''),
    geom_vline(xintercept = 0, color = "grey70", lty = "dashed"), 
    geom_hline(yintercept = 0, color = "grey70", lty = "dashed"),
    geom_point(size = 3, shape = 21, color = 'grey'), # + 'black', shape 21
    scale_fill_manual(values  = my_cols_RdGn,
                      name= lab_manag),
    scale_color_manual(values = my_cols_RdGn,
                       name= lab_manag),
    theme_bw(),
    theme(plot.title = element_text(size = 9, face = "plain"),
          axis.title  = element_text(size = 9, face="plain", family = "sans"),
          axis.text.x = element_text(angle = 90, vjust = 0.5, face="plain", size = 9, family = "sans"),
          axis.text.y = element_text(face="plain", size = 8, family = "sans"),
          legend.position = "right",
          strip.background =element_rect(fill="white", 
                                         color = NA),
          panel.grid.major = element_line(size = 0, 
                                          linetype = 'dotted',
                                          colour = NA),
          panel.grid.minor = element_line(size = 0, 
                                          linetype = 1,
                                          colour = NA))
  )
}

# Get individual plots for species -------------------------------------
my_lab_risk = c("Difference in\nwind damage risk [%]")

my_cols_RdGn <- c(
  '#b10026', # dark red
  '#fd8d3c',
  '#0868ac',  # blue
  #  '#ffeda0',  # bright red
  '#f7fcb9',
  '#addd8e',
  '#41ab5d',
  '#005a32'  # dark green
)


library(RColorBrewer)


# Get individual species specific plots -----------------------------------

p1 <- 
  df.species.means %>% 
  ggplot(aes(x = p_change_CAPER,
             y = p_change_risk,
             fill = regime)) + 
  ggtitle("a) Capercaillie\n") +
  pt_details() +
  ylab(my_lab_risk)


p2 <- df.species.means %>% 
  ggplot(aes(x = p_change_HAZ,
             y = p_change_risk,
             fill = regime)) +
  pt_details() +
  ggtitle("b) Hazel grouse\n")

p3 <- df.species.means %>% 
  ggplot(aes(x = p_change_THREE,
             y = p_change_risk,
             fill = regime)) +
  pt_details() +
  ggtitle("c) Three toed\nwoodpecker")

p4 <- df.species.means %>% 
  ggplot(aes(x = p_change_LESSER,
             y = p_change_risk,
             fill = regime)) +
  pt_details() +
  ggtitle("d) Lesser spotted\nwoodpecker") +
  ylab(my_lab_risk)

p5 <- df.species.means %>% 
  ggplot(aes(x = p_change_TIT,
             y = p_change_risk,
             fill = regime)) +
  pt_details() +
  ggtitle("e) Long tailed tit\n")

p6 <- df.species.means %>% 
  ggplot(aes(x = p_change_SQIRR,
             y = p_change_risk,
             fill = regime)) +
  pt_details() +
  ggtitle("f) Siberian flying\nsquirrel")



species.plot <- ggarrange(p1,p2,p3,p4,p5,p6, 
                          widths = c(1.05,1, 1, 1.05,1,1),
                          ncol = 3,nrow = 2,
                          common.legend = TRUE,
                          legend = 'bottom')

windows(7,5)  
# https://rpkgs.datanovia.com/ggpubr/reference/annotate_figure.html
p_4 <- annotate_figure(species.plot,
                bottom = text_grob("Difference in HSI [%]", 
                                   color = "black",
                                   hjust = 0.5, 
                                   #x = -1, 
                                   y = 4.5,
                                   face = "plain", size = 9)
)


ggsave(filename = 'Fig_4.pdf',
       #plot = p2, #last_plot(),
       device = 'pdf',
       path = paste(getwd(), outFigs, sep = '/'),
       width = 7,
       height = 5,
       units = c("in"),
       dpi = 300#,
)






# Get species& clim change specific plots ------------------------------------------


# Calculate total sum of harvested timber given scenarios --------------

df.vol <- 
  df.out %>% 
  filter_at(vars(Harvested_V_log, Harvested_V_pulp), all_vars(!is.na(.))) %>%  # remove rows with NA in harvested volume
  dplyr::select(id, regime, climChange, Harvested_V_log, Harvested_V_pulp) %>%
  group_by(id, regime, climChange) %>% 
  summarize(sum_harv_V_log  = sum(Harvested_V_log, na.rm = T),
            sum_harv_V_pulp = sum(Harvested_V_pulp, na.rm = T),
            sum_harv_V = sum_harv_V_log + sum_harv_V_pulp) #%>% 


# how much was harvested at average from each id? 
df.vol2 <- df.vol %>%  
  group_by(regime, climChange) %>% 
  summarize(totalV_mean    = mean(sum_harv_V),
            totV_log_mean  = mean(sum_harv_V_log),
            totV_pulp_mean = mean(sum_harv_V_pulp)) %>%
  gather(harv_type, 
         value, 
         totalV_mean:totV_pulp_mean, factor_key=TRUE)# %>%



# stacked area chart for harvested timber volume
windows(width = 7, height = 2.5)
df.vol2 %>% 
  filter(harv_type != 'totalV_mean') %>%
  ggplot(aes(x=regime, 
             y=value, 
             fill=harv_type)) + 
  geom_area(aes(color = harv_type, 
                group = harv_type)) +
  scale_fill_discrete(name = "Timber type", labels = c("Log", "Pulp")) +
  scale_color_discrete(name = "Timber type", labels = c("Log", "Pulp")) +
  facet_grid(.~climChange) +
  ylab('Harvested timber volume [m3/ha]') +
  theme(text = element_text(size=8, 
                            face="plain"),
        axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, 
                                   #hjust = -0.5,
                                   face="plain", 
                                   size = 8, 
                                   family = "sans"),
        panel.background = element_rect(fill = "white", colour = "black"),
        panel.border = element_rect(linetype = "solid", fill = NA))





# -------------------------------
# Export data for map
# -------------------------------

# Calculate the % change to plot on the map -----------------
# Reference: BAU
# Export file to check it with map ------------------------------
df.plot <- 
  df.out %>% 
  group_by(id, climChange, regime) %>% 
  summarise(mean_risk = mean(windRisk, na.rm = T),
            mean_HSI  = mean(COMBINED_HSI, na.rm = T))  %>%
  mutate(control_risk = mean_risk[match('BAU', regime)],
         control_HSI  = mean_HSI[ match('BAU', regime)],
         perc_change_HSI  = mean_HSI /control_HSI  * 100 - 100,
         perc_change_risk = mean_risk/control_risk * 100 - 100)# %>%
#data.table::fwrite(paste(inPath, "output/plotting", 'id_1000.csv', sep = "/"))











# What is the age at harvest???? ---------------------------------------------

# inspect at one stand;
df.out %>% 
  filter(id == '101000107' & climChange == "RCP85") %>% 
  filter(regime == "BAU" | regime == 'noThin') %>% 
  dplyr::select(year, Age, V_total_deadwood, climChange, regime) %>% 
  arrange(year) %>% 
  ggplot(aes(x = year, 
             y = V_total_deadwood,
             color = regime)) +
  geom_line()

# Try another random point:
df.out %>% 
  filter(id == '500102306' & climChange == "RCP85") %>% 
  filter(regime == "BAU" | regime == 'noThin' | regime == 'ext_30') %>% 
  dplyr::select(year, Age, V_total_deadwood, climChange, regime) %>% 
  arrange(year) %>% 
  ggplot(aes(x = year, 
             y = Age,
             color = regime)) +
  geom_line() +
  facet_grid(.~regime)

windows()
df.out %>% 
  filter(id == '500102306' & climChange == "RCP85") %>% 
  filter(regime == "BAU" | regime == 'noThin' | regime == 'ext_30') %>% 
  dplyr::select(year, Age, V_total_deadwood, climChange, regime) %>% 
  arrange(year) %>% 
  ggplot(aes(x = year, 
             y = V_total_deadwood,
             color = regime)) +
  geom_line() +
  facet_grid(.~regime)


# volume
df.out %>% 
  filter(id == '500102306' & climChange == "RCP85") %>% 
  filter(regime == "BAU" | regime == 'noThin' | regime == 'ext_30') %>% 
  dplyr::select(year, Age, V_total_deadwood, climChange, regime, V) %>% 
  arrange(year) %>% 
  ggplot(aes(x = year, 
             y = V,
             color = regime)) +
  geom_line() +
  facet_grid(.~regime)



# Try for another stand id: 101000407
my_id = '101400206'  # age 0 at 2016

windows()
df.out %>% 
  filter(id == my_id & climChange == "REF") %>% 
  filter(regime == "BAU" | regime == 'noThin' ) %>% 
  dplyr::select(year, Age, V_total_deadwood, climChange, regime) %>% 
  arrange(year)# %>% 
  ggplot(aes(x = year, 
             y = Age,
             color = regime)) +
  geom_line() #+
  facet_grid(.~regime)
  
  
# Is the shift visible for stands that are older at the 2016, and under REF85? 
# Get random stand id that is old in 2016
df.out %>% 
  filter(year == 2016 & Age > 90)# %>% 
  distinct(id) 


df.out %>% filter(id == 101400401      )


# 101400401 #  no effect of postponing the harvest even in old stands, between NPV and noThin
  
  windows()
  df.out %>% 
    filter(id == 101400401 & climChange == "RCP85") %>% 
    filter(regime == "BAU" | regime == 'noThin' ) %>% 
    dplyr::select(year, Age, V, climChange, regime) %>% 
    arrange(year) %>% 
  ggplot(aes(x = year, 
             y = Age,# V, #Age, #
             color = regime)) +
    geom_line() #+
  #facet_grid(.~regime)
  
  

windows()
df.out %>% 
  filter(id == my_id & climChange == "RCP85") %>% 
  filter(regime == "BAU" | regime == 'noThin' | regime == 'ext_30') %>% 
  dplyr::select(year, Age, V_total_deadwood, climChange, regime) %>% 
  arrange(year) %>% 
  ggplot(aes(x = year, 
             y = V_total_deadwood,
             color = regime)) +
  geom_line() +
  facet_grid(.~regime)


# volume
df.out %>% 
  filter(id == my_id & climChange == "RCP85") %>% 
  filter(regime == "BAU" | regime == 'noThin' | regime == 'ext_30') %>% 
  dplyr::select(year, Age, V_total_deadwood, climChange, regime, V) %>% 
  arrange(year) %>% 
  ggplot(aes(x = year, 
             y = V,
             color = regime)) +
  geom_line() +
  facet_grid(.~regime)









# Why no thinning has little DW volume?  --------

# maybe it changes over time??
windows()
df.out %>%
  filter(regime == 'noThin' | regime == 'BAU' ) %>%  # | regime == 'ext_30'
  group_by(year, regime,climChange) %>% 
  summarize(mean_dw = mean(V_total_deadwood, na.rm = T),
            sum_dw = sum(V_total_deadwood, na.rm = T)) %>% 
  ggplot(aes(x = year, y = mean_dw, color = regime)) +
  geom_line() +
  facet_grid(.~climChange)

# very little difference in DW volume, why??


# check the age differences between stands?
df.out %>%
  filter(regime == 'noThin' | regime == 'BAU' | regime == 'ext_30') %>% 
  group_by(year, regime,climChange) %>% 
  summarize(mean_age = mean(Age, na.rm = T)) %>% 
  ggplot(aes(x = year, y = mean_age, color = regime)) +
  geom_line() +
  facet_grid(.~climChange)






# Do I have a variation between CC scenarios and sites? -------------------------------------------------------------------
# calculate means of H_dom 
my_shade_pt <- function() {
  list(
    geom_ribbon(
      data = ~ pivot_wider(., 
                           names_from = climChange, 
                           values_from = my_y),
      aes(ymin = no, 
          ymax = cc85, 
          fill = change_time), alpha = 0.2),
    geom_line(aes(y = my_y,
                  color = change_time,     
                  linetype = climChange),
              lwd  = 1),
    scale_linetype_manual(values=c('dotted', 'solid', 'dashed')),
    theme_bw(),
    facet_grid(mainType ~ modif),
    theme(legend.position = 'bottom',
          axis.text.x = element_text(angle = 90, 
                                     vjust = 0.5, 
                                     face="plain", 
                                     size = 9, 
                                     family = "sans"))
  )
}



# Define color scheme
# Color shades: red  
cols_ylRd3 <- c(	'#ff9a00', # yellow 
                 '#ff5a00', # orange
                 '#ff0000' ) #red







############################################################################
#
#                 SUPPLEMENTARY material
#
############################################################################


# Make a bar plot for NPV & timber volume for Supplementary materials

df_harv <- df.out %>% 
  group_by(id, climChange, regime) %>% # modif, #geo_grad,
  summarise(sum_V_log     = sum(Harvested_V_log, na.rm = T),
            sum_V_pulp    = sum(Harvested_V_pulp, na.rm = T))

# Get plot for the timber:
windows(7, 4)
df_harv %>% 
  pivot_longer(!c(id, regime, climChange), #everything(vars = NULL),
               names_to = "Indicator", 
               values_to = "Volume")  %>%
  mutate(Indicator = factor(Indicator, 
                            levels = c('sum_V_log', 'sum_V_pulp'),
                            labels = c('Log', 'Pulp' ))) %>% 
  ggplot(aes(x = regime,
             y = Volume,
             fill = climChange)) + 
  stat_summary(geom = 'bar', 
               fun = 'mean',
               position = 'dodge') +
  stat_summary(geom = 'errorbar', 
               fun.data = mean_cl_normal,# mean_sdl, #, 
               fun.args=list(mult = 3), 
               position = 'dodge') +
  facet_wrap(.~ Indicator) +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"), 
                    name="Climate change") +
  ylab("Harvested timber volume [m3/ha]") +
  xlab(lab_manag) +
  theme_bw()  + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        #legend.position = c(.28, .7), # legend position within the plot, x, y
        legend.position = 'bottom',
        legend.title = element_text(size=10),
        legend.text  = element_text(size=8),
        legend.background = element_rect(fill = "white", color = "white"),
        legend.box.background = element_rect(colour = "black")) 


# Get plot for NPV

# Get plot for the timber:
windows(7, 4)
df.NPV %>% 
  ggplot(aes(x = regime,
             y = NPV,
             fill = climChange)) + 
  stat_summary(geom = 'bar', 
               fun = 'mean',
               position = 'dodge') +
  stat_summary(geom = 'errorbar', 
               fun.data = mean_cl_normal,# mean_sdl, #, 
               fun.args=list(mult = 3), 
               position = 'dodge') +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"), 
                    name="Climate change") +
  ylab("Net present volume [ €/ha]") +
  xlab(lab_manag) +
  theme_bw()  + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = 'bottom',
        legend.title = element_text(size=10),
        legend.text  = element_text(size=8),
        legend.background = element_rect(fill = "white", color = "white"),
        legend.box.background = element_rect(colour = "black")) 




# Get plot for parts of NPV: income  + PV -----------------------------------------

# PLOT Supplementary Compare the NPV calculation by plots: ------------------------------

#windows()
p_PV <- df.NPV %>% 
 # filter(regime == 'BAU' | regime == 'noThin' | regime == 'GTR') %>% 
  ggplot(aes(x = regime,
             y = sum_dist_PV,
             fill = climChange)) +
  geom_boxplot( outlier.shape=NA) +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"), 
                    name="Climate change") +
  ylab("Discounted\npresent value [€/ha]") +
  coord_cartesian(ylim = c(0, 3000)) + 
  xlab(lab_manag) +
  theme_bw()  + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = 'bottom',
        legend.title = element_text(size=10),
        legend.text  = element_text(size=8),
        legend.background = element_rect(fill = "white", color = "white"),
        legend.box.background = element_rect(colour = "black")) 
  
  

p_income <- df.NPV %>% 
  #filter(regime == 'BAU' | regime == 'noThin' | regime == 'GTR') %>% 
  ggplot(aes(x = regime,
             y = sum_dist_income,
             fill = climChange)) +
  geom_boxplot(outlier.shape=NA) +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"), 
                    name="Climate change") +
  ylab("Discounted\nincome [€/ha]") +
  coord_cartesian(ylim = c(0, 20000)) + 
  xlab(lab_manag) +
  theme_bw()  + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = 'bottom',
        legend.title = element_text(size=10),
        legend.text  = element_text(size=8),
        legend.background = element_rect(fill = "white", color = "white"),
        legend.box.background = element_rect(colour = "black")) 

windows(7,3.1)
ggarrange(p_PV, p_income, 
          ncol = 2,
          nrow = 1,
          common.legend = TRUE,
          legend = 'bottom')








# For supplementary material -------------------------------------------------------

# the BAU is a climate change specific - checked

df.species.means.clim <- 
  df.out %>% 
  group_by(regime, climChange) %>% # modif, #geo_grad,
  summarise(mean_risk    = mean(windRisk, na.rm = T),
            mean_CAPER   = mean(CAPERCAILLIE, na.rm = T),
            mean_HAZ     = mean(HAZEL_GROUSE, na.rm = T),
            mean_THREE   = mean(THREE_TOED_WOODPECKER, na.rm = T),
            mean_LESSER  = mean(LESSER_SPOTTED_WOODPECKER, na.rm = T),
            mean_TIT     = mean(LONG_TAILED_TIT, na.rm = T),
            mean_SQIRR   = mean(SIBERIAN_FLYING_SQUIRREL, na.rm = T)
  )  %>%
  ungroup(.) %>% 
  group_by(climChange) %>% 
  mutate(control_risk    = mean_risk[match('BAU', regime)],
         p_change_risk   = mean_risk/control_risk * 100 - 100 ,
         
         control_CAPER   = mean_CAPER[match('BAU', regime)],
         p_change_CAPER  = mean_CAPER /control_CAPER * 100 - 100,
         
         control_HAZ     = mean_HAZ[match('BAU', regime)],
         p_change_HAZ    = mean_HAZ /control_HAZ * 100 - 100,
         
         control_THREE   = mean_THREE[match('BAU', regime)],
         p_change_THREE  = mean_THREE /control_THREE * 100 - 100,
         
         control_LESSER  = mean_LESSER[match('BAU', regime)],
         p_change_LESSER = mean_LESSER /control_LESSER * 100 - 100,
         
         control_TIT     = mean_TIT[match('BAU', regime)],
         p_change_TIT    = mean_TIT /control_TIT * 100 - 100,
         
         control_SQIRR   = mean_SQIRR[match('BAU', regime)],
         p_change_SQIRR  = mean_SQIRR /control_SQIRR * 100 - 100
  ) %>%
  #print(n = 40)
  dplyr::filter(regime != 'BAU')





# get function for scatter plot style
pt_details_clim <- function() {
  list(
    ylab(''),
    xlab(''),
    geom_vline(xintercept = 0, color = "grey70", lty = "dashed"), 
    geom_hline(yintercept = 0, color = "grey70", lty = "dashed"),
    geom_point(size = 2.5, 
               #shape = 21, 
               #color = 'grey'
    ), # + 'black', shape 21
    scale_fill_manual(values  = my_cols_RdGn ,
                      name = lab_manag),
    scale_color_manual(values = my_cols_RdGn,
                       name = lab_manag),
    scale_shape(name = 'Climate scenario'),
    theme_bw(),
    theme(plot.title = element_text(size = 9, face = "plain"),
          axis.title  = element_text(size = 9, face="plain", family = "sans"),
          axis.text.x = element_text(angle = 90, vjust = 0.5, face="plain", size = 9, family = "sans"),
          axis.text.y = element_text(face="plain", size = 8, family = "sans"),
          legend.position = "bottom",
          strip.background =element_rect(fill="white", 
                                         color = NA),
          panel.grid.major = element_line(size = 0, 
                                          linetype = 'dotted',
                                          colour = NA),
          panel.grid.minor = element_line(size = 0, 
                                          linetype = 1,
                                          colour = NA))
  )
}



# Get individual species specific plots -----------------------------------

p1.clim <- 
  df.species.means.clim %>% 
  ggplot(aes(x = p_change_CAPER,
             y = p_change_risk,
             fill = regime,
             shape = climChange,
             color = regime)) + 
  ggtitle("a) Capercaillie\n") +
  pt_details_clim() +
  ylab(my_lab_risk)


p2.clim <- df.species.means.clim %>% 
  ggplot(aes(x = p_change_HAZ,
             y = p_change_risk,
             fill = regime,
             shape = climChange,
             color = regime)) +
  pt_details_clim() +
  ggtitle("b) Hazel grouse\n")

p3.clim <- df.species.means.clim %>% 
  ggplot(aes(x = p_change_THREE,
             y = p_change_risk,
             fill = regime,
             shape = climChange,
             color = regime)) +
  pt_details_clim() +
  ggtitle("c) Three toed\nwoodpecker")

p4.clim <- df.species.means.clim %>% 
  ggplot(aes(x = p_change_LESSER,
             y = p_change_risk,
             fill = regime,
             shape = climChange,
             color = regime)) +
  pt_details_clim() +
  ggtitle("d) Lesser spotted\nwoodpecker") +
  ylab(my_lab_risk)

p5.clim <- df.species.means.clim %>% 
  ggplot(aes(x = p_change_TIT,
             y = p_change_risk,
             fill = regime,
             shape = climChange,
             color = regime)) +
  pt_details_clim() +
  ggtitle("e) Long tailed tit\n")

p6.clim <- df.species.means.clim %>% 
  ggplot(aes(x = p_change_SQIRR,
             y = p_change_risk,
             fill = regime,
             shape = climChange,
             color = regime)) +
  pt_details_clim() +
  ggtitle("f) Siberian flying\nsquirrel")



species.plot.clim <- ggarrange(p1.clim, p2.clim, p3.clim,
                               p4.clim, p5.clim, p6.clim, 
                               widths = c(1.05,1, 1, 1.05,1,1),
                               ncol = 3,
                               nrow = 2,
                               common.legend = TRUE,
                               legend = 'right')

windows(7.5,5.3)  
# https://rpkgs.datanovia.com/ggpubr/reference/annotate_figure.html
annotate_figure(species.plot.clim,
                bottom = text_grob("Difference in HSI [%]", 
                                   color = "black",
                                   hjust = 0.5, 
                                   #x = -1, 
                                   y = 1.5, #4.5,
                                   face = "plain", size = 9)
)






# Temporal trends over years:   -----------------------------------------
# try for:
# a) wind damage risk, 
# c) combined HSI
# d) deadwood volume
#

# Get colors for line plots
my_cols_8 <- c(
  '#b10026', # dark red
  '#fd8d3c',
  
  '#ffeda0',  # bright red
  '#0868ac',  # blue
  '#f7fcb9',
  '#addd8e',
  '#41ab5d',
  '#005a32'  # dark green
)


# make a small function for the line plotting:
fun_line_pt <- function() {
  
  list( geom_line(size = 1, alpha = 0.5),
        geom_point(size= 0.8), #aes(fill = regime, 
                    #   shape = 21), size = 1),
        facet_grid(.~climChange),
        scale_color_manual(values =my_cols_8 ),
      #  scale_shape(),
        #viridis::scale_color_viridis(discrete = TRUE),
        theme_bw(), 
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
              legend.position = 'bottom')# +
  )
}







# wind risk
#windows(height = 3, width = 7)
p.risk <- 
  df.out %>% 
    #filter(regime ==  'BAU') %>% 
  group_by(year, regime, climChange) %>% 
  summarize(mean_windRisk = mean(windRisk, na.rm = T)) %>% 
  ggplot(aes(x = year,
             y = mean_windRisk*100,
             col = regime)) +
  fun_line_pt() +
  ylab("Wind damage risk [%]") #+


# Combined HSI
#windows(height = 3, width = 7)
p.HSI <- df.out %>% 
  group_by(year, regime, climChange) %>% 
  summarize(my_mean = mean(COMBINED_HSI, na.rm = T)) %>% 
  ggplot(aes(x = year,
             y = my_mean,
             col = regime)) +
  fun_line_pt() +
  ylab("Combined HSI ") 


# Deadwood
#windows(height = 3, width = 7)
p.DW <- df.out %>% 
  group_by(year, regime, climChange) %>% 
  summarize(my_mean = mean(V_total_deadwood, na.rm = T)) %>% 
  ggplot(aes(x = year,
             y = my_mean,
             col = regime)) +
  fun_line_pt() +
   ylab("Total deadwood [V m3/ha]") 

# V_pulp
#windows(height = 3, width = 7)
p.V_pulp <- 
  df.out %>% 
  group_by(year, regime, climChange) %>% 
  summarize(my_mean = mean(Harvested_V_pulp, na.rm = T)) %>% 
  ggplot(aes(x = year,
             y = my_mean,
             col = regime)) +
  fun_line_pt() +
  ylab("Harvested pulp [m3/ha]") #+

# V_log
#windows(height = 3, width = 7)
p.V_log <- 
  df.out %>% 
  group_by(year, regime, climChange) %>% 
  summarize(my_mean = mean(Harvested_V_log, na.rm = T)) %>% 
  ggplot(aes(x = year,
             y = my_mean,
             col = regime)) +
  fun_line_pt() +  
  ylab("Harvested log [m3/ha]") 






# print all at one page ----------------------------------
windows(width = 8, height = 15)
ggarrange(p.risk,  p.HSI, p.DW, p.V_log, p.V_pulp, 
          ncol = 1, nrow = 5, common.legend = T, legend = 'bottom')


# Adapt colors:





# Tremporal trends for individual HSI ---------------------------------------------
# try for:
# "CAPERCAILLIE"
# "HAZEL_GROUSE"
# "THREE_TOED_WOODPECKER"
# "LESSER_SPOTTED_WOODPECKER"
# "LONG_TAILED_TIT" 
# "SIBERIAN_FLYING_SQUIRREL"





p.caper <- 
  df.out %>% 
  group_by(year, regime, climChange) %>% 
  summarize(mean_y = mean(CAPERCAILLIE, na.rm = T)) %>% 
  ggplot(aes(x = year,
             y = mean_y,
             col = regime)) +
  ylab("Capercaillie") +
  fun_line_pt()




p.hazel <- df.out %>% 
  group_by(year, regime, climChange) %>% 
  summarize(my_mean = mean(HAZEL_GROUSE, na.rm = T)) %>% 
  ggplot(aes(x = year,
             y = my_mean,
             col = regime)) +
  ylab("Hazel grouse") +
  fun_line_pt()


p.three <- df.out %>% 
  group_by(year, regime, climChange) %>% 
  summarize(my_mean = mean(THREE_TOED_WOODPECKER, na.rm = T)) %>% 
  ggplot(aes(x = year,
             y = my_mean,
             col = regime)) +
  ylab("Three-toed wodpecker") +
  fun_line_pt()



p.lesser <- df.out %>% 
  group_by(year, regime, climChange) %>% 
  summarize(my_mean = mean(LESSER_SPOTTED_WOODPECKER, na.rm = T)) %>% 
  ggplot(aes(x = year,
             y = my_mean,
             col = regime)) +
  ylab("Lesser spotted woodpecker")  +
  fun_line_pt()

p.long <- df.out %>% 
  group_by(year, regime, climChange) %>% 
  summarize(my_mean = mean(LONG_TAILED_TIT, na.rm = T)) %>% 
  ggplot(aes(x = year,
             y = my_mean,
             col = regime)) +
  ylab("Long tailed tit") +  fun_line_pt()



p.squir <- df.out %>% 
  group_by(year, regime, climChange) %>% 
  summarize(my_mean = mean(SIBERIAN_FLYING_SQUIRREL, na.rm = T)) %>% 
  ggplot(aes(x = year,
             y = my_mean,
             col = regime)) +
  ylab("Siberian flying squirrel") +
  fun_line_pt()

# print all at one page ----------------------------------
windows(width = 8, height = 15)
ggarrange(p.caper,
          p.hazel,
          p.three,
          p.lesser,
          p.long,
          p.squir, 
          ncol = 1, nrow = 6, common.legend = T, legend = 'bottom')



















































# Dummy exmples: make a barplot with error bars

# Dummy example 

# How to get error bars? ---------------------------------
# make sure that the BAU is group-specific

dd1 <- data.frame(id = rep(c(1,2,3), 2),
                 vol = c(4,2,1,3,5,9),
                 reg = rep(c('control', 'new'), each = 3),
                 scen = rep('cc1', 6))

dd2 <- data.frame(id = rep(c(1,2,3), 2),
                  vol = c(13,14,10,15,15,11),
                  reg = rep(c('control', 'new'), each = 3),
                  scen = rep('cc2', 6))

# Merge dd1 and dd2
dd <- rbind(dd1, dd2)

dd %>% 
  group_by(scen, reg) %>% 
  summarize(means = mean(vol))

# 
# scen  reg     means
# <chr> <chr>   <dbl>
#   1 cc1   control  2.33
# 2 cc1   new      5.67
# 3 cc2   control 12.3 
# 4 cc2   new     13.7 



# Complete the mean values to the table??


dd %>% 
  group_by(scen) %>% 
 # mutate(vol_mean = mean(vol[reg == "new"])) #  %>% 
#mutate(control_vol = vol[reg == "control"]) #%>% 
  mutate(my_mean = mean(vol[reg == "control"])) %>% 
mutate(norm_vol = vol/mean(vol[reg == "control"]))# %>% 
ggplot(aes(reg, 
           y = norm_vol-1,
           fill = scen)) + 
  stat_summary(geom = "bar", 
               fun = mean) +
  stat_summary(geom = "errorbar", 
               fun.data = mean_cl_normal, 
               fun.args = list(mult = 1))# +
#scale_y_continuous(labels = scales::percent_format())








# calculate mean and sd
sum_dd <- dd %>% 
  group_by(reg, scen) %>% 
  summarize(V_mean = mean(vol, na.rm = T),
            V_sd = sd(vol, na.rm = T)) #
(sum_dd)


sum_dd %>%
  ggplot(aes(x = reg,
             y = V_mean)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(aes(x=reg,
                    min=V_mean-V_sd, 
                    ymax=V_mean+V_sd)) # + 



# Express the 'b' as percent change from 'a':
# first count the difference in %
sum_dd %>% 
  group_by(reg) %>% 
  # Calculate % change from a to b value
  mutate(control_mean   = 7.67,
         perc_change    = (10-7.67)/7.67 * 100) %>%
  filter(reg !='control') %>% 
  ggplot(aes(x = reg,
             y = perc_change)) +
  geom_bar(stat = 'identity') #+
# from which values calculate the error bar??
geom_errorbar(aes(x=reg,
                  min=V_mean-V_sd, 
                  ymax=V_mean+V_sd)) # +


# Calculate directly the difference between samples?




# Run example:
ggplot(mtcars, aes(cyl, qsec)) + 
  stat_summary(fun = mean, geom = "bar") + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", mult = 1)









