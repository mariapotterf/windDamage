

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
inFolder = "output/plotting"
inName   = 'df_filt.csv'

# Input table
df.out <- data.table::fread(paste(inPath, inFolder, inName,  sep = "/"),  # 
                                                                data.table=TRUE, 
                                                                stringsAsFactors = FALSE,
                                                                integer64="character")



# Order factors levels ----------------------------------------------------

# Climate change
df.out$climChange <-factor(df.out$climChange, 
                           levels = c("REF", "RCP45", "RCP85"))

# Regimes
df.out <- df.out %>% 
  mutate(regime = factor(regime, 
                         levels = c("short_30", "short_10","BAU", "noThin", "ext_10", "ext_30", "GTR", "CCF")))


# time effect
df.out$timeEffect <-factor(df.out$timeEffect, 
                           levels = c("short-term", 'long-term'))




# Create plots  ----------------------------------------------------------------------------------------

# Define labels:
lab_manag = c("Regime adaptation")


# Plots for averaged deadwood and volume over short vs long-term

p.DW <- df.out %>% 
  group_by(regime, climChange, timeEffect) %>% 
  summarize(DW_mean = mean(V_total_deadwood, na.rm = T)) %>% 
  ggplot(aes(x = regime,
             y = DW_mean,
             color = climChange,
             group = climChange)) +
  geom_line() + 
  geom_point() +
  ylim(0,40)+
  ylab("Deadwood volume \n [m3/ha]") +
  xlab(lab_manag) + 
  facet_grid(.~timeEffect, scales = 'free') + # scales="free"
  viridis::scale_color_viridis(discrete = TRUE) +
theme( axis.title  = element_text(size = 9, face="plain", family = "sans"),
        axis.text   = element_text(size=8),
       axis.text.x = element_text(angle = 90, 
                                 vjust = 0.5, 
                                 hjust=1))

# Total stand volume
p.V <- df.out %>% 
  #sample_n(500000) %>% 
  group_by(regime, climChange, timeEffect) %>% 
  summarize(V_mean = mean(V, na.rm = T)) %>% 
  ggplot(aes(x = regime,
             y = V_mean,
             color = climChange,
             group = climChange)) +
  geom_line() + 
  geom_point() +
  ylab("Volume \n[m3/ha]") +
  xlab(lab_manag) + 
  facet_grid(.~timeEffect, scales = 'free') +
  viridis::scale_color_viridis(discrete = TRUE) +
  theme( axis.title  = element_text(size = 9, face="plain", family = "sans"),
         axis.text   = element_text(size = 8),
         axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, 
                                   hjust=1))



windows(width = 7,height = 2.4)
ggarrange(p.V, p.DW, 
          ncol = 2, 
          nrow = 1, 
          labels = "auto",
          common.legend = T, 
          legend = 'bottom' )




# Explore the data basic statistics --------------------------------------------------------


# Get histograms
windows()
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




# Make several histograms at once: reshape the data from wide to long:
# whould I use median or median??? 
# highly skewed, I should use median!!
df.ind <- 
  df.out %>% 
  dplyr::select(#'year',
    "CAPERCAILLIE",
    "HAZEL_GROUSE",
    "THREE_TOED_WOODPECKER",
    "LESSER_SPOTTED_WOODPECKER",
    "LONG_TAILED_TIT",
    "SIBERIAN_FLYING_SQUIRREL",
    "COMBINED_HSI")  %>% # ,
  # "V_total_deadwood" removed as has a different scale than 0-1 HSI
  pivot_longer(everything(vars = NULL),
               names_to = "indicator", values_to = "HSI") #%>%



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



# Differences from timber volume
# ----------------------------------------------------------------------------------
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















# ------------------------------------------------------------------------------
# Put barplot data together to have the same y labels
# ------------------------------------------------------------------------------


# Put together wind risk and pulp and log  -------------------------------------

# Get the table, merge with log and pulp estimates

# Table wind risk
df.risk <- df.out %>% 
  group_by(climChange, regime) %>% # modif, #geo_grad,
  summarise(windRisk_mean = mean(windRisk, na.rm = T)) %>% 
  mutate(BAU_risk         = windRisk_mean[match('BAU', regime)],
         perc_change_risk = windRisk_mean/BAU_risk * 100 - 100) %>%
  dplyr::select(c(climChange, 
                  regime, 
                  perc_change_risk)) %>% 
  filter(regime != "BAU")   # remove BAU from teh table

# Table log and pulp
df.log.pulp <- df.out %>% 
  group_by(id, climChange, regime) %>% # modif, #geo_grad,
  summarise(sum_V_log     = sum(Harvested_V_log, na.rm = T),
            sum_V_pulp    = sum(Harvested_V_pulp, na.rm = T))  %>%
  ungroup() %>% 
  group_by(climChange, regime) %>% 
  summarise(sum_V_log     = mean(sum_V_log, na.rm = T),
            sum_V_pulp    = mean(sum_V_pulp, na.rm = T))  %>%
  mutate(BAU_log          = sum_V_log[match('BAU', regime)],
         BAU_pulp         = sum_V_pulp[ match('BAU', regime)],
         perc_change_log  = sum_V_log /BAU_log  * 100 - 100,
         perc_change_pulp = sum_V_pulp/BAU_pulp * 100 - 100)  %>%
  filter(regime != "BAU")  %>%    # remove BAU from teh table
  dplyr::select(c(climChange, regime, 
                  perc_change_log, 
                  perc_change_pulp)) #%>%
  

# Join  tables for wind risk and timber quality, and pivot values --------------------------------------------
windows(height = 3.5, width=7)

df.risk %>% 
  left_join(df.log.pulp) %>% 
  pivot_longer(!c(regime, climChange), #everything(vars = NULL),
               names_to = "Indicator", 
               values_to = "perc_ch")  %>%
 # print(n = 80)
  mutate(Indicator = factor(Indicator, 
                            levels = c('perc_change_risk', 'perc_change_log', 'perc_change_pulp' ),
                            labels = c('Wind damage risk', 'Log timber',      'Pulp timber'))) %>% 
  ggplot(aes(y=perc_ch, 
               x=regime,
               fill = climChange)) + 
  geom_bar(position="dodge", 
           stat="identity") +
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
        legend.position = c(.2, .3), # legend position within the plot, x, y
        legend.title = element_text(size=10),
        legend.text  = element_text(size=8),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.box.background = element_rect(colour = "black")) 






# Plot for Combined HSI and Deadwood volume

windows(height = 3.2, width=6)
df.out %>% 
  group_by(climChange, regime) %>% # modif, #geo_grad,
  summarise(#windRisk_mean = mean(windRisk, na.rm = T),
            HSI_mean = mean(COMBINED_HSI, na.rm = T),
            DW_mean = mean(V_total_deadwood, na.rm = T),) %>% 
  mutate(BAU_HSI          = HSI_mean[match('BAU', regime)],
         perc_change_HSI  = HSI_mean/BAU_HSI * 100 - 100,
         BAU_DW           = DW_mean[match('BAU', regime)],
         perc_change_DW   = DW_mean/BAU_DW * 100 - 100) %>%
  dplyr::select(c(climChange, 
                  regime, 
                  perc_change_HSI,
                  perc_change_DW)) %>%
  pivot_longer(!c(regime, climChange), #everything(vars = NULL),
               names_to = "Indicator", 
               values_to = "perc_ch")  %>%
  mutate(Indicator = factor(Indicator, 
                            levels = c('perc_change_HSI', 'perc_change_DW' ),
                            labels = c('Combined HSI',    'Deadwood volume'))) %>% 
  filter(regime != "BAU")  %>%    # remove BAU from teh table
  ggplot(aes(y=perc_ch, 
             x=regime,
             fill = climChange)) + 
  geom_bar(position="dodge", 
           stat="identity") +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"), 
                    name="Climate change") +
  ylab("Differences from BAU scenario [%]") +
  xlab(lab_manag) +
  facet_grid(.~Indicator, scales="free") +
  coord_flip() +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        #legend.position = 'bottom',
        legend.position = c(.90, .22), # legend position within the plot, x, y
        legend.title = element_text(size=10),
        legend.text  = element_text(size=8),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.box.background = element_rect(colour = "black")) 



# ------------------------------------
# Economic consequences:
# ------------------------------------


# Evaluate sum of harvested timber: -----------------------------------------
# calculate the sum by id and then get a mean by regime for ids
# as now I have only sum of the subset, not whole Finland!
# to have a value representative for site!
windows(7, 3.5)
df.out %>% 
  group_by(id, climChange, regime) %>% # modif, #geo_grad,
  summarise(sum_V_log     = sum(Harvested_V_log, na.rm = T),
            sum_V_pulp    = sum(Harvested_V_pulp, na.rm = T))  %>%
  ungroup() %>% 
  group_by(climChange, regime) %>% 
  summarise(sum_V_log     = mean(sum_V_log, na.rm = T),
            sum_V_pulp    = mean(sum_V_pulp, na.rm = T))  %>%
  mutate(BAU_log          = sum_V_log[match('BAU', regime)],
         BAU_pulp         = sum_V_pulp[ match('BAU', regime)],
         perc_change_log  = sum_V_log /BAU_log  * 100 - 100,
         perc_change_pulp = sum_V_pulp/BAU_pulp * 100 - 100)  %>%
  filter(regime != "BAU")  %>%    # remove BAU from teh table
  dplyr::select(c(climChange, regime, 
                  perc_change_log, 
                  perc_change_pulp)) %>%
  pivot_longer(!c(regime, climChange), #everything(vars = NULL),
               names_to = "Timber_quality", 
               values_to = "perc_V")  %>%
  
  # Change the coding of characters to have nice facet names
  mutate(Timber_quality = recode(Timber_quality,
                                 "perc_change_log" = "Log", 
                                 "perc_change_pulp" = "Pulp")) %>% 
  ggplot(aes(y=perc_V, 
             x=regime,
             fill = climChange)) + 
  geom_bar(position="dodge", 
           stat="identity") +
  geom_hline(yintercept = 0) +
  coord_flip() +
  facet_grid(.~ Timber_quality) +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"), 
                    name="Climate change") +
  ylab("Difference in harvested\ntimber volume [%]") +
  xlab(lab_manag) +
  theme_bw()  + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        #legend.position = 'bottom',
        legend.position = c(.86, .70), # legend position within the plot, x, y
        legend.title = element_text(size=10),
        legend.text  = element_text(size=8),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.box.background = element_rect(colour = "black")) 





# Temporal trends over years:   -----------------------------------------
# try for:
# a) wind damage risk, 
# b) age, 
# c) combined HSI
# d) deadwood volume
# e) H_dom
library(viridis)


# wind risk
#windows(height = 3, width = 7)
p.risk <- df.out %>% 
  group_by(year, regime, climChange) %>% 
  summarize(mean_windRisk = mean(windRisk, na.rm = T)) %>% 
  ggplot(aes(x = year,
             y = mean_windRisk*100,
             col = regime)) +
  geom_line(size = 1.2) +
  ylim(0,10) +
  ylab("Wind damage risk [%]") +
  facet_grid(.~climChange) +
  viridis::scale_color_viridis(discrete = TRUE) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = 'bottom')# +



# Age over landscape
#windows(height = 3, width = 7)
p.age <- df.out %>% 
  group_by(year, regime, climChange) %>% 
  summarize(my_mean = mean(Age, na.rm = T)) %>% 
  ggplot(aes(x = year,
             y = my_mean,
             col = regime)) +
  geom_line(size = 1.2) +
  ylim(20,130) +
  facet_grid(.~climChange) +
  viridis::scale_color_viridis(discrete = TRUE) +
  theme_bw() +
  ylab("Age [years]") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = 'bottom')# +



# Combined HSI
#windows(height = 3, width = 7)
p.HSI <- df.out %>% 
  group_by(year, regime, climChange) %>% 
  summarize(my_mean = mean(COMBINED_HSI, na.rm = T)) %>% 
  ggplot(aes(x = year,
             y = my_mean,
             col = regime)) +
  geom_line(size = 1.2) +
  ylim(0,1) +
  facet_grid(.~climChange) +
  viridis::scale_color_viridis(discrete = TRUE) +
  theme_bw() +
  ylab("COMBINED_HSI ") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = 'bottom')# +



# Deadwood
#windows(height = 3, width = 7)
p.DW <- df.out %>% 
  group_by(year, regime, climChange) %>% 
  summarize(my_mean = mean(V_total_deadwood, na.rm = T)) %>% 
  ggplot(aes(x = year,
             y = my_mean,
             col = regime)) +
  geom_line(size = 1.2) +
  # ylim(0,1) +
  facet_grid(.~climChange) +
  viridis::scale_color_viridis(discrete = TRUE) +
  theme_bw() +
  ylab("Total deadwood [V m3/ha]") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = 'bottom')# +


# H_dom
#windows(height = 3, width = 7)
p.H_dom <- df.out %>% 
  group_by(year, regime, climChange) %>% 
  summarize(my_mean = mean(H_dom, na.rm = T)) %>% 
  ggplot(aes(x = year,
             y = my_mean,
             col = regime)) +
  geom_line(size = 1.2) +
  # ylim(0,1) +
  facet_grid(.~climChange) +
  viridis::scale_color_viridis(discrete = TRUE) +
  theme_bw() +
  ylab("Dominant tree height [m]") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = 'bottom')# +



# print all at one page ----------------------------------
windows(width = 8, height = 15)
ggarrange(p.risk, p.age, p.HSI, p.DW, p.H_dom, ncol = 1, nrow = 5, common.legend = T)







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
  group_by(climChange, regime) %>% 
  summarise(windRisk_mean = round(mean(windRisk, na.rm = T)*100, digits = 1),
            windRisk_sd   = round(sd(windRisk, na.rm = T)*100, digits = 1),
            HSI_mean      = round(mean(COMBINED_HSI, na.rm = T), digits = 1),
            HSI_sd        = round(sd(COMBINED_HSI, na.rm = T), digits = 1),
            DW_mean       = round(mean(V_total_deadwood, na.rm = T), digits = 1),
            DW_sd         = round(sd(V_total_deadwood, na.rm = T), digits = 1)) %>% 
  left_join(df.timber)


# Third, format output table
formated_df_main <- 
  df_summary_main %>% 
  mutate(WindDamage    = stringr::str_glue("{windRisk_mean}±{windRisk_sd}"),
         Combined_HSI  = stringr::str_glue("{HSI_mean}±{HSI_sd}"),
         Deadwood      = stringr::str_glue("{DW_mean}±{DW_sd}"),
         Harvested_log = stringr::str_glue("{mean_sum_V_log}±{sd_sum_V_log}"),
         Harvested_pulp= stringr::str_glue("{mean_sum_V_pulp}±{sd_sum_V_pulp}"),
  ) %>%  #,  {scales::percent(sd_height)}
  dplyr::select(regime, climChange, WindDamage, Deadwood,
                Combined_HSI, Harvested_log, Harvested_pulp)



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









# 
# wind damage risk  vs individual species no climate change !!-------------------------

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
         control_CAPER   = mean_CAPER[match('BAU', regime)],
         p_change_CAPER  = mean_CAPER /control_CAPER * 100 - 100,
         p_change_risk   = mean_risk/control_risk * 100 - 100,
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


pt_details <- function() {
  list(
    ylab(''),
    xlab(''),
    geom_vline(xintercept = 0, color = "grey70", lty = "dashed"), 
    geom_hline(yintercept = 0, color = "grey70", lty = "dashed"),
    geom_point(size = 3, shape = 21, color = 'grey'), # + 'black', shape 21
    scale_fill_manual(values  = my_cols_RdGn),
    scale_color_manual(values = my_cols_RdGn),
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

p1 <- 
  df.species.means %>% 
  ggplot(aes(x = p_change_CAPER,
             y = p_change_risk,
             fill = regime)) + 
  ggtitle("a) capercaillie\n") +
  pt_details() +
  ylab(my_lab_risk)


p2 <- df.species.means %>% 
  ggplot(aes(x = p_change_HAZ,
             y = p_change_risk,
             fill = regime)) +
  pt_details() +
  ggtitle("b) hazel grouse\n")

p3 <- df.species.means %>% 
  ggplot(aes(x = p_change_THREE,
             y = p_change_risk,
             fill = regime)) +
  pt_details() +
  ggtitle("c) three toed\nwoodpecker")

p4 <- df.species.means %>% 
  ggplot(aes(x = p_change_LESSER,
             y = p_change_risk,
             fill = regime)) +
  pt_details() +
  ggtitle("d) lesser spotted\nwoodpecker") +
  ylab(my_lab_risk)

p5 <- df.species.means %>% 
  ggplot(aes(x = p_change_TIT,
             y = p_change_risk,
             fill = regime)) +
  pt_details() +
  ggtitle("e) long tailed tit\n")

p6 <- df.species.means %>% 
  ggplot(aes(x = p_change_SQIRR,
             y = p_change_risk,
             fill = regime)) +
  pt_details() +
  ggtitle("f) siberian flying\nsquirrel")



species.plot <- ggarrange(p1,p2,p3,p4,p5,p6, 
                          widths = c(1.05,1, 1, 1.05,1,1),
                          ncol = 3,nrow = 2,
                          common.legend = TRUE,
                          legend = 'bottom')

windows(7,5)  
# https://rpkgs.datanovia.com/ggpubr/reference/annotate_figure.html
annotate_figure(species.plot,
                bottom = text_grob("Difference in HSI [%]", 
                                   color = "black",
                                   hjust = 0.5, 
                                   #x = -1, 
                                   y = 4.5,
                                   face = "plain", size = 9)
)



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









# Evaluate changes in mean age over landscape ---




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
my_id = '101000407'

windows()
df.out %>% 
  filter(id == my_id & climChange == "RCP85") %>% 
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





# Put differences in HSI and wind damage risk together in points plots --------------

# ========================================================
#                    WIND RISK x COMBINED HSI
# ========================================================



# Calculate % change between BAU_normal and other adaptations? ---------



# Make functions for plotting: LOLLIPOP
my_lollipop <- function() {
  list(
    geom_hline(yintercept = 0),# +
    #geom_segment( aes(x= climChange, 
    #                 xend= climChange, 
    #                y=0, 
    #            yend= yval*100,#median_risk*100,
    #               col = climChange)), # +
    geom_point(aes(col = climChange), 
               size = 5), # +
    scale_color_manual(values = cols_ylRd3),#  +
    facet_grid(geo_grad~adapt), # +
    theme(legend.position="bottom")
  )
}







# subset only regimes of interest and check if values will change?
df.out2 <- df.out %>% 
  filter(regime %in% my_regimes)

my_cols = c('climChange', 'adapt', 'mainType', 
            'median_risk', 'control_risk', 'perc_change_risk', 
            'median_HSI',  'control_HSI',  'perc_change_HSI',
            'adaptPaste')

# Get a compare toward the BAU normal
df.plot <- 
  df.out2 %>% 
  #  filter(mainType != "SA" ) %>% # & climChange == "no"
  group_by(geo_grad, climChange, adapt, mainType) %>% # modif, #geo_grad,
  summarise(median_risk = median(windRisk, na.rm = T),
            median_HSI  = median(COMBINED_HSI, na.rm = T)) %>%
  mutate(adaptPaste = paste(adapt, mainType, sep = "_")) %>%
  group_by(geo_grad, climChange) %>% 
  mutate(control_risk = median_risk[match('normal_BAUwT', adaptPaste)],
         control_HSI  = median_HSI[ match('normal_BAUwT', adaptPaste)],
         perc_change_HSI  = median_HSI /control_HSI  * 100 - 100,
         perc_change_risk = median_risk/control_risk * 100 - 100) %>% 
  dplyr::select(all_of(my_cols)) %>% 
  filter(adapt != "normal") %>% 
  mutate(adapt = factor(adapt, 
                        levels = c("extended", "shorten", 'noThin', "GTR" ,"CCF" )))  



# Lollipop risk by adaptation: medians, absolute ---------------------------------------
windows(7,5.5)
df.plot %>% 
  ggplot(aes(x = climChange,
             y = median_risk*100)) +
  my_lollipop()  +
  geom_segment(aes(x= climChange, 
                   xend= climChange, 
                   y=0, 
                   yend= median_risk*100,#median_risk*100,
                   col = climChange)) +
  # ylim() +
  ylab("median wind damage risk")




windows(7,5.5)
df.plot %>% 
  filter(adapt != "normal") %>% 
  mutate(adapt = factor(adapt, 
                        levels = c("extended", "shorten", 'noThin', "GTR" ,"CCF" ))) %>% 
  ggplot(aes(x = climChange,
             y = median_HSI)) +
  geom_hline(yintercept = c(0, 0.5), 
             col = 'grey80',
             lty = "dashed") +
  geom_segment( aes(x= climChange, 
                    xend= climChange, 
                    y=0, 
                    yend= median_HSI,
                    col = climChange)) +
  geom_point(aes(col = climChange), size = 5) +
  scale_color_manual(values = cols_ylRd3) +
  ylim(0, 1) +
  ylab("median combined HSI") +
  facet_grid(geo_grad~adapt) +
  theme(legend.position="bottom") 



# make point plots by geo gradient  --------------------------------------------- 
# https://stackoverflow.com/questions/57153428/r-plot-color-combinations-that-are-colorblind-accessible
colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                       "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


windows(width = 7, length = 3.5)
df.plot %>%  
  filter(adapt != "normal" ) %>% 
  mutate(adapt = factor(adapt, 
                        levels =  c("extended", "shorten", 'noThin', "GTR" ,"CCF" ))) %>% 
  ggplot(aes(x = perc_change_HSI,
             y = perc_change_risk,
             color = adapt,
             shape = climChange)) + 
  geom_vline(xintercept = c(0,100), col = 'grey', lty = "dashed") +
  geom_hline(yintercept = c(0,100), col = 'grey', lty = "dashed") +
  geom_point(size = 4, alpha = 0.8) +  
  ylim(-50,170) +
  xlim(-50,170) + 
  facet_grid(.~geo_grad) +
  # scale_color_manual(values = colorBlindBlack8) +
  ylab("Change in wind damage risk (%)") +
  xlab("Change in combined HSI (%)") +
  guides(colour = guide_legend(override.aes = list(size=3))) # change the point size in teh legend



# ========================================================
# Are there differences for individual species??? ======================================
# ========================================================

# CAPERCAILLIE  
# HAZEL_GROUSE
# LESSER_SPOTTED_WOODPECKER
# SIBERIAN_FLYING_SQUIRREL
# LONG_TAILED_TIT
# THREE_TOED_WOODPECKER


# automate script plotting:
col_keep = c('climChange', 'adapt', 'mainType', 
             'median_risk', 'control_risk', 'perc_change_risk', 
             'median_HSI',  'control_HSI',  'perc_change_HSI',
             'adaptPaste')

# CAPERCAILIE   =============================
windows(width = 7, height = 3)

df.CAP <- 
  df.out %>% 
  filter(mainType != "SA" ) %>% # & climChange == "no"
  group_by(geo_grad, climChange, adapt, mainType) %>% # modif, #geo_grad,
  summarise(median_risk = median(windRisk, na.rm = T),
            median_HSI  = median(CAPERCAILLIE, na.rm = T)) %>%
  mutate(adaptPaste = paste(adapt, mainType, sep = "_")) %>%
  group_by(geo_grad, climChange) %>% 
  mutate(control_risk = median_risk[match('normal_BAUwT', adaptPaste)],
         control_HSI  = median_HSI[ match('normal_BAUwT', adaptPaste)],
         perc_change_HSI  = median_HSI /control_HSI * 100 - 100,
         perc_change_risk = median_risk/control_risk * 100 - 100) %>% 
  #  print(n = 30) %>% 
  dplyr::select(col_keep) 


# Lollipop risk by adaptation: medians, absolute ---------------------------------------

windows(7,5.5)
df.CAP %>% 
  filter(adapt != "normal") %>% 
  mutate(adapt = factor(adapt, 
                        levels = c("extended", "shorten", 'noThin', "GTR" ,"CCF" ))) %>% 
  ggplot(aes(x = climChange,
             y = median_HSI)) +
  geom_hline(yintercept = c(0, 0.5), 
             col = 'grey80',
             lty = "dashed") +
  geom_segment( aes(x= climChange, 
                    xend= climChange, 
                    y=0, 
                    yend= median_HSI,
                    col = climChange)) +
  geom_point(aes(col = climChange), size = 5) +
  scale_color_manual(values = cols_ylRd3) +
  ylim(0, 1) +
  ylab("median CAPERCAILLIE") +
  facet_grid(geo_grad~adapt) +
  theme(legend.position="bottom") 


# =======================================
#    Capercailie scatter plot
# =======================================

windows(width = 7, length = 3.5)
df.CAP %>%  
  filter(adapt != "normal" ) %>% 
  mutate(adapt = factor(adapt, 
                        levels =  c("extended", "shorten", 'noThin', "GTR" ,"CCF" ))) %>% 
  ggplot(aes(x = perc_change_HSI,
             y = perc_change_risk,
             color = adapt,
             shape = climChange)) + 
  geom_vline(xintercept = c(0,100), col = 'grey', lty = "dashed") +
  geom_hline(yintercept = c(0,100), col = 'grey', lty = "dashed") +
  geom_point(size = 4, alpha = 0.8) +  
  #ylim(-50,170) +
  #xlim(-50,170) + 
  facet_grid(.~geo_grad) +
  # scale_color_manual(values = colorBlindBlack8) +
  ylab("Change in wind damage risk (%)") +
  xlab("Change in CAPERCAILLIE (%)") +
  guides(colour = guide_legend(override.aes = list(size=3))) # change the point size in teh legend


# ==========================================
#    HASEL GROUSE 
# ==========================================

df.HASEL <- 
  df.out %>% 
  filter(mainType != "SA" ) %>% # & climChange == "no"
  group_by(geo_grad, climChange, adapt, mainType) %>% # modif, #geo_grad,
  summarise(median_risk = median(windRisk, na.rm = T),
            median_HSI  = median(HAZEL_GROUSE, na.rm = T)) %>%
  mutate(adaptPaste = paste(adapt, mainType, sep = "_")) %>%
  group_by(geo_grad, climChange) %>% 
  mutate(control_risk = median_risk[match('normal_BAUwT', adaptPaste)],
         control_HSI  = median_HSI[ match('normal_BAUwT', adaptPaste)],
         perc_change_HSI  = median_HSI /control_HSI * 100 - 100,
         perc_change_risk = median_risk/control_risk * 100 - 100) %>% 
  #  print(n = 30) %>% 
  dplyr::select('climChange', 'adapt', 'mainType', 
                'median_risk', 'control_risk', 'perc_change_risk', 
                'median_HSI',  'control_HSI',  'perc_change_HSI',
                'adaptPaste') 


# Lollipop HAZEL_GROUSE by adaptation: medians, absolute ---------------------------------------
windows(7,5.5)
df.HASEL %>% 
  filter(adapt != "normal") %>% 
  mutate(adapt = factor(adapt, 
                        levels = c("extended", "shorten", 'noThin', "GTR" ,"CCF" ))) %>% 
  ggplot(aes(x = climChange,
             y = median_HSI)) +
  geom_hline(yintercept = c(0, 0.5), 
             col = 'grey80',
             lty = "dashed") +
  geom_segment( aes(x= climChange, 
                    xend= climChange, 
                    y=0, 
                    yend= median_HSI,
                    col = climChange)) +
  geom_point(aes(col = climChange), size = 5) +
  scale_color_manual(values = cols_ylRd3) +
  ylim(0, 1) +
  ylab("median HAZEL_GROUSE") +
  facet_grid(geo_grad~adapt) +
  theme(legend.position="bottom") 


# =======================================
#    HAZEL_GROUSE scatter plot
# =======================================

windows(width = 7, length = 3.5)
df.HASEL %>%  
  filter(adapt != "normal" ) %>% 
  mutate(adapt = factor(adapt, 
                        levels =  c("extended", "shorten", 'noThin', "GTR" ,"CCF" ))) %>% 
  ggplot(aes(x = perc_change_HSI,
             y = perc_change_risk,
             color = adapt,
             shape = climChange)) + 
  geom_vline(xintercept = c(0,100), col = 'grey', lty = "dashed") +
  geom_hline(yintercept = c(0,100), col = 'grey', lty = "dashed") +
  geom_point(size = 4, alpha = 0.8) +  
  facet_grid(.~geo_grad) +
  ylab("Change in wind damage risk (%)") +
  xlab("Change in HAZEL_GROUSE (%)") +
  guides(colour = guide_legend(override.aes = list(size=3))) # change the point size in teh legend



###!! 
# ==========================================
#    LESSER_SPOTTED_WOODPECKER 
# ==========================================

df.woodPeck <- 
  df.out %>% 
  filter(mainType != "SA" ) %>% # & climChange == "no"
  group_by(geo_grad, climChange, adapt, mainType) %>% # modif, #geo_grad,
  summarise(median_risk = median(windRisk, na.rm = T),
            median_HSI  = median(LESSER_SPOTTED_WOODPECKER, na.rm = T)) %>%
  mutate(adaptPaste = paste(adapt, mainType, sep = "_")) %>%
  group_by(geo_grad, climChange) %>% 
  mutate(control_risk = median_risk[match('normal_BAUwT', adaptPaste)],
         control_HSI  = median_HSI[ match('normal_BAUwT', adaptPaste)],
         perc_change_HSI  = median_HSI /control_HSI * 100 - 100,
         perc_change_risk = median_risk/control_risk * 100 - 100) %>% 
  #  print(n = 30) %>% 
  dplyr::select('climChange', 'adapt', 'mainType', 
                'median_risk', 'control_risk', 'perc_change_risk', 
                'median_HSI',  'control_HSI',  'perc_change_HSI',
                'adaptPaste') 


# Lollipop LESSER_SPOTTED_WOODPECKER by adaptation: medians, absolute ---------------------------------------
windows(7,5.5)
df.HASEL %>% 
  filter(adapt != "normal") %>% 
  mutate(adapt = factor(adapt, 
                        levels = c("extended", "shorten", 'noThin', "GTR" ,"CCF" ))) %>% 
  ggplot(aes(x = climChange,
             y = median_HSI)) +
  geom_hline(yintercept = c(0, 0.5), 
             col = 'grey80',
             lty = "dashed") +
  geom_segment( aes(x= climChange, 
                    xend= climChange, 
                    y=0, 
                    yend= median_HSI,
                    col = climChange)) +
  geom_point(aes(col = climChange), size = 5) +
  scale_color_manual(values = cols_ylRd3) +
  ylim(0, 1) +
  ylab("median LESSER_SPOTTED_WOODPECKER") +
  facet_grid(geo_grad~adapt) +
  theme(legend.position="bottom") 


# =======================================
#    LESSER_SPOTTED_WOODPECKER scatter plot
# =======================================

windows(width = 7, length = 3.5)
df.woodPeck %>%  
  filter(adapt != "normal" ) %>% 
  mutate(adapt = factor(adapt, 
                        levels =  c("extended", "shorten", 'noThin', "GTR" ,"CCF" ))) %>% 
  ggplot(aes(x = perc_change_HSI,
             y = perc_change_risk,
             color = adapt,
             shape = climChange)) + 
  geom_vline(xintercept = c(0,100), col = 'grey', lty = "dashed") +
  geom_hline(yintercept = c(0,100), col = 'grey', lty = "dashed") +
  geom_point(size = 4, alpha = 0.8) +  
  facet_grid(.~geo_grad) +
  ylab("Change in wind damage risk (%)") +
  xlab("Change in LESSER_SPOTTED_WOODPECKER (%)") +
  guides(colour = guide_legend(override.aes = list(size=3))) # change the point size in teh legend
































# how does the shortening affect tree age over landscape? --------------------------------------
# 
windows(7,2.5)
df.out %>% 
  filter(year == 2111 & geo_grad == "center") %>% #& geo_grad == "center" 
  filter(mainType != "SA") %>% 
  filter(windRisk < quantile(windRisk, 0.95, na.rm = T))  %>%
  group_by(geo_grad, climChange, mainType, modif) %>% # modif,
  summarise(my_y = median(Age, na.rm = T)) %>%
  ggplot(aes(x= mainType,
             y = my_y,
             color = modif,
             group = climChange)) +
  geom_point(aes()) #+
ylab("median age") +
  facet_grid(geo_grad~mainType)




# Make median +- quantil plot for HSI: -------------------------------------------------
windows(7,2.5)
df.out %>% 
  filter(mainType != "CCF" & mainType != "SA") %>% 
  filter(windRisk < quantile(windRisk, 0.95, na.rm = T))  %>%  
  ggplot() + 
  stat_summary(mapping = aes(x = modif, #change_time, 
                             y = COMBINED_HSI,
                             group = climChange,
                             col = climChange),
               fun.min = function(z) { quantile(z,0.25) },
               fun.max = function(z) { quantile(z,0.75) },
               fun = median,
               position=position_dodge(width=0.4)) +
  facet_grid(.~geo_grad) +
  theme_classic()+
  theme(legend.position = "bottom")





