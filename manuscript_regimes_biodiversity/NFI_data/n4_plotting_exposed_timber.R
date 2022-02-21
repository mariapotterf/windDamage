

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
  ggplot (aes(x=regime, 
              y=DW_mean,
              fill=climChange)) + 
  geom_bar (stat="identity", position = position_dodge()) + # width = 1
  #geom_bar(stat='identity') + 
 # geom_point() +
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
p.V <- 
  
  df.out %>% 
  #sample_n(500000) %>% 
  group_by(regime, climChange, timeEffect) %>% 
  summarize(V_mean = mean(V, na.rm = T)) %>% 
    ggplot (aes(x=regime, 
                y=V_mean,
                fill=climChange)) + 
    geom_bar (stat="identity", position = position_dodge()) + # width = 1
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





# Ryan: 
# Not necessary, but I’d be curious to see what % of stands in the starting conditions 
# scored 1’s for how many of these species, and how many stands each pair overlapped in. 
# Might be an interesting little exercise for supplemental to let us see the relative prevalence of these species (or maybe the Monkonnen paper does something like this already)

my_species = c("CAPERCAILLIE",
               "HAZEL_GROUSE",
               "THREE_TOED_WOODPECKER",
               "LESSER_SPOTTED_WOODPECKER", 
               "LONG_TAILED_TIT",
               "SIBERIAN_FLYING_SQUIRREL" )

# get proporton of stands scoring >0.8 in short and lon term

# # how many rows are in one year?
n_rows = df.out %>% 
  #dplyr::filter(regime == 'short_30') %>%
  dplyr::filter(year == '2016' ) %>% 
  nrow()


my_cols_RdGn2 <- c(
  '#b10026', # dark red
  '#fd8d3c',
  '#0868ac',  # blue
  #  '#ffeda0',  # bright red
  'yellow',
  '#f7fcb9',
  '#addd8e',
  '#41ab5d',
  '#005a32' # dark green
)

# make plot: 
df.out %>% 
   #dplyr::filter(regime == 'short_30') %>%
    dplyr::filter(year == '2016' | year == '2111' ) %>% 
  dplyr::select(#'year',
    "CAPERCAILLIE",
    "HAZEL_GROUSE",
    "THREE_TOED_WOODPECKER",
    "LESSER_SPOTTED_WOODPECKER",
    "LONG_TAILED_TIT",
    "SIBERIAN_FLYING_SQUIRREL",
    "year",
    'regime',
    'climChange')  %>% # ,
  # "V_total_deadwood" removed as has a different scale than 0-1 HSI
  pivot_longer(cols = CAPERCAILLIE:SIBERIAN_FLYING_SQUIRREL,
               names_to = "indicator", values_to = "HSI") %>%
    filter(HSI > 0.5 ) %>% 
    group_by(year, regime, climChange, indicator) %>%
    tally() %>% 
    mutate(freq = n / n_rows) %>% 
    ggplot(aes(y = freq,
               x = indicator,
               color = regime,
               fill = regime,
               shape = indicator)) +
  geom_jitter(size = 2.5, shape = 21, color = 'grey', 
              #alpha = 0.9, 
              width = 0.2, height = 0.0) + # + 'black', shape 21
  scale_fill_manual(values  = my_cols_RdGn2) + 
  scale_color_manual(values = my_cols_RdGn2) +
  facet_grid(year~climChange) +
    theme( axis.title  = element_text(size = 9, face="plain", family = "sans"),
           axis.text   = element_text(size = 8),
           axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  
  #  mutate(freq = n / sum(n))


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
# medium age = 



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




# DO NOT RUN Make several histograms at once: reshape the data from wide to long: -------
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



# Put barplot data together to have the same y labels ------

windows(height = 3.5, width=7)
df.out %>% 
  group_by(climChange, regime) %>% # modif, #geo_grad,
  summarise(V_mean = mean(V, na.rm = T),
            HSI_mean = mean(COMBINED_HSI, na.rm = T),
            HSI_mean = mean(COMBINED_HSI, na.rm = T),
            DW_mean = mean(V_total_deadwood, na.rm = T),) %>% 
  mutate(BAU_HSI          = HSI_mean[match('BAU', regime)],
         perc_change_HSI  = HSI_mean/BAU_HSI * 100 - 100,
         BAU_risk         = V_mean[match('BAU', regime)],
         perc_change_risk = V_mean/BAU_risk * 100 - 100,
         BAU_DW           = DW_mean[match('BAU', regime)],
         perc_change_DW   = DW_mean/BAU_DW * 100 - 100) %>%
  dplyr::select(c(climChange, 
                  regime, 
                  perc_change_risk,
                  perc_change_HSI,
                  perc_change_DW)) %>%
  pivot_longer(!c(regime, climChange), #everything(vars = NULL),
               names_to = "Indicator", 
               values_to = "perc_ch")  %>%
  mutate(Indicator = factor(Indicator, 
                            levels = c('perc_change_risk', 'perc_change_HSI', 'perc_change_DW' ),
                            labels = c('Wind damage risk','Combined HSI', 'Deadwood volume'))) %>% 
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
        legend.position = c(.88, .22), # legend position within the plot, x, y
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

  



# Make bar plot of changes in biodiversity indicators given regime and climate change --------
df.ind.diff <-  
df.out %>% 
  group_by(climChange, regime) %>% # modif, #geo_grad,
  summarise(mean_risk    = mean(V, na.rm = T),
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



# Dodge bar plot for harvested timber volume 
windows(width = 7, height = 2.5)
df.vol2 %>% 
  filter(harv_type != 'totalV_mean') %>%
  ggplot(aes(x=regime, 
             y=value)) + 
  geom_bar(aes(color = harv_type, 
               group = harv_type,
               fill=harv_type), 
           position = 'dodge',
           width = 0.6,
           stat="identity")  +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"),
                    name = "Timber type", 
                      labels = c("Log", "Pulp")#,
                      ) +
  scale_color_manual(values = c("#00AFBB", "#E7B800"), 
                     name = "Timber type", 
                     labels = c("Log", "Pulp") ) +
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
  summarise(mean_risk = mean(V, na.rm = T),
            mean_HSI  = mean(COMBINED_HSI, na.rm = T))  %>%
  mutate(control_risk = mean_risk[match('BAU', regime)],
         control_HSI  = mean_HSI[ match('BAU', regime)],
         perc_change_HSI  = mean_HSI /control_HSI  * 100 - 100,
         perc_change_risk = mean_risk/control_risk * 100 - 100)# %>%
#data.table::fwrite(paste(inPath, "output/plotting", 'id_1000.csv', sep = "/"))




