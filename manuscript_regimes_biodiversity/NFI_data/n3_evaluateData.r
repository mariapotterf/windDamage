# ----------------------------------
# Make plots:
# ----------------------------------


# Read data and make tables and plots
rm(list = ls())


setwd("C:/Users/ge95bag/Documents/git/windDamage")

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
source("C:/Users/ge95bag/Documents/git/windDamage/myFunctions.R")




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
inPath = "C:/Users/ge95bag/Documents/Projects/2021_WindRisk_biodiversity"
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


# Sample the specific IDs
#my_ids  <- unique(df.ls[[1]]$id)
#sub_ids <- sample(my_ids, 25000) 

# convert from integer64 to numeric:
#sub_ids <- as.numeric(sub_ids)

# subset teh same stands from each regime
#df.ls2 <- list()
#df.ls2 <- lapply(df.ls, function(df) df %>% 
        # filter(id %in% sub_ids))


df.ls2 <- df.ls

# remove unnecessary columns
cl_keep <- c(
  "year",
  "branching_group",
  "Age",
  "PV",
  "cash_flow",
  "BA",
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
  # "MAIN_SP"
  "CAPERCAILLIE",
  "HAZEL_GROUSE" ,
  "THREE_TOED_WOODPECKER",
  "LESSER_SPOTTED_WOODPECKER",
  "LONG_TAILED_TIT",
  "SIBERIAN_FLYING_SQUIRREL",
  "COMBINED_HSI",
   "name",
  # "cell"
  "id",
  #"avgTemp"
  #"windSpeed"
  "regime",
  "adapt",
  "magnit" ,
  #[35] "THIN2"                     "THIN_filled_lagged"
  #[37] "difference"
  "since_thin",
  "windRisk"
)

# check if I have the same stands all over??
# lapply(df.ls2, function(df) length(unique(df$id))) # the final number varies between regimes


# keep only needed columns
df.ls2 <- lapply(df.ls2, function(df) df %>% 
         dplyr::select(cl_keep))


# Merge data together -----------------------------------------------
df.out <- do.call(rbind, df.ls2)

rm(df.ls)

# Classify climate change
df.out <- df.out %>% 
  mutate(climChange = case_when(
    grepl("RCP0", name)  ~ "REF",
    grepl("RCP45", name) ~ "RCP45",
    grepl("RCP85", name) ~ "RCP85"))


# Correct regimes names
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


# Change order of change time---------------------------------------
df.out$climChange <-factor(df.out$climChange, 
                            levels = c("REF", "RCP45", "RCP85"))


# Order the regimes by 'intensity': from the most intensive to the least intensive: 
# a bit questionnable if my order is correct? CCF can be very intensive n terms of thinning
# ordered in terms of timing of final cut
df.out <- df.out %>% 
  mutate(regime = factor(regime, 
                         levels = c("short_30", "short_10","BAU", "noThin", "ext_10", "ext_30", "GTR", "CCF")))




# Crazy values !!!! 
# Filter the data ------------------------------------------

# deadwood volume seems incredible high for GTR: check for mean and max values!
range(df.out$V)

#[1]    0.000 1239.226
# Max volume is 600 m3/ha

range(df.out$V_total_deadwood)
# 0.0 191014.6
# max deadwood volume is for each species 300m3/ha


range(df.out$H_dom, na.rm = T)
# [1]  0.5869817 41.2494774

# which stands are crazy??

# Check V
df.out %>% 
  filter(V > 600 | V_total_deadwood > 300 ) %>% 
  distinct(id) #%>% # regime, 
  nrow()


# Check deadwood
df.out %>% 
  filter(V_total_deadwood > 300) %>% 
  distinct(id) #%>% # regime, 
  #nrow()


# Remove crazy values and try print plots and tables again:
# Make a copy of the table
df.all <- df.out
length(unique(df.all$id))


# remove crazy values
df.out <- df.out %>% 
  filter(V < 600 ) %>% 
  filter(V_total_deadwood < 300)

length(unique(df.out$id))

# keep the vector of ids: !!!! this goes later in the summary table script!!
my_stands = unique(df.out$id)


# -------------------
length(unique(df.all$id))


# Get histograms
hist(df.out$V)
hist(df.out$V_total_deadwood)


median(df.out$V_total_deadwood)
mean(df.out$V_total_deadwood)


# Test hypotheses:  --------------------------------------------------------

# select only regimes to test my hypotheses: RF, adaptation, w/wo thinning, CCF
# keep climate change
# biodindicator




# Define labels:
lab_manag = c("Regime adaptation")


# Investigate skewness of data: should I use mean or median??

names(df.out)

# Make several histograms at once: reshape the data from wide to long:
# whould I use median or mean??? 
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

# Make a histogram for all indicators
df.ind %>% 
  ggplot(aes(y = HSI,
             x = indicator)) + 
  geom_violin()
 # geom_histogram(binwidth = 30) + 
  facet_wrap(.~indicator)
  



# H1: 
# We suggest that shorter rotation length will reduce wind damage risk
# and conflict with biodiversity (HSI, old trees). 
# This effect will decrease with more sever climate change.

# Calculate the differences between mean values of wind risk 
# for each scenarios, BAU is a reference


# simple plotting of differences between regimes and CC
# wind risk --------------------
df.out %>% 
  group_by(id, regime, climChange) %>% 
  summarize(my_risk = mean(windRisk, na.rm = T)) %>% 
  ggplot(aes(x = regime,
             y = my_risk*100,
             fill = climChange)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# combined HSI
df.out %>% 
  group_by(id, regime, climChange) %>% 
  summarize(mean_HSI = mean(COMBINED_HSI, na.rm = T)) %>% 
  ggplot(aes(x = regime,
             y = mean_HSI,
             fill = climChange)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



# -----------------------------------------
#    Final plots                          #
# -----------------------------------------


# Barplot differenes in wind damage risk compared to BAU by climate change  --------------

# Make barplots with CI around it!
# group by ID ad then calculate teh differences?

# df.out %>% 
#   group_by(id, climChange, regime) %>% # modif, #geo_grad, id, 
#  # head()
#   summarise(windRisk_mean = mean(windRisk, na.rm = T),
#             HSI_mean = mean(COMBINED_HSI, na.rm = T)) %>% 
#   mutate(BAU_HSI          = HSI_mean[match('BAU', regime)],
#          perc_change_HSI  = HSI_mean/BAU_HSI * 100 - 100,
#          BAU_risk         = windRisk_mean[match('BAU', regime)],
#          perc_change_risk = windRisk_mean/BAU_risk * 100 - 100) #%>%
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
  summarise(windRisk_mean = mean(windRisk, na.rm = T),
            HSI_mean = mean(COMBINED_HSI, na.rm = T),
            DW_mean = mean(V_total_deadwood, na.rm = T),) %>% 
  mutate(BAU_HSI          = HSI_mean[match('BAU', regime)],
         perc_change_HSI  = HSI_mean/BAU_HSI * 100 - 100,
         BAU_risk         = windRisk_mean[match('BAU', regime)],
         perc_change_risk = windRisk_mean/BAU_risk * 100 - 100,
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
    facet_grid(.~Indicator) +
    coord_flip() +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        #legend.position = 'bottom',
        legend.position = c(.90, .25), # legend position within the plot, x, y
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
windows(height = 3, width = 7)
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
windows(height = 3, width = 7)
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
windows(height = 3, width = 7)
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
windows(height = 3, width = 7)
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
windows(height = 3, width = 7)
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
        legend.position = c(.88, .78), # legend position within the plot, x, y
        legend.title = element_text(size=10),
        legend.text  = element_text(size=8),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.box.background = element_rect(colour = "black")) 




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
         p_change_SQIRR  = mean_SQIRR /control_SQIRR * 100 - 100,
         control_DW      = mean_DW[match('BAU', regime)],
         p_change_DW     = mean_DW /control_DW * 100 - 100,
         control_HSI     = mean_HSI[match('BAU', regime)],
         p_change_HSI    = mean_HSI /control_HSI * 100 - 100) %>% 
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
  summarise(mean_sum_V_log     = round(mean(sum_V_log, na.rm = T),1),
            sd_sum_V_log       = round(sd(sum_V_log, na.rm = T),1),
            mean_sum_V_pulp    = round(mean(sum_V_pulp, na.rm = T),1),
            sd_sum_V_pulp      = round(sd(sum_V_pulp, na.rm = T), 1)) 
  

# Second, get the summary table:

df_summary_main <-
  df.out %>% 
  group_by(climChange, regime) %>% 
  summarise(windRisk_mean = round(mean(windRisk, na.rm = T)*100, digits = 2),
            windRisk_sd   = round(sd(windRisk, na.rm = T)*100, digits = 2),
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









# Calculate the % change to plot on the map
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
    #geom_text(aes(label=regime,
     #             color = regime),
      #        hjust=0.5, 
       #       vjust=0.5),
    geom_vline(xintercept = 0, color = "grey30", lty = "dashed"), 
    geom_hline(yintercept = 0, color = "grey30", lty = "dashed"),
    geom_point(size = 2.5, shape = 21, color = 'black'), # +
    scale_fill_manual(values = my_cols_RdGn),
    scale_color_manual(values = my_cols_RdGn),
    #scale_fill_viridis_d(),
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

(p1 <- 
  df.species.means %>% 
  ggplot(aes(x = p_change_CAPER,
             y = p_change_risk,
             fill = regime)) + 
    ggtitle("a) capercaillie\n") +
    pt_details() +
  ylab(my_lab_risk))
  

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
  
windows(7,4.5)  
# https://rpkgs.datanovia.com/ggpubr/reference/annotate_figure.html
annotate_figure(species.plot,
                bottom = text_grob("Difference in HSI [%]", 
                                   color = "black",
                                   hjust = 0.5, 
                                   #x = -1, 
                                   y = 4.5,
                                   face = "plain", size = 9)
)
  








# Evaluate changes in mean age over landscape






  
  
  

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
          #            yend= yval*100,#mean_risk*100,
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
              'mean_risk', 'control_risk', 'perc_change_risk', 
              'mean_HSI',  'control_HSI',  'perc_change_HSI',
              'adaptPaste')

# Get a compare toward the BAU normal
df.plot <- 
    df.out2 %>% 
  #  filter(mainType != "SA" ) %>% # & climChange == "no"
    group_by(geo_grad, climChange, adapt, mainType) %>% # modif, #geo_grad,
    summarise(mean_risk = mean(windRisk, na.rm = T),
              mean_HSI  = mean(COMBINED_HSI, na.rm = T)) %>%
    mutate(adaptPaste = paste(adapt, mainType, sep = "_")) %>%
    group_by(geo_grad, climChange) %>% 
    mutate(control_risk = mean_risk[match('normal_BAUwT', adaptPaste)],
           control_HSI  = mean_HSI[ match('normal_BAUwT', adaptPaste)],
           perc_change_HSI  = mean_HSI /control_HSI  * 100 - 100,
           perc_change_risk = mean_risk/control_risk * 100 - 100) %>% 
    dplyr::select(all_of(my_cols)) %>% 
    filter(adapt != "normal") %>% 
    mutate(adapt = factor(adapt, 
                         levels = c("extended", "shorten", 'noThin', "GTR" ,"CCF" )))  
  
  

# Lollipop risk by adaptation: means, absolute ---------------------------------------
windows(7,5.5)
df.plot %>% 
  ggplot(aes(x = climChange,
             y = mean_risk*100)) +
  my_lollipop()  +
  geom_segment(aes(x= climChange, 
                    xend= climChange, 
                    y=0, 
                    yend= mean_risk*100,#mean_risk*100,
                    col = climChange)) +
 # ylim() +
  ylab("Mean wind damage risk")
  



windows(7,5.5)
df.plot %>% 
  filter(adapt != "normal") %>% 
  mutate(adapt = factor(adapt, 
                        levels = c("extended", "shorten", 'noThin', "GTR" ,"CCF" ))) %>% 
  ggplot(aes(x = climChange,
             y = mean_HSI)) +
  geom_hline(yintercept = c(0, 0.5), 
             col = 'grey80',
             lty = "dashed") +
  geom_segment( aes(x= climChange, 
                    xend= climChange, 
                    y=0, 
                    yend= mean_HSI,
                    col = climChange)) +
  geom_point(aes(col = climChange), size = 5) +
  scale_color_manual(values = cols_ylRd3) +
  ylim(0, 1) +
  ylab("Mean combined HSI") +
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
             'mean_risk', 'control_risk', 'perc_change_risk', 
             'mean_HSI',  'control_HSI',  'perc_change_HSI',
             'adaptPaste')

# CAPERCAILIE   =============================
windows(width = 7, height = 3)

df.CAP <- 
  df.out %>% 
  filter(mainType != "SA" ) %>% # & climChange == "no"
  group_by(geo_grad, climChange, adapt, mainType) %>% # modif, #geo_grad,
  summarise(mean_risk = mean(windRisk, na.rm = T),
            mean_HSI  = mean(CAPERCAILLIE, na.rm = T)) %>%
  mutate(adaptPaste = paste(adapt, mainType, sep = "_")) %>%
  group_by(geo_grad, climChange) %>% 
  mutate(control_risk = mean_risk[match('normal_BAUwT', adaptPaste)],
         control_HSI  = mean_HSI[ match('normal_BAUwT', adaptPaste)],
         perc_change_HSI  = mean_HSI /control_HSI * 100 - 100,
         perc_change_risk = mean_risk/control_risk * 100 - 100) %>% 
  #  print(n = 30) %>% 
  dplyr::select(col_keep) 


# Lollipop risk by adaptation: means, absolute ---------------------------------------

windows(7,5.5)
df.CAP %>% 
  filter(adapt != "normal") %>% 
  mutate(adapt = factor(adapt, 
                        levels = c("extended", "shorten", 'noThin', "GTR" ,"CCF" ))) %>% 
  ggplot(aes(x = climChange,
             y = mean_HSI)) +
  geom_hline(yintercept = c(0, 0.5), 
             col = 'grey80',
             lty = "dashed") +
  geom_segment( aes(x= climChange, 
                    xend= climChange, 
                    y=0, 
                    yend= mean_HSI,
                    col = climChange)) +
  geom_point(aes(col = climChange), size = 5) +
  scale_color_manual(values = cols_ylRd3) +
  ylim(0, 1) +
  ylab("Mean CAPERCAILLIE") +
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
  summarise(mean_risk = mean(windRisk, na.rm = T),
            mean_HSI  = mean(HAZEL_GROUSE, na.rm = T)) %>%
  mutate(adaptPaste = paste(adapt, mainType, sep = "_")) %>%
  group_by(geo_grad, climChange) %>% 
  mutate(control_risk = mean_risk[match('normal_BAUwT', adaptPaste)],
         control_HSI  = mean_HSI[ match('normal_BAUwT', adaptPaste)],
         perc_change_HSI  = mean_HSI /control_HSI * 100 - 100,
         perc_change_risk = mean_risk/control_risk * 100 - 100) %>% 
  #  print(n = 30) %>% 
  dplyr::select('climChange', 'adapt', 'mainType', 
                'mean_risk', 'control_risk', 'perc_change_risk', 
                'mean_HSI',  'control_HSI',  'perc_change_HSI',
                'adaptPaste') 


# Lollipop HAZEL_GROUSE by adaptation: means, absolute ---------------------------------------
windows(7,5.5)
df.HASEL %>% 
  filter(adapt != "normal") %>% 
  mutate(adapt = factor(adapt, 
                        levels = c("extended", "shorten", 'noThin', "GTR" ,"CCF" ))) %>% 
  ggplot(aes(x = climChange,
             y = mean_HSI)) +
  geom_hline(yintercept = c(0, 0.5), 
             col = 'grey80',
             lty = "dashed") +
  geom_segment( aes(x= climChange, 
                    xend= climChange, 
                    y=0, 
                    yend= mean_HSI,
                    col = climChange)) +
  geom_point(aes(col = climChange), size = 5) +
  scale_color_manual(values = cols_ylRd3) +
  ylim(0, 1) +
  ylab("Mean HAZEL_GROUSE") +
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
  summarise(mean_risk = mean(windRisk, na.rm = T),
            mean_HSI  = mean(LESSER_SPOTTED_WOODPECKER, na.rm = T)) %>%
  mutate(adaptPaste = paste(adapt, mainType, sep = "_")) %>%
  group_by(geo_grad, climChange) %>% 
  mutate(control_risk = mean_risk[match('normal_BAUwT', adaptPaste)],
         control_HSI  = mean_HSI[ match('normal_BAUwT', adaptPaste)],
         perc_change_HSI  = mean_HSI /control_HSI * 100 - 100,
         perc_change_risk = mean_risk/control_risk * 100 - 100) %>% 
  #  print(n = 30) %>% 
  dplyr::select('climChange', 'adapt', 'mainType', 
                'mean_risk', 'control_risk', 'perc_change_risk', 
                'mean_HSI',  'control_HSI',  'perc_change_HSI',
                'adaptPaste') 


# Lollipop LESSER_SPOTTED_WOODPECKER by adaptation: means, absolute ---------------------------------------
windows(7,5.5)
df.HASEL %>% 
  filter(adapt != "normal") %>% 
  mutate(adapt = factor(adapt, 
                        levels = c("extended", "shorten", 'noThin', "GTR" ,"CCF" ))) %>% 
  ggplot(aes(x = climChange,
             y = mean_HSI)) +
  geom_hline(yintercept = c(0, 0.5), 
             col = 'grey80',
             lty = "dashed") +
  geom_segment( aes(x= climChange, 
                    xend= climChange, 
                    y=0, 
                    yend= mean_HSI,
                    col = climChange)) +
  geom_point(aes(col = climChange), size = 5) +
  scale_color_manual(values = cols_ylRd3) +
  ylim(0, 1) +
  ylab("Mean LESSER_SPOTTED_WOODPECKER") +
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
  summarise(my_y = mean(Age, na.rm = T)) %>%
  ggplot(aes(x= mainType,
             y = my_y,
             color = modif,
             group = climChange)) +
  geom_point(aes()) #+
  ylab("Mean age") +
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




