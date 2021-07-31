# ----------------------------------
# Make plots:
# ----------------------------------


# Read data and make plots
# data form N-S gradient and climate change

# Make working example for no climate changes; then calculate values for CC scenario 
rm(list = ls())


setwd("C:/MyTemp/myGitLab/windDamage")

# Read libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(rgdal)
library(ggspatial)
library(rgeos)
library(raster)
library(dplyr)
library(spData)
library(sf)
library(RColorBrewer)


# Correct order of variables, factors & levels
# need to have same names of columns?? YES
# when data are sourced (source()), they are all available in my script
source("C:/MyTemp/myGitLab/windDamage/myFunctions.R")




# Set themes ----
theme_set(theme_classic())
theme_update(panel.grid.major = element_line(colour = "grey95",
                                             size = 0.1,
                                             linetype = 2),
             strip.background = element_rect(color="grey95", 
                                             fill="grey95",
                                             size=0.1, 
                                             linetype="solid"))





# get data
# ----------------------
inPath = "C:/MyTemp/myGitLab/windDamage/manuscript_regimes"
inFolder = "output_CC"
#outFolder = 'output_CC'

df.names = list.files(paste(inPath, inFolder, sep = "/"), pattern = ".csv$")

# Read dataframes
df.ls <- lapply(df.names, function(name, ...) data.table::fread(paste(inPath, inFolder, name,  sep = "/"),  # 
                                                           data.table=FALSE, stringsAsFactors = FALSE))


lapply(df.ls, function(df) unique(df$siteName))


# modify the structure of df
df.ls1 <- lapply(df.ls, function(df, ...) {
    df1 <- df %>%
    mutate(modif = case_when(
      grepl('_m5' , regime) ~ 'shorten',
      grepl('_m20', regime) ~ 'shorten',
      grepl('_5'  , regime) ~ 'extended',
      grepl('_10' , regime) ~ 'extended',
      grepl('_15' , regime) ~ 'extended',
      grepl('_30' , regime) ~ 'extended',
      grepl("CCF_1", regime)  ~ "shorten",
      grepl("CCF_3", regime)  ~ "extended",
      grepl("CCF_4", regime)  ~ "extended",
      TRUE~ 'normal')) 
    return(df1)
}
   )

# Classify the type of regime, type of adjustement (extension or shortening)
# and change in time (how many years)
df.ls2 <- lapply(df.ls1, function(df, ...)  {
  df1 <- df %>%
    mutate(change_time = case_when(
      grepl("_15", regime)  ~ "15",
      grepl("_5",  regime)  ~ "5",
      grepl("_10", regime)  ~ "10",
      grepl("_30", regime)  ~ "30",
      grepl("_20", regime)  ~ "20",
      grepl("_m5", regime)  ~ "-5",
      grepl("_m20", regime) ~ "-20",
      grepl("CCF_1", regime)  ~ "-5",
      grepl("CCF_2", regime)  ~ "0",
      grepl("CCF_3", regime)  ~ "5",
      grepl("CCF_4", regime)  ~ "10",
      TRUE~'0'))
  
  return(df1)

} )
  
# add dominant regime

df.ls3 <- lapply(df.ls2, function(df, ...)  {
  df1 <- df %>%
      mutate(regime = replace(regime, regime == "BAU", "BAU_")) %>% # replace BAU to facilitate further classification
      mutate(mainType = case_when(
        grepl("BAUwGTR"     , regime) ~ "BAUwGTR",
        grepl("BAUwT_GTR"   , regime) ~ "BAUwT_GTR",
        grepl("BAU_"        , regime) ~ "BAU",
        grepl("BAUwT"       , regime) ~ "BAUwT",
        grepl("BAUwoT"      , regime) ~ "BAUwoT",
        grepl("SA"          , regime) ~ "SA",
        grepl("CCF"         , regime) ~ "CCF")) %>%
      mutate(thinning = case_when(
        grepl("wG|wT"       , regime) ~ "thin_YES",
        grepl("woT"         , regime) ~ "thin_NO",
        grepl("SA_DWextract", regime) ~ "thin_NO",
        TRUE~'thin_YES'))
  return(df1)
})


# Merge data together
df.out <- do.call(rbind, df.ls3)



# add Geo gradient: -------------------
df.out <- df.out %>% 
  mutate(geo_grad = case_when(
    grepl("Korsnas", siteName) ~ "center",
    grepl("Raasepori", siteName) ~ "south",
    grepl("Simo", siteName) ~ "north" ))

 

# Change order of change time---------------------------------------
df.out$change_time <-factor(df.out$change_time, 
                              levels = c("-20", "-15", "-10", "-5", "0",  "5",  "10", "15","20", "25", "30"))
 
# Change order of change time---------------------------------------
df.out$climChange <-factor(df.out$climChange, 
                            levels = c("no", "cc45", "cc85"))

# Change order of change time---------------------------------------
df.out$geo_grad <-factor(df.out$geo_grad, 
                           levels = c("south", "center", "north"))



# Get time delay for CCF -----------------------------------------------
# get approaximate delay in time
# for all clim change???
df.out %>% 
  filter(mainType == "CCF" & geo_grad == "center") %>% 
  distinct(id)

# select id = 28248101
df.out %>% 
  filter(mainType == "CCF" & geo_grad == "center" & id == "28248101" & climChange == "no") %>%
 # distinct(climChange) 
  ggplot(aes(x = year,
             y = BA,
             col = regime)) +
  geom_line() +
  scale_x_continuous(breaks = seq(2020, 2100, by = 10)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) #+
  #facet_grid(.~regime)
  #dplyr::select(year, Age, regime, BA, V) %>% 
  #arrange(regime) 
  #filter()

# BA breaks: -3, 0, +3, +6 (basal area)
# time_change -5, 0, 5, 10


# Check initial conditions ------------------------------------------------

# HOw to fill in stands structure to have a continuous landscape every time?
# use my main category
# can I reuse values for missing fatds from BAU no change (time_change = 0)
# for specific climate change scenarios?
df.out %>% 
  filter(year == 2016 & regime == "BAU_") %>% # "SA_DWextract") %>% #"BAU_") %>% # SA_DWextract
  #group_by(siteName,  climChange) %>% 
  distinct(id) #%>% 
# 790 unique stands for SA
# 618 unique sites with BAU_ in 2106 ~ 80% stands under management
# why? are they not productive or what?
# let's check the values for stands under SA and under BAU in 2016!
windows()
df.out %>% 
  filter(year == 2016 & (regime == "BAU_"| regime == "SA_DWextract")) %>% #"BAU_") %>% # SA_DWextract
  ggplot(aes(x = regime,
             y = H_dom,
             fill = regime)) +
  geom_boxplot() +
  facet_grid(.~geo_grad)


windows()
df.out %>% 
  filter(year == 2016 & (regime == "BAU_"| regime == "SA_DWextract")) %>% #"BAU_") %>% # SA_DWextract
  ggplot(aes(x = regime,
             y = BA,
             fill = regime)) +
  geom_boxplot() +
  facet_grid(.~geo_grad)

# check for final year??
windows()
df.out %>% 
  filter(year == 2111 & (regime == "BAU_"| regime == "SA_DWextract")) %>% #"BAU_") %>% # SA_DWextract
  ggplot(aes(x = regime,
             y = H_dom,
             fill = regime)) +
  geom_boxplot() +
  facet_grid(.~geo_grad)


windows()
# Make sure that each stand have only one regime???
df.out %>% 
  filter(year == 2111 & (regime == "BAU_"| regime == "SA_DWextract")) %>% #"BAU_") %>% # SA_DWextract
  #distinct(regime)
  ggplot(aes(x = regime,
             y = BA,
             fill = regime)) +
  geom_boxplot() +
  facet_grid(.~geo_grad)



# 1836 # BAU_
# 2370 # SA
# how many stands in total?
length(unique(df.out$id)) # only 790 stands




# Filter only the stands that have all regimes ----------------------------
# does not work??? 

head(df.out)
# Filter rows by groups: https://stackoverflow.com/questions/65110401/r-dplyr-filter-common-values-by-group



regime = c("a", "a", "a",
          "b", "b", "b",
          "c", "c", "c")
id = c(1,2,4,
        8,4,5,
        1,3, 4)

dd <- data.frame(regime,
                 id)



dd %>%
  group_by(id) %>%
  filter(n_distinct(regime) == n_distinct(regime))
  #filter(n_distinct(regime))


# Here it seems that every stand has  all regimes? for KOrsnas?


# Filter only stands that have all regimes
# how many regimes has each stand??? --------------------------------------------------------------------



# number of stands in KOrsnas;
df.out %>% 
  filter(siteName == "Korsnas" & climChange == "no" & regime == "BAUwT_30") %>% 
  dplyr::select(id, year, H_dom,  siteName, climChange,regime ) %>% 
 # print(n = 80)
  distinct(id) #%>% 
  #nrow()
# 297


#df2<-
  df.out %>% 
  filter(siteName == "Korsnas" & climChange == "no" & regime == "CCF_4") %>% 
 #   nrow()
  group_by(id) %>%
  filter(n_distinct(regime) == n_distinct(df.out$regime)) %>% 
    distinct(id)
  
  


  
# Test hypotheses:  --------------------------------------------------------

# select only Korsnas - do not put there N-S gradient
# select only regimes to test my hypotheses: RF, adaptation, w/wo thinning, CCF
# keep climate change
# for old trees: use indicator N_where_D_gt_40
# check for clarity just the last 30 years

# H1: 
# We suggest that shorter rotation length will reduce wind damage risk
# and conflict with biodiversity (old trees). 
# This effect will decrease with more sever climate change.

# all have thinings if not specified otherwise 

# Do I have a variation between CC scenarios and sites?
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


# Make a plot: how does Climate change and regime modification affect wind damage risk?
windows()
df.out %>% 
  filter(geo_grad == "center") %>% 
  filter(mainType == "BAU" | mainType == "CCF") %>%  
  filter(windRisk < quantile(windRisk, 0.95, na.rm = T))  %>%  # filter out data above certain percentile
  group_by(year, climChange, change_time, modif, mainType ) %>% 
  summarise(my_y = mean(windRisk, na.rm = T)) %>% 
  ungroup() %>% 
  ggplot(aes(x = year )) +
  my_shade_pt() +
  ylab("wind risk")


# for HSI
windows()
df.out %>% 
  filter(geo_grad == "center") %>% 
  filter(mainType == "BAU" | mainType == "CCF") %>%  
  filter(windRisk < quantile(windRisk, 0.95, na.rm = T))  %>%  # filter out data above certain percentile
  group_by(year, climChange, change_time, modif, mainType ) %>% 
  summarise(my_y = mean(COMBINED_HSI, na.rm = T)) %>% 
  ungroup() %>% 
  ggplot(aes(x = year )) +
  my_shade_pt() +
  ylab("COMB_HSI")
  


# Make scatter plot: HSI vs wind risk given climChange 
df.out %>% 
  filter(geo_grad == "center") %>% 
  filter(mainType == "BAU" | mainType == "CCF") %>%  
  filter(windRisk < quantile(windRisk, 0.95, na.rm = T))  %>%  # filter out data above certain percentile
  group_by(year, climChange, change_time, modif, mainType ) %>% 
  summarise(my_risk = mean(windRisk, na.rm = T),
            my_HSI  = mean(COMBINED_HSI, na.rm = T)) %>% 
  #ungroup() %>% 
  ggplot(aes(x = my_risk,
             y = my_HSI,
             col = mainType)) +
  geom_point(alpha = 0.5)  +
  facet_grid(climChange~modif)



# Plot differences in HSI and risk given adaptation
df.out %>% 
  filter(geo_grad == "center" & climChange == "no") %>% 
  filter(mainType == "BAU" | mainType == "CCF") %>%  
  filter(windRisk < quantile(windRisk, 0.95, na.rm = T))  %>%  # filter out data above certain percentile
  group_by(climChange, modif, mainType ) %>% 
  summarise(my_risk = mean(windRisk, na.rm = T),
            my_HSI  = mean(COMBINED_HSI, na.rm = T)) #%>% 
  


# Get % change between groups ---------------------------------------------------
set.seed(5)
dd <- data.frame(id = rep(c(1:4), 3),
                 val = c(rnorm(4) +2,
                         rnorm(4) +3,
                         rnorm(4) +4),
                 grp = rep(c("control", "ch1", "ch2"), each = 4))

dd %>% 
  group_by(grp) %>% 
  summarise(my_mean = mean(val)) 

dd %>% 
  group_by(grp) %>% 
  summarise(my_mean = mean(val)) %>%
  mutate(perc_change = (my_mean - my_mean[grp == 'control'])/my_mean[grp == 'control']*100) %>% 

 # mutate(perc_change = scales::percent((my_mean - my_mean[grp == 'control'])/my_mean[grp == 'control'])) %>% 
  ggplot(aes(x = grp,
             y = perc_change)) +
  geom_col()





# Make simple point with poit range:
# consider change = adaptation, climate change and gradient
df.out %>% 
  filter(mainType != "CCF" & mainType != "SA") %>% 
  filter(windRisk < quantile(windRisk, 0.95, na.rm = T))  %>%  
  group_by(geo_grad, climChange, change_time,  modif ) %>% # modif,
  summarise(my_y = mean(windRisk, na.rm = T)) %>%
  ggplot(aes(x = change_time,
             y = my_y,
             #shape = climChange,
             color = climChange)) +
    geom_point() + 
  facet_grid(.~geo_grad)



# H1: for thinning included or not? need to set only to 'modif' not to 'time_change'
# as thinnings does not have all categories
df.out %>% 
  filter(mainType != "CCF" & mainType != "SA") %>% 
  filter(windRisk < quantile(windRisk, 0.95, na.rm = T))  %>%  
  group_by(geo_grad, climChange, modif, thinning ) %>% # modif,
  summarise(my_y = mean(windRisk, na.rm = T)) %>%
  ggplot(aes(x = modif,
             y = my_y,
             group = climChange,
             color = climChange)) +
  geom_point() + 
  geom_line() +
  facet_grid(thinning~geo_grad)




# Make plot of wind risk over years?
library(ggthemes)
library("scales")
df.out %>% 
  filter(mainType == "CCF" | mainType == "BAU") %>% 
  filter(windRisk < quantile(windRisk, 0.95, na.rm = T))  %>%  
  group_by(year, geo_grad, climChange, modif, mainType) %>% # modif,
  summarise(my_y = mean(windRisk, na.rm = T)) %>%
  ggplot(aes(x = year)) +
  geom_ribbon(data = ~ pivot_wider(.,
                       names_from = climChange,
                      values_from = my_y),
  aes(ymin = no,
     ymax = cc85,
     fill = modif), alpha = 0.2)  +
  geom_line(aes(y = my_y,
              color = modif,     
              linetype = climChange),
          lwd  = 1) +
  scale_linetype_manual(values=c('dotted', 'solid', 'dashed')) +
  theme_bw() +
  facet_grid(mainType ~ geo_grad) +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 90, 
                                 vjust = 0.5, 
                                 face="plain", 
                                 size = 9, 
                                 family = "sans")) #

  
  
 
# get for HSi:
df.out %>% 
  filter(mainType == "CCF" | mainType == "BAU") %>% 
  filter(windRisk < quantile(windRisk, 0.95, na.rm = T))  %>%  
  group_by(year, geo_grad, climChange, modif, mainType) %>% # modif,
  summarise(my_y = mean(COMBINED_HSI, na.rm = T)) %>%
  ggplot(aes(x = year)) +
  geom_ribbon(data = ~ pivot_wider(.,
                                   names_from = climChange,
                                   values_from = my_y),
              aes(ymin = no,
                  ymax = cc85,
                  fill = modif), alpha = 0.2)  +
  geom_line(aes(y = my_y,
                color = modif,     
                linetype = climChange),
            lwd  = 1) +
  scale_linetype_manual(values=c('dotted', 'solid', 'dashed')) +
  theme_bw() +
  facet_grid(mainType ~ geo_grad) +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, 
                                   face="plain", 
                                   size = 9, 
                                   family = "sans")) +
  ylab("Combined HSI")


# !!!!! make category for CCF as delayed! CCF_1 to CCF_4!
# or make category 

# Make plot with median annd quantiles:
windows(7,2.5)
df.out %>% 
  filter(mainType != "CCF" & mainType != "SA") %>% 
  filter(windRisk < quantile(windRisk, 0.95, na.rm = T))  %>%  
  ggplot() + 
  stat_summary(mapping = aes(x = modif, #change_time
                             y = windRisk,
                             group = climChange,
                             col = climChange),
               fun.min = function(z) { quantile(z,0.25) },
               fun.max = function(z) { quantile(z,0.75) },
               fun = median,
               position=position_dodge(width=0.4)) +
  facet_grid(.~geo_grad) +
  theme_classic()+
  theme(legend.position = "bottom")




# Make plot for HSI:
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



# Put HSI and wind risk in one plot ass scatter plot, color by adaptation and clim change -------------------
df.out %>% 
  filter(mainType != "CCF" & mainType != "SA") %>% 
  filter(windRisk < quantile(windRisk, 0.95, na.rm = T))  %>%  
  group_by(geo_grad, climChange, modif ) %>% # modif,
  summarize(m.risk  = median(windRisk, na.rm = T),
            m.HSI   = median(COMBINED_HSI, na.rm = T)) %>% 
  ggplot(aes(x = m.HSI,
             y = m.risk,
             color =  climChange,
             group = climChange,
            # size = modif,
             shape = modif)) +
  geom_point(size = 2) + 
  geom_line(aes(linetype = climChange)) +
  xlim(0,1) +
  ylim(0,0.04)+
  facet_grid(.~geo_grad) + 
  theme_classic()
  





# why is no adaptation always higher then adaptation?? 
# What is the stand age given adaptation?

df.out %>% 
  filter(geo_grad == "center" & (mainType != "CCF" & mainType != "SA") & year == 2016) %>% 
  filter(windRisk < quantile(windRisk, 0.95, na.rm = T))  %>%  
  sample_n(5000) %>% 
 # group_by(geo_grad, climChange, change_time,  modif ) %>% # modif,
 # summarise(mean_risk = mean(windRisk, na.rm = T),
  #          mean_age = mean(Age, na.rm = T)) %>%
  ggplot(aes(x = windRisk,
             y = Age)) +
  geom_point(alpha = 0.2) + 
  geom_smooth(method = "gam") + 
  facet_grid(.~change_time)
  



# Plot risk vs  biodiversity ----------------------------------------------

df.out %>% 
  filter((mainType != "CCF" & mainType != "SA")) %>% # geo_grad == "center" & 
  filter(windRisk < quantile(windRisk, 0.95, na.rm = T))  %>%  
  sample_n(5000) %>% 
  ggplot(aes(x = windRisk,
             y = COMBINED_HSI,
             group = interaction(climChange, thinning),
             col = interaction(climChange, thinning))) +
  #geom_point(alpha = 0.2) + 
  #scale_color_manual(values = c("red",  "red",  "red", "black", "black", 'black')) +
 # scale_linetype_manual(values = c("dotted", 'dashed',  'solid')) +
  geom_smooth(method = "gam") + 
  facet_grid(geo_grad~modif) 



# Chech the changes in mean Age over the landscape given adaptation regime?
windows(7,2.5)
df.out %>% 
  filter(year !=  2015) %>% 
  #filter(geo_grad == "center" ) %>% #
#  filter(Age < quantile(Age, 0.95, na.rm = T))  %>%  
  group_by(geo_grad, climChange, modif,  year) %>% # modif, change_time
  summarise(my_y = mean(Age, na.rm = T)) %>%
  ggplot(aes(x = year,
             y = my_y)) +
  geom_line(aes(linetype = climChange,
                color =  modif ),
             lwd = 1) + 
  ylab("Mean age\n (landscape)") +
  ylim(0,100) + 
  facet_grid(.~geo_grad)



# check wind damage risk?
windows(7,2.5)
df.out %>% 
  filter(year !=  2015) %>% 
  #filter(geo_grad == "center" ) %>% #
  filter(windRisk < quantile(windRisk, 0.95, na.rm = T))  %>%  
  group_by(geo_grad, climChange, modif,  year) %>% # modif, change_time
  summarise(my_y = mean(windRisk, na.rm = T)) %>%
  ggplot(aes(x = year,
             y = my_y)) +
  geom_line(aes(linetype = climChange,
                color =  modif ),
            lwd = 1) + 
  ylab("Mean wind risk\n (landscape)") +
  #ylim(0,100) + 
  facet_grid(.~geo_grad)



# Compare age between 2016-2111?
windows()
df.out %>% 
  filter(year == 2016) %>% 
  filter(mainType != "CCF" | mainType != "SA") %>% 
  #filter(Age < quantile(Age, 0.95, na.rm = T))  %>%  
  group_by(geo_grad, climChange, change_time,  modif ) %>% # modif,
  summarise(my_y = mean(Age, na.rm = T)) %>%
  ggplot(aes(x = change_time,
             y = my_y,
             #shape = climChange,
             color = climChange)) +
  geom_point() + 
  ylab("Age 2016") +
  facet_grid(.~geo_grad) +
  ylim(0,120)


windows()
df.out %>% 
  filter(year == 2111) %>% 
  filter(mainType != "CCF" | mainType != "SA") %>% 
  #filter(Age < quantile(Age, 0.95, na.rm = T))  %>%  
  group_by(geo_grad, climChange, change_time,  modif ) %>% # modif,
  summarise(my_y = mean(Age, na.rm = T)) %>%
  ggplot(aes(x = change_time,
             y = my_y,
             #shape = climChange,
             color = climChange)) +
  geom_point() + 
  ylab("Age 2111") +
  ylim(0,120) +
  facet_grid(.~geo_grad)







# check out for conservation values: N_where_D_gt_40
df.out %>% 
  filter(mainType != "CCF" | mainType != "SA") %>% 
  filter(N_where_D_gt_40 < quantile(windRisk, 0.95, na.rm = T))  %>%  
  group_by(geo_grad, climChange, change_time,  modif ) %>% # modif,
  summarise(my_y = mean(windRisk, na.rm = T)) %>%
  ggplot(aes(x = change_time,
             y = my_y,
             #shape = climChange,
             color = climChange)) +
  geom_point() + 
  facet_grid(.~geo_grad)




# Make XY scatter plot: wind risk vs. biodiversity?
df.out %>% 
  filter(year == 2016 |year == 2111 ) %>% 
  filter(mainType != "CCF" | mainType != "SA") %>% 
  group_by(geo_grad, climChange,   modif, year ) %>% # modif, change_time,
  summarise(m_risk = mean(windRisk, na.rm = T),
            m_Age  = mean(Age, na.rm = T),
            m_HSI  = mean(COMBINED_HSI, na.rm = T))  %>%
  ggplot(aes(x = m_HSI,
             y = m_risk )) +
  geom_point(aes(color = climChange)) +
  facet_grid(year ~geo_grad)
  


# Calculate teh % on change: compare specific BAU scenario
# get % of change given adaptation, climate change and adaptation+ climaChange
# only for Korsnas
unique(df.out$mainType)


# Get a unique reference: this varyu by group of harvest (Kyle's colors)
# Keep the reference value separated, as later I need to add the means to mean value of change
# my reference is BAU, no clim change and change_time = 0
df.bau.ref <- 
  df.out %>%
  filter(windRisk < quantile(windRisk, 0.95, na.rm = T))  %>%  # filter out data above certain percentile
  filter(siteName == "Korsnas" & mainType == "BAU" & climChange == "no" & change_time == "0") %>% 
  group_by(year, change_time, climChange, regime) %>% 
  summarize(w_risk_ref = mean(windRisk, na.rm = T))  %>% 
  ungroup() %>% 
  dplyr::select(year, w_risk_ref)


# get table for other values to calculate the change
#df.bau.ch <- 
  df.out %>%  
  filter(windRisk < quantile(windRisk, 0.95, na.rm = T))  %>%  # filter out data above certain percentile
  filter(siteName == "Korsnas" & mainType == "BAU" & climChange != "no" & change_time != "0") %>% 
  group_by(year, modif, climChange, change_time) %>% 
  summarize(w.mean = mean(windRisk, na.rm = T))   %>%
  left_join(df.bau.ref, by = "year") %>% 
  mutate(perc_ch = w.mean / w_risk_ref*100 -100 )   %>%    # calculate percentage change
  ungroup()  %>% 
  #print(n = 50)
    ggplot(aes(x = year)) +  # ,
    #y = perc_ch
    #geom_ribbon(
    #  data = ~ pivot_wider(., 
    #                       names_from = climChange, 
    #                       values_from = perc_ch),
    #  aes(ymin = cc85, 
    #      ymax = cc45, 
    #      fill = change_time), 
    #  alpha = 0.2) +
    geom_line(aes(y = perc_ch,
                  color = change_time,
                  linetype = climChange),
              lwd  = 1)  +
    facet_grid(.~change_time)
   
















# BAU has thinnings;
# check if all regimes have thinings??
regime.v <- unique(df.out$regime)

lapply(regime.v, function(x, ...) {df.out %>%
             filter(regime == x) %>% 
         distinct(THIN) }
       )



# select id: 29609064
df.out %>% 
 # filter(id =="29609064" & (regime == "BAU"|regime == "BAUwT"| regime == "SA_DWextract")) %>%
  filter(id =="29609064" & (regime == "BAUwT")) %>%
  filter((regime == "BAUwT")) %>%
  dplyr::select(id, year, THIN, BA, H_dom, V, regime, climChange) %>% 
  distinct(year)
  #distinct(THIN) 


# get stands id for BAUwT
df.out %>% 
  filter(id == "31326051" & (regime == "BAU"|regime == "BAUwT" )) %>%  
  dplyr::select(id, year, THIN, BA, H_dom, V, regime, climChange) %>% 
  arrange(year, regime)


# check how many yeasr I have for each rregime
table(df.out$regime, df.out$year)




# Keep the reference value separated, as later I need to add the means to mean value of change
# my reference is BAU, no clim change and change_time = 0
df.bau.ref <- 
  df.out %>%
  filter(windRisk < quantile(windRisk, 0.95, na.rm = T))  %>%  # filter out data above certain percentile
  filter(siteName == "Korsnas" & regime == "BAU" & climChange == "no" & change_time == "0") %>% 
  group_by(year, change_time, climChange, thinning, regime) %>% 
  summarize(w_risk_ref = mean(windRisk, na.rm = T))  %>% 
  ungroup() %>% 
  dplyr::select(year, w_risk_ref)
 

# make historgram of N_where_D_gt_40
df.out %>% 
  ggplot(aes(x = N_where_D_gt_40,
             fill = interaction(siteName))) +
  geom_histogram(alpha = 0.6, binwidth = 10) + 
  facet_grid(regime~climChange)

#!!!!!!

# Make a point plot of the means wind risk, adaptation, climate change
df.out %>% 
  filter(windRisk < quantile(windRisk, 0.95, na.rm = T))  %>%  # filter out data above certain percentile
  filter(siteName == "Korsnas" & mainType == "BAU") %>% 
  group_by(year, climChange, thinning, change_time) %>% 
  summarize(w.mean = mean(windRisk, na.rm = T),
            oldT.mean  = mean(N_where_D_gt_40, na.rm = T))#  %>%
  

# where are the old tree values missing???
df.out %>% 
  group_by(siteName, climChange) %>% 
  summarize(oldT.mean  = mean(N_where_D_gt_40, na.rm = T))


# complete from here by calculated the changes towards reference values (mean BAU by year)
# make ribbon plot of changes
windows()
df.out %>%  
  filter(windRisk < quantile(windRisk, 0.95, na.rm = T))  %>%  # filter out data above certain percentile
  filter(siteName == "Korsnas" & mainType == "BAU" & regime != "BAU" & thinning == "thin_YES") %>% 
  group_by(year, modif, climChange, thinning, change_time) %>% 
  summarize(w.mean = mean(windRisk, na.rm = T))  %>%
  left_join(df.bau.ref, by = "year") %>% 
  mutate(perc_ch = w_risk_ref/w.mean*100 - 100)   %>%               # calculate percentage change
  print(n = 50)
  
  ungroup() %>% 
  ggplot(aes(x = year)) +
  #geom_ribbon(
   # data = ~ pivot_wider(.,
    #                     names_from = climChange,
    #                     values_from = perc_ch),
    #aes(ymin = no, 
    #    ymax = cc85, 
     #   fill=change_time), alpha = 0.2  )# +
  geom_line(aes(y = perc_ch,
                color = change_time,     
                linetype = climChange),
            lwd  = 1)  +
  facet_grid(.~ change_time)
  
  
  scale_linetype_manual(values=c('dotted', 'solid', 'dashed'))# +
 # theme_bw()  +
  ggtitle("Effect of extension or shortening of BAU on wind damage risk") +
  ylab("Wind risk change (%)") +
  facet_grid(.~modif) #+ 
  #theme(legend.position = 'bottom',
   #     axis.text.x = element_text(angle = 90, 
    #                               vjust = 0.5, 
     #                              face="plain", 
      #                             size = 9, 
       #                            family = "sans"))
  
  #geom_point() + 
  geom_line() + 
  facet_grid(climChange~thinning)



# Make plots --------------------------------------------------------------


# differences in wind risk given CC and geo region


# Calculate mean values for no change scenario 
# to add it as horizontal line to facets
df.m <- 
  df.out %>% 
  filter(mainType == "BAU"  & change_time == '0' & climChange == "no") %>%
  group_by(geo_grad) %>% 
  dplyr::summarize(Mean = mean(windRisk, na.rm=TRUE))
 

df.out %>%  
  filter(mainType == "BAU" ) %>% 
  ggplot(aes(y = windRisk,
             x = factor(change_time),
             fill = climChange)) + 
  geom_boxplot() +
  ylim(0,0.25) +
  geom_hline(yintercept =df.m$Mean) +
  facet_wrap(.~ geo_grad,  scales="free_y")

# NOt fininished, not clear how to plot one line by facet???


df.out %>% 
  filter(mainType == "BAU") %>% 
  ggplot(aes(y = windRisk,
             x = factor(change_time),
             fill = climChange)) + 
  geom_boxplot() +
  ylim(0,0.25) +
  geom_hline(xintercept = 3) +
  facet_wrap(.~ geo_grad,  scales="free_y")



# check risk for regimes-------------------------------------------
df.out %>% 
  filter(mainType == "BAU"  ) %>% 
  ggplot(aes(x = modif, #climChange,
             y= windRisk,
             group = modif, #climChange,
             fill = modif)) + #, climChange)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom="point", col = "red") +
  # ylim(0, 0.25) +
  facet_grid(thinning ~ climChange) +
  theme(legend.position = "none") + 
  ggtitle("BAU")


windows()
df.out %>% 
  filter(mainType == "CCF") %>% 
  ggplot(aes(x = climChange,
             y= windRisk,
             group = climChange,
             fill = climChange)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom="point", col = "red") +
  ylim(0, 0.25) +
  facet_grid(. ~ modif) +
  theme(legend.position = "none") + 
  ggtitle("CCF")


# check the risk by time delay
# Make plots for change time -----------------------------------------------------------------
p.extended<- 
  df.out %>% 
  filter(mainType == "BAU") %>%
  filter(modif == "extended" | modif == "normal" ) %>% 
  ggplot(aes(x = change_time,
             y= windRisk
             #group = climChange,
  )) +
  geom_boxplot(aes(fill = climChange), position=position_dodge(.9)) +
  ylim(0, 0.20) +
  stat_summary(aes(group =climChange ), position=position_dodge(.9), 
               fun = mean, geom="point", col = "red") +
  
  #facet_grid(climChange ~ year) +
  #theme(legend.position = "none") + 
  ggtitle("a) BAU extended")


p.shorten<- 
  df.out %>% 
  filter(mainType == "BAU") %>% 
  filter(modif == "shorten" | modif == "normal") %>% 
  ggplot(aes(x = change_time,
             y= windRisk
             #group = climChange,
  )) +
  geom_boxplot(aes(fill = climChange), position=position_dodge(.9)) +
  ylim(0, 0.20) +
  stat_summary(aes(group =climChange ), position=position_dodge(.9), 
               fun = mean, geom="point", col = "red") +
  
  #facet_grid(climChange ~ year) +
  #theme(legend.position = "none") +
  ggtitle("b) BAU shortened")

library(ggpubr)
ggarrange(p.extended, p.shorten, common.legend = TRUE, legend="bottom")



# Plots for Climate change ------------------------------------------------

p.extended<- 
  df.out %>% 
  filter(mainType == "BAU") %>% 
  filter(modif == "extended" | modif == "normal") %>% 
  ggplot(aes(x = climChange, 
             y= windRisk
             #group = ,
  )) +
  geom_boxplot(aes(fill = change_time), position=position_dodge(.9)) +
  ylim(0, 0.20) +
  stat_summary(aes(group =change_time ), position=position_dodge(.9), 
               fun = mean, geom="point", col = "red") +
  
  #facet_grid(climChange ~ year) +
  #theme(legend.position = "none") + 
  ggtitle("a) extended")


p.shorten<- 
  df.out %>% 
  filter(modif == "shorten" | modif == "normal") %>% 
  ggplot(aes(x = climChange,
             y= windRisk
             #group = ,
  )) +
  geom_boxplot(aes(fill = change_time), position=position_dodge(.9)) +
  ylim(0, 0.20) +
  stat_summary(aes(group =change_time ), position=position_dodge(.9), 
               fun = mean, geom="point", col = "red") +
  
  #facet_grid(climChange ~ year) +
  #theme(legend.position = "none") +
  ggtitle("b) shortened")

library(ggpubr)
ggarrange(p.extended, p.shorten, common.legend = FALSE, legend="bottom")


# What regime is the most sensitive to shortening/extension? -----------------

p.extended<- 
  df.out %>% 
  filter(modif == "extended" | modif == "normal") %>% 
  ggplot(aes(x = change_time,
             y= windRisk
             #group = climChange,
  )) +
  geom_boxplot(aes(fill = climChange), position=position_dodge(.9)) +
  ylim(0, 0.20) +
  stat_summary(aes(group =climChange ), position=position_dodge(.9), 
               fun = mean, geom="point", col = "red") +
  
  #facet_grid(climChange ~ year) +
  #theme(legend.position = "none") + 
  ggtitle("a) extended")


p.shorten<- 
  df.out %>% 
  filter(modif == "shorten"  & mainType == "BAU") %>% 
  ggplot(aes(x = change_time,
             y= windRisk
             #group = climChange,
  )) +
  geom_boxplot(aes(fill = climChange), position=position_dodge(.9)) +
  ylim(0, 0.20) +
  stat_summary(aes(group =climChange ), position=position_dodge(.9), 
               fun = mean, geom="point", col = "red") +
  facet_grid(.~ regime) +
  ggtitle("b) shortened")


ggarrange(p.extended, p.shorten, common.legend = TRUE, legend="bottom")



# need to compare specific regime with its modification:

# Line plots  -------------------------------------------------------------


df.out %>% 
  filter(modif == "shorten"  & mainType == "BAU") %>% 
  ggplot(aes(x = change_time,
             y= windRisk
             #group = climChange,
  )) +
  geom_boxplot(aes(fill = climChange), position=position_dodge(.9)) +
  ylim(0, 0.20) +
  stat_summary(aes(group =climChange ), position=position_dodge(.9), 
               fun = mean, geom="point", col = "red") +
  facet_grid(.~ regime) +
  ggtitle("b) shortened")


# get shaded region  ----------------------------------------------
# Get the line plot, for one regime over years
windows()
df.out %>% 
  filter(mainType == "BAU" & modif == "extended" ) %>%  #
  #filter(mainType == "BAU") %>% 
  group_by(year, climChange, change_time, modif) %>% 
  summarise(my_y = mean(windRisk, na.rm = T)) %>% 
  #arrange(year, change_time) %>% 
  #print(n = 80) #%>% 
   
  ungroup() %>% 
  ggplot(aes(x = year )) +
  #geom_line(aes(y = my_y))
  geom_ribbon(
    data = ~ pivot_wider(., 
                         names_from = climChange, 
                         values_from = my_y),
    aes(ymin = no, 
        ymax = cc85, fill=change_time), alpha = 0.2) +
  geom_line(aes(y = my_y,
                color = change_time,     
                linetype = climChange),
            lwd  = 1)  +
  scale_linetype_manual(values=c('dotted', 'solid', 'dashed')) +
  theme_bw()  +
  ggtitle("Effect of extension or shortening of BAU on wind damage risk") +
  ylab("Wind damage risk (%)") +
  facet_grid(.~modif) + 
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, 
                                   face="plain", 
                                   size = 9, 
                                   family = "sans"))





# get means by SA, CCF and BAU (CCF does not have modifications in time)
df.out %>% 
  #filter(mainType == "BAU" & modif == "extended" ) %>%  #
  # filter(mainType == "CCF") %>% 
  group_by(year, climChange, mainType) %>% 
  summarise(my_y = mean(windRisk, na.rm = T)) %>% 
  ungroup() %>% 
  ggplot(aes(x = year )) +
  #geom_line(aes(y = my_y))
  geom_ribbon(
    data = ~ pivot_wider(., 
                         names_from = climChange, 
                         values_from = my_y),
    aes(ymin = no, 
        ymax = cc85, fill=mainType), alpha = 0.2) +
  geom_line(aes(y = my_y,
                color = mainType,     
                linetype = climChange),
            lwd  = 1)  +
  scale_linetype_manual(values=c('dotted', 'solid', 'dashed')) +
  theme_bw()  +
  ggtitle("Effect of management variation\non wind damage risk") +
  ylab("Wind damage risk (%)") +
  # facet_grid(.~modif) + 
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, 
                                   face="plain", 
                                   size = 9, 
                                   family = "sans"))










df.out2 %>% 
  filter(mainType == "BAU" & modif == "shorten" ) %>%  #
  group_by(year, climChange, change_time) %>% 
  summarise(my_y = mean(windRisk, na.rm = T)) %>% 
  ungroup() %>% 
  ggplot(aes(x = year )) +
  #geom_line(aes(y = my_y))
  geom_ribbon(
    data = ~ pivot_wider(., 
                         names_from = climChange, 
                         values_from = my_y),
    aes(ymin = no, 
        ymax = cc85), fill="grey80",alpha=0.4) +
  geom_line(aes(y = my_y,
                #color = "black",     
                linetype = climChange),
            lwd  = 1)  +
  theme_bw()  +
  facet_grid(.~change_time)



# put together and color by specific groups
df.out2 %>% 
  filter(mainType == "BAU" ) %>%  # | mainType == "SA"
  # filter(mainType == "SA") %>% 
  group_by(year, climChange, change_time) %>% 
  summarise(my_y = mean(windRisk, na.rm = T))  %>% 
  # ungroup() %>% 
  ggplot(aes(x = year,
             y = my_y,
             linetype = climChange,
             color = change_time)) +  #
  geom_line()  +
  # facet_grid(.~mainType) +
  #scale_color_continous() +
  theme_bw()







# Make a different colur of shade by group

library(ggplot2)
library(tidyr)

# example for shaded line plot
dd1 <- data.frame(year = c(1:5),
                  grp = rep(c("a", "b", "c"), each = 5),
                  vals = c(5, 5.2, 5.6, 5.8, 6,
                           5, 4.9, 4.8, 4.7, 4.2,
                           5, 4.8, 4.4, 4,   3),
                  modif = rep('no', each = 15))

dd2 <- dd1
dd2$vals = dd1$vals*0.8
dd2$modif = 'yes'

# create a new factor


dd <- rbind(dd1, dd2)
dd$comb = paste(dd$modif, dd$grp, sep = "_")



dd %>% 
  ggplot(aes(x = year)) +
  ylim(0,6.5) +
  #geom_ribbon(
  #  data = ~ pivot_wider(., names_from = grp, 
  #                      values_from = vals),
  #  aes(ymin = c, ymax = a, fill = modif)
  #) +
  geom_line(aes(y = vals,
                color = interaction(modif, grp)),
            lwd  = 1.5)  +
  theme_bw()


dd %>% 
  ggplot(aes(x = year)) +
  geom_ribbon(
    data = ~ pivot_wider(., names_from = grp,
                         values_from = vals),
    aes(ymin = c, ymax = a, fill = modif, alpha = 0.25)
  ) +
  ylim(0,6.5) +
  geom_line(aes(y = vals, 
                color = modif, 
                linetype = grp),  # color = interaction(modif, grp)
            lwd  = 1.5)  +
  scale_color_grey()+
  # scale_fill(alpha = 0.25) +
  theme_bw() #+
#facet_grid(modif~.)




# Make a different colur of shade by group

library(ggplot2)
library(tidyr)

# example for shaded line plot
dd1 <- data.frame(year = c(1:5),
                  grp = rep(c("a", "b", "c"), each = 5),
                  vals = c(5, 5.2, 5.6, 5.8, 6,
                           5, 4.9, 4.8, 4.7, 4.2,
                           5, 4.8, 4.4, 4,   3),
                  modif = rep('no', each = 15))

dd2 <- dd1
dd2$vals = dd1$vals*0.8
dd2$modif = 'yes'

# Put data together
dd <- rbind(dd1, dd2)


# Get a plot
dd %>% 
  ggplot(aes(x = year)) +
  geom_ribbon(
    data = ~ pivot_wider(., names_from = grp,
                         values_from = vals),
    aes(ymin = c, ymax = a, fill = modif)
  ) +
  ylim(0,6.5) +
  geom_line(aes(y = vals, color = modif, linetype = grp), 
            lwd  = 1.5)  +
  scale_fill_manual(c("red", "blue")) +
  
  theme_bw()








# Dummy example ------------------------------------------------------------------
# pass values by group as a new column
dd <- data.frame(id   = rep(1,9),
                 val  = c(10, 11, 12, 10, 15, 20,10, 30, 50),
                 cc   = rep(c("no", "c1", 'c2'), each = 3),
                 year = rep(c(1:3), 3)) 


# calculate change towards reference group by year
dd %>% 
  group_by(year) %>%
  mutate(val.ref = val[cc == "no"])

# fill in values based on a reference group

