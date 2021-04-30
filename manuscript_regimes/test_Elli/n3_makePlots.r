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
      TRUE~'0'))
  
  return(df1)

} )
  
# add dominant regime

df.ls3 <- lapply(df.ls2, function(df, ...)  {
  df1 <- df %>%
      mutate(mainType = case_when(
        grepl("SA", regime) ~ "SA",
        grepl("BAU", regime) ~ "BAU",
        grepl("CCF", regime) ~ "CCF")) %>%
      mutate(thinning = case_when(
        grepl("wG|wT", regime) ~ "thin_YES",
        grepl("woT", regime) ~ "thin_NO",
        TRUE~'no'))
  return(df1)
})


# Merge data together
df.out <- do.call(rbind, df.ls3)


# add Geo gradient:
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
df.out2 %>% 
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
df.out2 %>% 
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
  df.out2 %>% 
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
  df.out2 %>% 
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
  df.out2 %>% 
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
  df.out2 %>% 
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
  df.out2 %>% 
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
  df.out2 %>% 
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

library(ggpubr)
ggarrange(p.extended, p.shorten, common.legend = TRUE, legend="bottom")



# need to compare specific regime with its modification:

# Line plots  -------------------------------------------------------------


df.out2 %>% 
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
df.out2 %>% 
  #filter(mainType == "BAU" & modif == "extended" ) %>%  #
  filter(mainType == "BAU") %>% 
  group_by(year, climChange, change_time, modif) %>% 
  summarise(my_y = mean(windRisk, na.rm = T)) %>% 
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
df.out2 %>% 
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
