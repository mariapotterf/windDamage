# ----------------------------------
# Make plots:
# ----------------------------------



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
