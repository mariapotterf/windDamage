
# ------------------------
# Calculate difference between wind risk value between neighbors
# ------------------------

# get simulated data with calculated wind risk
# get list of neighbors to get the values from the simulated data
# compare the heights differences between neighbors. Maybe take just one with 
# open edge??



# Results and methods for paper 

# Read libraries ----
rm(list = ls())


library(data.table)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(broom)


# Set themes ----
theme_set(theme_classic())
theme_update(panel.grid.major = element_line(colour = "grey95",
                                             size = 0.1,
                                             linetype = 2),
             strip.background = element_rect(color="grey95", 
                                             fill="grey95",
                                             size=0.1, 
                                             linetype="solid"))

source("C:/MyTemp/myGitLab/windDamage/myFunctions.R")


# Set wd ------------------------------------------------------------------

setwd("C:/MyTemp/myGitLab/windDamage/output/even_flow")

# Read input data ----------------------------------------------------

# includes optimal scenario, raster data, windRisk
df <- fread(paste(getwd(), "finalFoPlotting.csv", sep = "/"))

# get list of neighbors: central cell and neighbors, get wind risk values
df_nbr <- data.table::fread(paste(getwd(), "df_nbrs_diff.csv", sep = "/"),
                                  data.table=FALSE, 
                                  stringsAsFactors = FALSE)

# filter df_nbrs to have only central-neighbors pairs:
df_nbr2 <- df_nbr %>% 
  dplyr::select(central_id, nbrs_id) %>% 
  distinct()


# Out data
outTabRisk    = "df_nbrs_diff_risk.csv"  # difference between wind risk values between neighbors
outTabHeight  = "df_nbrs_diff_height.csv"  # difference between wind risk values between neighbors


# Filter the data to get values of wind risk between 
# neighbors
# maybe make data shorter?
df_filt <- df %>% 
  dplyr::select(id, landscape, year, windRisk, Management, scenSimpl2, open_edge, H_dom, NPI)


# Merge two dataframes together
df_out<-
  df_filt %>% 
  left_join(df_nbr2, by = c('id' = 'central_id')) %>%
  left_join(df_filt, by = c('landscape' = 'landscape',
                            'nbrs_id' = 'id'),
            suffix = c('', '_nbrs')) 


# Does SA have any sheltering effect on surrounding actively managed stands??
# Gete differences only between SA and surrounding actively managed stands 
df_out2 <- df_out %>% 
  filter(Management == "Set Aside" & Management_nbrs == "Active") #%>% 
  #filter(H_dom > 160 & H_dom_nbrs > 160)



# ====================================================================
#     Get a mean height difference between SA - active neighbor
# ====================================================================

#df_diff_H <-
ylab_H_diff = "Height difference (dm)"

p.H_diff.npi <-  
  df_out2 %>% 
  mutate(H_diff_c_nbr = H_dom - H_dom_nbrs) %>% 
  group_by(scenSimpl2, 
             NPI) %>% 
  summarize(my_y = mean(H_diff_c_nbr, na.rm =T )) %>%
    ggplot(aes(x = NPI,
               y = my_y,
               group = scenSimpl2,
               col = scenSimpl2,
               linetype = scenSimpl2)) +
    geom_line() + ylab(ylab_H_diff) +
    ggtitle("Height difference\nbetween SA and neighbors") +
     ylim(-25, 80)+ 
    plot_line_details2()

  

  
# height differences over time
windows()

p.H_diff.time <- 
  df_out2 %>% 
    mutate(H_diff_c_nbr = H_dom - H_dom_nbrs) %>% # central height mins neighbors: ie. central is higher then neighbors
    group_by(scenSimpl2, 
             year) %>% 
    summarize(my_y = mean(H_diff_c_nbr, na.rm =T )) %>%
    ggplot(aes(x = year,
               y = my_y,
               group = scenSimpl2,
               col = scenSimpl2,
               linetype = scenSimpl2)) +
    geom_line() + ylab(ylab_H_diff) +
  ylim(-25, 80)+ 
    ggtitle("Height difference\nbetween SA and neighbors") +
  plot_line_details2()
  


ggarrange(p.H_diff.npi,  p.H_diff.time, 
          ncol = 2, nrow = 1,
          common.legend = TRUE,
          align = c("hv"),
          legend="bottom",
          labels= "AUTO",
          hjust = -5,
          vjust = 3,
          font.label = list(size = 10, 
                            face = "bold", 
                            color ="black"))





# ====================================================================
#     Get a mean windRisk difference between SA - active neighbor
# ====================================================================

#df_diff_H <-
# Difine label for wind risks
ylab_risk = "Wind damage\nprobability (%)"


windows()

p.risk_diff.npi <-  
  df_out2 %>% 
  mutate(risk_diff = windRisk - windRisk_nbrs) %>% 
  group_by(scenSimpl2, 
           NPI) %>% 
  summarize(my_y = mean(risk_diff, na.rm =T )) %>%
  ggplot(aes(x = NPI,
             y = my_y*100,
             group = scenSimpl2,
             col = scenSimpl2,
             linetype = scenSimpl2)) +
  geom_line() +
    ylab(ylab_risk) +
  ylim(-2.5,2) +
  ggtitle("Risk difference\nbetween SA and neighbors") +
    plot_line_details2()


p.risk_diff.time <- 
  df_out2 %>% 
  mutate(risk_diff = windRisk - windRisk_nbrs) %>% 
  group_by(scenSimpl2, 
           year) %>% 
  summarize(my_y = mean(risk_diff, na.rm =T )) %>%
  ggplot(aes(x = year,
             y = my_y*100,
             group = scenSimpl2,
             col = scenSimpl2,
             linetype = scenSimpl2)) +
  geom_line() +
    ylab(ylab_risk) +
  ylim(-2.5,2) +
  ggtitle("Risk difference\nbetween SA and neighbors") +
    plot_line_details2()

ggarrange(p.risk_diff.npi,  p.risk_diff.time, 
          ncol = 2, nrow = 1,
          common.legend = TRUE,
          align = c("hv"),
          legend="bottom",
          labels= "AUTO",
          hjust = -5,
          vjust = 3,
          font.label = list(size = 10, 
                            face = "bold", 
                            color ="black"))








# =================================================================
#          Filter the stands that have at least one SA neighbor
# =================================================================
  # that have no SA neighbors
# filter stands that have SA as neighbors
# filter stands that do not have SA as neighbors
# will they wind risk values change???
df_SA <- 
  df_out %>% 
  group_by(id, landscape) %>% 
  filter(Management == "Active" & any(Management_nbrs == "Set Aside")) %>% 
  select(id, landscape, year, windRisk, scenSimpl2, NPI, Management, H_dom) %>% 
  distinct() %>% 
  mutate(SA_nbr = "Yes")
  

df_act <- 
  df_out %>% 
  filter(Management == "Active" & Management_nbrs == "Active") %>% 
  select(id, landscape, year, windRisk, scenSimpl2, NPI, Management, H_dom) %>% 
  distinct() %>%
  mutate(SA_nbr = "No")

# Merge two data together, need to convert them to dataframe first (from tibble, etc)
df_by_SA <- rbind(data.frame(df_SA), 
                  data.frame(df_act))


# Is there is a difference in wind risk if SA neighbors with actively managed stands?? ========================================


# BY NPI
#windows()
p.risk.npi <-
  df_by_SA %>%
 # filter(H_dom > 160) %>% 
  group_by(NPI, scenSimpl2, SA_nbr) %>% 
  summarize(my.y = mean(windRisk, na.rm = T)) %>% 
  ggplot(aes(x = NPI,
             y = my.y*100,
             col = scenSimpl2,
             linetype =scenSimpl2)) +
  ylim(0,6) +
  geom_line() +
  ylab(ylab_risk) +
  facet_grid(.~ SA_nbr) +
  plot_line_details2()


# by time
#windows()
p.risk.time <- 
  df_by_SA %>%
  #filter(H_dom > 160) %>% 
  group_by(year, scenSimpl2, SA_nbr) %>% 
  summarize(my.y = mean(windRisk, na.rm = T)) %>% 
  ggplot(aes(x = year,
             y = my.y*100,
             #group = scenSimpl2,
             col = scenSimpl2,
             linetype =scenSimpl2 )) +
  ylim(0,6) +
  geom_line() +
  ylab(ylab_risk) +
  facet_grid(.~ SA_nbr) +
  plot_line_details2()


# For >16m tall

p.risk.npi16 <-
  df_by_SA %>%
  filter(H_dom > 160) %>% 
  group_by(NPI, scenSimpl2, SA_nbr) %>% 
  summarize(my.y = mean(windRisk, na.rm = T)) %>% 
  ggplot(aes(x = NPI,
             y = my.y*100,
             col = scenSimpl2,
             linetype =scenSimpl2)) +
  ylim(0,6) +
  geom_line() +
  ylab(ylab_risk) +
 facet_grid(.~ SA_nbr) +
  plot_line_details2()


# by time
#windows()
p.risk.time16 <- 
  df_by_SA %>%
  filter(H_dom > 160) %>% 
  group_by(year, scenSimpl2, SA_nbr) %>% 
  summarize(my.y = mean(windRisk, na.rm = T)) %>% 
  ggplot(aes(x = year,
             y = my.y*100,
             #group = scenSimpl2,
             col = scenSimpl2,
             linetype =scenSimpl2 )) +
  ylim(0,6) +
  geom_line() +
  ylab(ylab_risk) +
  facet_grid(.~ SA_nbr) +
  plot_line_details2()






ggarrange(p.risk.npi,  p.risk.time,
          p.risk.npi16,  p.risk.time16,
          ncol = 2, nrow = 2,
          common.legend = TRUE,
          align = c("hv"),
          legend="bottom",
          labels= "AUTO",
          hjust = -5,
          vjust = 3,
          font.label = list(size = 10, 
                            face = "bold", 
                            color ="black"))




# ==================================================
# filter the stands that have at least 3 nbrs set aside
# =====================================================


# !!!! does not work, finnalize tommorow!!


# Filter the stands that have at least one SA neighbor;
# that have no SA neighbors
# filter stands that have SA as neighbors
# filter stands that do not have SA as neighbors
# will they wind risk values change???
#df_SA <- 
  df_out %>% 
  filter(Management == "Active" & Management_nbrs == "Set Aside") # %>% 
 # group_by(landscape) %>% 
  filter(n(Management_nbrs == 3))
  #filter(Management == "Active" & any(Management_nbrs == "Set Aside")) %>% 
  filter(Management == "Active") %>% 
  tally()

select(id, landscape, year, windRisk, scenSimpl2, NPI, Management, H_dom) %>% 
  mutate(SA_nbr = "Yes")


df_act <- 
  df_out %>% 
  filter(Management == "Active" & Management_nbrs == "Active") %>% 
  select(id, landscape, year, windRisk, scenSimpl2, NPI, Management, H_dom) %>% 
  mutate(SA_nbr = "No")

# Merge two data together, need to convert them to dataframe first (from tibble, etc)
df_by_SA <- rbind(data.frame(df_SA), 
                  data.frame(df_act))


# Is there is a difference in wind risk if SA neighbors with actively managed stands?? =============================

# BY NPI
#windows()
p.risk.npi <-
  df_by_SA %>%
  # filter(H_dom > 160) %>% 
  group_by(NPI, Management, scenSimpl2, SA_nbr) %>% 
  summarize(my.y = mean(windRisk, na.rm = T)) %>% 
  ggplot(aes(x = NPI,
             y = my.y*100,
             group = scenSimpl2,
             col = scenSimpl2)) +
  #geom_point() + 
  # stat_summary(fun = mean, fun.min = min, fun.max = max) +
  ylim(0,6) +
  geom_line() +
  ylab("Wind damage risk (%)") +
  facet_grid(.~ SA_nbr) +
  plot_line_details2()


# by time
#windows()
p.risk.time <- df_by_SA %>%
  #filter(H_dom > 160) %>% 
  group_by(year, Management, scenSimpl2, SA_nbr) %>% 
  summarize(my.y = mean(windRisk, na.rm = T)) %>% 
  ggplot(aes(x = year,
             y = my.y*100,
             group = scenSimpl2,
             col = scenSimpl2)) +
  ylim(0,6) +
  geom_line() +
  ylab("Wind damage risk (%)") +
  facet_grid(.~ SA_nbr) +
  plot_line_details2()


ggarrange(p.risk.npi,  p.risk.time, 
          ncol = 2, nrow = 1,
          #widths = c(1, 1),
          common.legend = TRUE,
          align = c("hv"),
          #font.label = list(size = 10, color = "black", face = "plain", family = NULL),
          legend="bottom",
          labels= "AUTO",
          hjust = -5,
          vjust = 3,
          font.label = list(size = 10, 
                            face = "bold", 
                            color ="black"))





# dummy example: 
# filter by number of groups

dd <- data.frame(id = rep(c(1,2,3), 3),
                 group1 = rep(c("a", "b", "d"), each = 3),
                 group2 = c("Y", "Y","N",
                            "Y", "Y", "N",
                            "N", "N", "N"))

# Filter data by group2 to if has exactly 2 occurences of "Y"
dd %>% 
  group_by(group1, group2) %>% 
  filter(n() == 1)

# ==================================================================
# Do actively managed stands increase risks in set asides??
# ==================================================================

# Filter the stands that have at least one SA neighbor;
# that have no SA neighbors
# filter stands that have SA as neighbors
# filter stands that do not have SA as neighbors
# will they wind risk values change???
df_act2 <- 
  df_out %>% 
  group_by(id, landscape) %>% 
  filter(Management == "Set Aside" & any(Management_nbrs == "Active")) %>% 
  select(id, landscape, year, windRisk, scenSimpl2, NPI, Management, H_dom) %>% 
  mutate(ACT_nbr = "Yes")


df_SA2 <- 
  df_out %>% 
  filter(Management == "Set Aside" & Management_nbrs == "Set Aside") %>% 
  select(id, landscape, year, windRisk, scenSimpl2, NPI, Management, H_dom) %>% 
  mutate(ACT_nbr = "No")

# Merge two data together, need to convert them to dataframe first (from tibble, etc)
df_by_ACT <- rbind(data.frame(df_act2), 
                  data.frame(df_SA2))


# Does the presence of actively managed stands increase the risk in SA? 

# BY NPI
#windows()
p.risk.npi2 <-
  df_by_ACT %>%
  filter(H_dom > 160) %>% 
  group_by(NPI, Management, scenSimpl2, ACT_nbr) %>% 
  summarize(my.y = mean(windRisk, na.rm = T)) %>% 
  ggplot(aes(x = NPI,
             y = my.y*100,
             group = scenSimpl2,
             col = scenSimpl2)) +
  #geom_point() + 
  # stat_summary(fun = mean, fun.min = min, fun.max = max) +
  ylim(0,6) +
  geom_line() +
  ylab("Wind damage risk (%)") +
  facet_grid(.~ ACT_nbr) +
  plot_line_details2()


# by time
#windows()
p.risk.time2 <- df_by_ACT %>%
  filter(H_dom > 160) %>% 
  group_by(year, Management, scenSimpl2, ACT_nbr) %>% 
  summarize(my.y = mean(windRisk, na.rm = T)) %>% 
  ggplot(aes(x = year,
             y = my.y*100,
             group = scenSimpl2,
             col = scenSimpl2)) +
  ylim(0,6) +
  geom_line() +
  ylab("Wind damage risk (%)") +
  facet_grid(.~ ACT_nbr) +
  plot_line_details2()


ggarrange(p.risk.npi2,  p.risk.time2, 
          ncol = 2, nrow = 1,
          #widths = c(1, 1),
          common.legend = TRUE,
          align = c("hv"),
          #font.label = list(size = 10, color = "black", face = "plain", family = NULL),
          legend="bottom",
          labels= "AUTO",
          hjust = -5,
          vjust = 3,
          font.label = list(size = 10, 
                            face = "bold", 
                            color ="black"))


# =============================================================
# By NPI
#windows()
p.mean.risk.c.line.npi <-
  df_out2 %>% 
#  filter(H_dom > 160) %>% 
  group_by(scenSimpl2, 
           NPI, 
           Management) %>% 
  summarize(my_y = mean(windRisk, na.rm =T )) %>%
  ggplot(aes(x = NPI,
             y = my_y*100,
             group = scenSimpl2,
             col = scenSimpl2)) +
  geom_line() +
  # geom_boxplot()+
  ylim(0,7) + 
  ggtitle("Risk in SA (>16m)")



#windows()
p.mean.risk.nbrs.line.npi <-
  df_out2 %>% 
#  filter(H_dom > 160) %>% 
  group_by(scenSimpl2, 
           NPI, 
           Management) %>% 
  summarize(my_y = mean(windRisk_nbrs, na.rm =T )) %>%
  ggplot(aes(x = NPI,
             y = my_y*100,
             group = scenSimpl2,
             col = scenSimpl2)) +
  geom_line() +
  # geom_boxplot()+
  ylim(0,7) +
  ggtitle("Risk in active nbrs (>16m)")


# BY time


#windows()
p.mean.risk.c.line.time<-
  df_out2 %>% 
#  filter(H_dom > 160) %>% 
  group_by(scenSimpl2, 
           year, 
           Management) %>% 
  summarize(my_y = mean(windRisk, na.rm =T )) %>%
  ggplot(aes(x = year,
             y = my_y*100,
             group = scenSimpl2,
             col = scenSimpl2)) +
  geom_line() +
  # geom_boxplot()+
  ylim(0,7) + 
  ggtitle("Risk in SA (>16m)")


#windows()
p.mean.risk.nbrs.line.time<-
df_out2 %>% 
#  filter(H_dom > 160) %>% 
  group_by(scenSimpl2, 
           year, 
           Management) %>% 
  summarize(my_y = mean(windRisk_nbrs, na.rm =T )) %>%
  ggplot(aes(x = year,
             y = my_y*100,
             group = scenSimpl2,
             col = scenSimpl2)) +
  geom_line() +
 # geom_boxplot()+
  ylim(0,7) +
  ggtitle("Risk in active nbrs (>16m)")



ggarrange(p.mean.risk.c.line.npi,  p.mean.risk.nbrs.line.npi, 
          p.mean.risk.c.line.time, p.mean.risk.nbrs.line.time,
          ncol = 2, nrow = 2,
          #widths = c(1, 1),
          common.legend = TRUE,
          align = c("hv"),
          #font.label = list(size = 10, color = "black", face = "plain", family = NULL),
          legend="bottom",
          labels= "AUTO",
          hjust = -5,
          vjust = 3,
          font.label = list(size = 10, 
                            face = "bold", 
                            color ="black"))




# ==============================================================================
# PLot : 
# # Why is SA in RF always having lower wind risk? -----------------------------
# ==============================================================================

windows()
#p.mean.risk.c.line.npi <-
df_out2 %>% 
  #  filter(H_dom > 160) %>% 
  group_by(scenSimpl2, 
           NPI, 
           Management) %>% 
  summarize(my_y = mean(H_dom, na.rm =T )) %>%
  ggplot(aes(x = NPI,
             y = my_y,
             group = scenSimpl2,
             col = scenSimpl2)) +
  geom_line() +
  # geom_boxplot()+
  ylim(190,240) +
  ggtitle("H_dom in SA (>16m)")



windows()
df_out2 %>% 
  #  filter(H_dom > 160) %>% 
  group_by(scenSimpl2, 
           year, 
           Management) %>% 
  summarize(my_y = mean(H_dom, na.rm =T ))%>%
  ggplot(aes(x = year,
             y = my_y,
             group = scenSimpl2,
             col = scenSimpl2)) +
  geom_line() +
  # geom_boxplot()+
  ylim(190,240) + 
  ggtitle("H_dom in SA (>16m)")











# ------------------------------

df.nbrs2 <-
  df.nbrs.risk %>% 
  mutate(abs_diff_rsk = abs(windRisk- windRisk_nbrs))  %>%
  group_by(id, landscape) %>% 
  summarise(abs_diff_rsk = mean(abs_diff_rsk, na.rm = T))

# Join calculated values of risk differences to simulated data
df <-
  df %>% 
  left_join(df.nbrs2, by = c("id", "landscape"))

# increase absolute difference wiind risk by 100%
df$abs_diff_rsk =  df$abs_diff_rsk*100

# --------------------

# write the table
#fwrite(merged.nbrs.df, "C:/MyTemp/myGitLab/windDamage/output/even_flow/df_nbrs_diff.csv")
fwrite(df_out, paste(getwd(), outTab , sep = "/"))  
  
  
  

