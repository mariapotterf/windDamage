


# 
rm(list = ls())

# , eval = FALSE
library(data.table)
library(dplyr)
library(raster)
library(ggplot2)
library(sf)
library(stringr)
library(gridExtra)
library(tidyr)
library(ggpubr)


theme_set(theme_classic())
theme_update(panel.grid.major = element_line(colour = "grey95",
                                             size = 0.1,
                                             linetype = 2))

# Read datasets:
# ----------------------------------------
#df <- fread("C:/MyTemp/myGitLab/windDamage/output/even_flow/df_sim_windRisk.csv")
df <- fread("/projappl/project_2003256/windDamage/output/final_df_solution8.csv")




# -----------------------------------------
# Visualize volume dynamics over scenarios
# -----------------------------------------
names(df)

vol_cols <- c("V", 
              "V_stand_log",
              "V_stand_pulp",
              "Harvested_V",
              "Harvested_V_log_under_bark",
              "Harvested_V_pulp_under_bark")


# Check which Volume characteristics contains NA values??
for (i in vol_cols){
  print(i)
  print(anyNA(df[[i]]))
}


# -------------------------------------------
# plot sum of different volumes over x
# -------------------------------------------
# For harvested V - calculate sum, as this is not measured every time,
# otherwise calculate mean or max???

V_mean          <- aggregate(V ~ scenSimpl2 +  NPI, df, mean)
V_st_log_mean   <- aggregate(V_stand_log ~ scenSimpl2 +  NPI, df, mean)
V_st_pulp_mean  <- aggregate(V_stand_pulp ~ scenSimpl2 +  NPI, df, mean)

V_harv_sum      <- aggregate(Harvested_V ~ scenSimpl2 +  NPI, df, sum)
V_harv_log_sum  <- aggregate(Harvested_V_log_under_bark ~  scenSimpl2 +  NPI, df, sum)
V_harv_pupl_sum <- aggregate(Harvested_V_pulp_under_bark ~  scenSimpl2 +  NPI, df, sum)


# Merge data into one table
all.V <- V_mean %>% 
  left_join(V_st_log_mean) %>% 
  left_join(V_st_pulp_mean) %>% 
  left_join(V_harv_sum) %>% 
  left_join(V_harv_log_sum) %>% 
  left_join(V_harv_pupl_sum)


# Try to melt the data???
all.V_melt <- reshape2::melt(all.V, id.vars = c('NPI', 'scenSimpl2'))

# Add new variable: stand or harvested
all.V_melt <- all.V_melt %>% 
  mutate(type = case_when(
    stringr::str_detect(variable, "Harvested_V") ~ "harvested",
   TRUE ~ "stand"))



# Create two plots, merge later into one
p.stand <- 
  all.V_melt %>% 
  filter(type == "stand" & variable != "V") %>% 
  ggplot(aes(x = NPI,
                       y = value,
                       group = scenSimpl2,
                       color = scenSimpl2)) +
  geom_line(size = 1) + 
  xlab("NPI k € by ha") +
  ylab("mean stand volume") +
  facet_wrap(.~ variable) 



p.harvested <- 
  all.V_melt %>% 
  filter(type == "harvested" & variable != "Harvested_V") %>% 
  ggplot(aes(x = NPI,
             y = value/10000,
             group = scenSimpl2,
             color = scenSimpl2)) +
  geom_line(size = 1) + 
  xlab("NPI k € by ha") +
  ylab("sum harvested volume (*1000)") +
  facet_wrap(.~ variable) 



ggarrange(p.stand, p.harvested, ncol = 1, nrow = 2,
          common.legend = TRUE, legend="bottom")



# --------------------------------
# make plots individually to allow 
# control for the y lims - same for STAND and for harvested volume
# --------------------------------





# ------------------------------------------
# sample the data to speed up visualisation
# ------------------------------------------

# especially if many points are present

# Sample  random rows:
set.seed(1)
sample_row <- sample(1:nrow(df), 100000, replace=F)
df.sample <- df[sample_row,]


# -----------------------------------
# Plot wind risk over NPI gradient
# ----------------------------------

# get sum wind risk oevr scenarios
wind.sum <- aggregate(windRisk ~ scenSimpl2 + scenNumb + NPI, df, sum)


# Calculate multiple statistics by groups:
# min, mean, max
wind.summary <-
  df %>% 
  group_by(scenSimpl2, scenNumb, NPI) %>% 
  summarise(
    min = min(windRisk),
    mean = mean(windRisk),
    max = max(windRisk),
    sd = sd(windRisk))


library(ggplot2)
ggplot(wind.summary,
       aes(x = NPI/tot.area*10,
           y = mean,
           group = scenSimpl2,
           color = scenSimpl2)) +
  geom_ribbon(aes(x = NPI/tot.area*10,
                 ymax = mean + sd,
                 ymin = mean - sd),
             alpha = 0.3,
             fill = "grey90",
             color = "grey90") +
  geom_line(lwd = 1) +# color = scenSimpl2,
  facet_grid(.~ scenSimpl2) +
  xlab("NPI k € by ha") + #  
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5))
# include the min and max as shaded regions:
# https://stackoverflow.com/questions/25244241/line-plot-with-average-and-shadow-for-min-max 











# -------------------------
# Rescale the Volume (V) at risk
# -------------------------

# define teh variables to rescale

maxRisk = 4  # the max % of the windRisk - Suvanto
ratio_V = 2  # what is the proportion of timber fallwen? let say 1/2 


df <-
  df %>%
  mutate(rescal_V = V/ratio_V * windRisk/maxRisk)

max(df$V)

df %>% 
  filter(V > 842) # stand in 2111, 12976980

# Count number of stands with timber volume over 600 m3/ha - but this likely
# recalculated on area:
# 271 stands has it
df %>% 
  filter(V > 600) %>% 
  #distinct(avohaakut) %>% 
  count()


# --------------------
# Plot: does the % of SA increases winf risk?
# ---------------------
df %>% 
  group_by(scenNumb, scenSimpl2) %>% 
  summarize(rescal_V.mean = mean(rescal_V)) %>% 
  ggplot(aes(x = factor(scenNumb),
             y = rescal_V.mean,
             color = scenSimpl2,
             group = scenSimpl2)) + # ,
  geom_line() +
  xlab("Income") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") +
  ggtitle("Does % of SA increases V at risk?") 




# -------------------------------
# Get the harvested volume over SA gradient:
# -------------------------------

# List Volumes to explore:

vol_cols <- c("V", 
              "V_stand_log",
              "V_stand_pulp",
              "Harvested_V",
              "Harvested_V_log_under_bark",
              "Harvested_V_pulp_under_bark")



df %>% 
  group_by(scenNumb, scenSimpl2, NPI) %>% 
  summarize(V.mean = sum(Harvested_V, na.rm = T)) %>% 
  ggplot(aes(x = NPI,
             y = V.mean,
             color = scenSimpl2,
             group = scenSimpl2)) + # ,
  geom_line(lwd = 1)# +
  xlab("Income") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") +
  ggtitle("Harvested_V_sum") + 
  ylim(0,150)



# MAke function to plot different y values:
# --------------------------------  
  # Dion't know how to do it
  # 
  
set.seed(5)
df <- data.frame(x = rep(c(1:5), 2),
                 y1 = rnorm(10)*3+2,
                 y2 = rnorm(10),
                 group = rep(c("a", "b"), each = 5))  


p.y2<- ggplot(df, aes(x = x,
              y = y2,
              group = group,
              color = group)) +
  geom_line()
p.y1<- ggplot(df, aes(x = x,
                      y = y1,
                      group = group,
                      color = group)) +
  geom_line()

grid.arrange(p.y1, p.y2)


# vector of my Y
my.s<-c("y1", "y2")

windows(par(mfrow = c(2,1)))
# Loop over list of y to create different plots
outPlots<- list()
for (i in my.s) {
  print(i)
  my.plot <- 
    ggplot(df, aes_string(x = "x",
                          y = i,
                          group = "group",
                          color = "group")) +
    geom_line()
 # print(plot)
  outPlots <- append(outPlots, my.plot)
}

class(outPlots) 



# Stack overflow
library(reshape2)
df.melt <- melt(df, id.vars = c('x', 'group'))


ggplot(df.melt,
       aes(x=x,
           y=value,
           group=group,
           color=group))+
  geom_line()+
  facet_wrap(~variable,ncol = 1,scales = 'free')+theme_bw()




# Other example: using plto function
# ------------------------------
  

# ---------------------
# Timber volume
# ---------------------

V.p <- df %>% 
  group_by(scenNumb, scenSimpl2) %>% 
  summarize(V.mean = mean(V)) %>% 
  ggplot(aes(x = factor(scenNumb),
             y = V.mean,
             color = scenSimpl2,
             group = scenSimpl2)) + # ,
  geom_line(lwd = 1) +
  xlab("Income") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") +
  ggtitle("V total") + 
  ylim(0,350)


# ---------------------
# V.pulp
# -------------------
pulp.p <- df %>% 
  group_by(scenNumb, scenSimpl2) %>% 
  summarize(V.pulp.mean = mean(V_stand_pulp)) %>% 
  ggplot(aes(x = factor(scenNumb),
             y = V.pulp.mean,
             color = scenSimpl2,
             group = scenSimpl2)) + # ,
  geom_line(lwd = 1) +
  xlab("Income") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") +
  ggtitle("V pulp") + 
  ylim(0,200)


# ---------------------
# V.log
# -------------------

log.p <-
  df %>% 
  group_by(scenNumb, scenSimpl2) %>% 
  summarize(V.log.mean = mean(V_stand_log)) %>% 
  ggplot(aes(x = factor(scenNumb),
             y = V.log.mean,
             color = scenSimpl2,
             group = scenSimpl2)) + # ,
  geom_line(lwd = 1) +
  xlab("Income") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") +
  ggtitle("Log V") + 
  ylim(0,200)


#V.harv.log.p <-
# have NA values!!! replace by 0?? 
df %>% 
  group_by(scenNumb, scenSimpl2, NPI) %>% 
  summarize(V.harv.log.mean = mean(Harvested_V_log_under_bark, na.rm = T))# %>% 
  #ungroup() +
  ggplot(aes(x = NPI,
             y = V.harv.log.mean,
             color = scenSimpl2,
             group = scenSimpl2)) + # ,
  geom_line(lwd = 1) +
  xlab("Income") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") +
  ggtitle("Log V") + 
  ylim(0,200)

  
  # finnalize the harvested volume!! why I have NA values??


#
windows()
grid.arrange(#V.p,
  pulp.p,
  log.p,
  nrow = 1, ncol = 2)























# classify regimes in 4 groups:
# SA, CCF, RF_T, RF_WoT
# Get mean values for stands and years
# Calculate teh difference of teh wind risk from teh SA state
# plot differences over years, groupped by regimes
# Make spider plot



# -----------------------------------
# Species composition change: 2016 to 2011
# ----------------------------------
# Get histograms by tree species, age and BA in 2016:
df.2016 <- df.all %>% 
  filter(year == 2016)



# Check by landscapes:
window(8,4)
df.all %>% 
  filter(year == 2016) %>% 
  ggplot(aes(species,
             fill = species)) +
  geom_histogram(stat = "count") +
  facet_grid(scenSimpl2 ~ scenNumb) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ggtitle("Species composition: 2016")

# Did management changed stand composition at the end??
windows(8, 4)
df.all %>% 
  filter(year == 2111) %>%
  ggplot( aes(species,
              fill = species)) +
  geom_histogram(stat = "count") +
  facet_grid(scenSimpl2 ~ scenNumb) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ggtitle("Species composition: 2111")

  


# Get sum of area by species
# In 2016
windows(8, 4)
df.all %>% 
  group_by(scenSimpl2, scenNumb, species, year) %>% 
  summarise(sumAreaS = sum(area)) %>% 
  filter(year == 2016)  %>% 
  ggplot(aes(x = species,
             y = sumAreaS,
             fill = species)) +
  geom_col() +
  facet_grid(scenSimpl2 ~ scenNumb) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ggtitle("Species composition: 2016 (area)")


# In 2111
windows(8, 4)
df.all %>% 
  group_by(scenSimpl2, scenNumb, species, year) %>% 
  summarise(sumAreaS = sum(area)) %>% 
  filter(year == 2111)  %>% 
  ggplot(aes(x = species,
             y = sumAreaS,
             fill = species)) +
  geom_col() +
  facet_grid(scenSimpl2 ~ scenNumb) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ggtitle("Species composition: 2111 (area)")
  



# Check counts of open_edge???
windows(8, 4)
df.all %>% 
  group_by(scenSimpl2, scenNumb, open_edge, year) %>% 
  summarise(sumAreaS = sum(area)) %>% 
  filter(year == 2016)  %>% 
  ggplot(aes(x = open_edge,
             y = sumAreaS,
             fill = open_edge)) +
  geom_col() +
  facet_grid(scenSimpl2 ~ scenNumb) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ggtitle("Open edge: 2016 (area)")


windows(8, 4)
df.all %>% 
  group_by(scenSimpl2, scenNumb, open_edge, year) %>% 
  summarise(sumAreaS = sum(area)) %>% 
  filter(year == 2016 & open_edge == FALSE)  %>% 
  ggplot(aes(x = open_edge,
             y = sumAreaS,
             fill = open_edge)) +
  geom_col() +
  facet_grid(scenSimpl2 ~ scenNumb) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ggtitle("Open edge: 2091 (area)")




# -----------------------------
# Landscape configuration: 
# -----------------------------
# Measure the variability between stand height???
# how to measure contrast???


# How often h_dom 0 occurs??? difference between CF, RF, ALL?
min(df.all$H_dom) # 0.001


mu = aggregate(H_dom ~ scenSimpl2, df.all, mean)
  
df.all %>% 
  ggplot(aes(H_dom,
             group = scenSimpl2,
             fill = scenSimpl2,
             color = scenSimpl2)) + 
  geom_density(alpha = 0.1) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") 
 # geom_vline(mu, 
  #           aes(x=H_dom, 
  #               color=scenSimpl2),
  #           linetype="dashed")
 # facet_grid(scenSimpl2 ~ scenNumb)



df.all %>% 
  filter(H_dom == min(df.all$H_dom)) %>% 
  group_by(scenSimpl2, scenNumb, year) %>% 
  #filter(open_edge == FALSE) %>% 
  tally() %>% 
  group_by(scenSimpl2, scenNumb) %>% 
  summarize(mean.m = mean(n)) %>% 
  ggplot(aes(x = factor(scenNumb),
             y = mean.m,
             color = scenSimpl2,
             group = scenSimpl2)) + # ,
  geom_line(lwd = 1) +
  xlab("Income") + 
  ylab("# of stand with 0 height)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") +
  ggtitle("")# + 
#ylim(0,350)








# Plot open_edge FALSE = (closed stand) over SA %
# ---------------------
df.all %>% 
  group_by(scenSimpl2, scenNumb, open_edge, year) %>% 
  filter(open_edge == TRUE) %>% 
  tally() %>% 
  group_by(scenSimpl2, scenNumb) %>% 
  summarize(mean.m = mean(n)) %>% 
  ggplot(aes(x = factor(scenNumb),
             y = mean.m,
             color = scenSimpl2,
             group = scenSimpl2)) + # ,
  geom_line(lwd = 1) +
  xlab("Income") + 
  ylab("# of stand with open edge (mean)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") +
  ggtitle("")# + 
  #ylim(0,350)

















#------------------------------
#     Make boxplots:
# --------------------------

# plot different levels of wind risk: based on 3 models:
# How to make a plot with 3 y axis data??? for 3 wind risk:
# convert to long format???


boxplot(df.sample)




# How does scenarios (63) differ in term of wind risk???
# df.all

# Over the years:
ggplot(df.sample, 
       aes(x = as.factor(year),
           y = windRisk)) +
  geom_boxplot() + 
  facet_grid(. ~ simpleScen) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# By three main scenarios:
ggplot(df.sample, 
       aes(x = as.factor(simpleScen),
           y = windRisk)) +
  geom_boxplot(fill = "grey92") + 
  #facet_grid(. ~ ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# BY 63 scenarios:
ggplot(df.sample, 
       aes(x = year,
           y = windRisk,
           group = year)) +
  geom_boxplot(fill = "grey92") + 
  facet_grid(scenSimpl2 ~  scenNumb) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))








# --------------------------------
# Does the interannual differences in Volume affect wind risk on stand level??
# ordered differences in wind risk by landscape - total sum - over time
# ordered differences in Volume by landscape - total sum - over time
# plot by the % of SA and main 3 scenarios:
# ----------------------------
# Maybe to be continued ... ????





range(df.all$V)

hist(df.all$V)


# What is the amount of timber volume at risk???





# BOxplot

df.sample %>% 
  #group_by(scenNumb, scenSimpl2) %>% 
  #summarize(risk.mean = mean(windRisk)) %>% 
  ggplot(aes(x = factor(scenNumb),
             y = windRisk,
             fill = factor(scenSimpl2))) + # ,
  #group = factor(scenSimpl2)
  geom_boxplot(outlier.size=0.5, 
               outlier.alpha = 0.3) +  # aes(fill = factor(scenSimpl2))
  #facet_grid(scenSimpl2 ~  .) +
  xlab("Income") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom")





# How does the % of SA affect windRisk???
# Calculate teh wind risk by scenario oevr time and plot with the freq data
risk.mean <- aggregate(windRisk ~ scenNumb + scenSimpl2, df.all, mean)

ggplot(risk.mean, 
       aes(x = factor(scenNumb),
           y = windRisk,
           color = scenSimpl2,
           group = scenSimpl2)) + # ,
  #group = factor(scenSimpl2)
  geom_line(size = 1.5) +
  #facet_grid(scenSimpl2 ~  .) +
  xlab("Income") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom")




# -----------------------------------------
# How does the % of SA per scenario affect windrisk??
# !!! ??? how to get teh % of SA by scenario???
# Count the number of stands with SA regime
# --------------------------------------------
prop.regimes<-
  df.all %>% 
  group_by(scenario, avohaakut) %>% 
  distinct(id) %>% 
  summarise(stands_n = n()) %>%
#  filter(avohaakut == "SA") %>% 
#  head() 
  mutate(freq = 100* (stands_n / sum(stands_n))) %>% 
  arrange(scenario) 


  
# Get teh proportion of SA by the each scenario??
sa.share <- 
  df.all %>% 
  group_by(scenSimpl2,scenNumb, avohaakut) %>% 
  distinct(id) %>% 
  summarise(stands_n = n()) %>%
  filter(avohaakut == "SA") %>% 
  mutate(SA_share = 100* (stands_n / 1470)) #%>%
 # print(n = 63)
 # arrange(scenario) 

ggplot(sa.share, aes(x = scenNumb,
                     y = SA_share,
                     group = scenSimpl2,
                     color = scenSimpl2)) +
  geom_line()


write.csv(sa.share, "output/SA_share.csv")

# Calculate how many different regimes are by each scenario:
regime.n <-
  df.all %>% 
  group_by(scenario) %>% 
  distinct(avohaakut) %>% 
  summarise(regimes_n = n()) %>%
  #mutate(freq = 100* (stands_n / sum(stands_n))) %>% 
  arrange(scenario) 




# Get the % of the SA per scenario to add as new variable
# do all scenarios have SA??? YES
SA.perc <- 
  prop.regimes %>% 
  filter(avohaakut == "SA") %>% 
  arrange(freq) #%>% 
#print(n = 70)



# Does the all have 100% SA???? 
# YES

#     ALL0      SA             1472 100   
#  62 CCF0      SA             1472 100   
#  63 not_CCF0  SA             1472 100     




# Check if the regimes couls are correct:
# ALL11 should have 48 avohaakut regimes 
unique(subset(df.all, scenario == "ALL11")$avohaakut)


# How does the % of SA affect windRisk???
# Calculate teh wind risk by scenario oevr time and plot with the freq data
risk.mean <- aggregate(windRisk ~ scenario + year, df.all, mean)


# join the SA% data:

# Add the % of SA to each scenario
risk.mean <- 
  risk.mean %>% 
  left_join(SA.perc, by= "scenario") %>%
    
  left_join(regime.n, by = "scenario")

# clasify in 3 groups:
risk.mean <- risk.mean %>% 
  mutate(simpleScen = case_when(
    str_detect(scenario, "not_CCF") ~ "RF",
    str_detect(scenario, "ALL") ~ "ALL",
    str_detect(scenario, "CCF") ~ "CCF"))


# Plot data: wind risk by % of SA
ggplot(risk.mean, 
       aes(x = freq,  # % of stands with SA
           y = windRisk)) +
  geom_point(aes(color = factor(simpleScen))) + 
  geom_line(aes(color = factor(simpleScen))) + 
  xlab("SA by landscape (%)") +
  ylab("mean wind damage risk (%)") +
  labs(color = "scenario") +
  facet_grid(. ~ year) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Something is ahappening over last three study years:
# what is my tree height and tree age?



# ------------------------------------------
# Explore individual predictors:
# ---------------------------------------
# How does the H_dom changes over year???
# Increases, less variability in RF and with the lower % of the SA
ggplot(df.all, 
       aes(x = factor(year),  # % of stands with SA
           y = H_dom,
           group = factor(year))) +
  geom_boxplot() +
  facet_grid(scenSimpl2 ~  scenNumb) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(df.all, 
       aes(x = factor(scenNumb),
           y = H_dom,
           fill = factor(scenSimpl2))) + # ,
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom")



# What about BA?
# also increases over years
ggplot(df.all, 
       aes(x = year,  # % of stands with SA
           y = BA,
           group = year)) +
  geom_boxplot() +
  facet_wrap(. ~ scenario) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# BA
ggplot(df.all, 
       aes(x = factor(scenNumb),
           y = BA,
           fill = factor(scenSimpl2))) + # ,
  geom_boxplot() +  # aes(fill = factor(scenSimpl2))
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom")


# Volume
ggplot(df.all, 
       aes(x = factor(scenNumb),
           y = V,
           fill = factor(scenSimpl2))) + # ,
  geom_boxplot() +  # aes(fill = factor(scenSimpl2))
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom")



# Diameter: D_gm
ggplot(df.all, 
       aes(x = factor(scenNumb),
           y = D_gm,
           fill = factor(scenSimpl2))) + # ,
  geom_boxplot() +  # aes(fill = factor(scenSimpl2))
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom")




# ----------------------------------------------
# Specific questions:
# -----------------------------------------------

# Does particular management regime increase wind risk?

ggplot(df.sample, 
       aes(x = avohaakut ,  # % of stands with SA
           y = windRisk,
           group = avohaakut,
           fill = simpleScen)) +
  geom_boxplot() +
  facet_wrap(. ~ year) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# High variability in RF forestry, chack which one it is?

ggplot(subset(df.all, simpleScen == "RF" & avohaakut != "SA" ), 
       aes(x = avohaakut ,  # % of stands with SA
           y = windRisk,
           group = avohaakut)) +
  geom_boxplot() +
 facet_wrap(. ~ year) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(subset(df.all, simpleScen == "RF" & avohaakut != "SA" ), 
       aes(x = avohaakut ,  # % of stands with SA
           y = windRisk,
           group = avohaakut)) +
  geom_boxplot() +
  #facet_wrap(. ~ year) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# HOw does wind risk differs between SA/Non-SA regimes???
ggplot(df.sample, aes(y = windRisk,
                      x = simpleScen,
                      color = twoRegm)) +
  geom_boxplot() 

# evaluate over years:
# SA is very stable over time
ggplot(df.sample, aes(y = windRisk,
                      x = simpleScen,
                      color = twoRegm)) +
  geom_boxplot() +
  facet_grid(.~year)

# what is the trend over SA gradient? the difference in windRisk between SA and non-SA regimes:
risk.mean <- aggregate(windRisk ~ scenNumb + scenSimpl2 + twoRegm, df.all, mean)

ggplot(risk.mean, 
       aes(x = factor(scenNumb),
           y = windRisk,
           color = scenSimpl2,
           group = scenSimpl2)) + # ,
  #group = factor(scenSimpl2)
  geom_line(size = 1.5) +
  facet_grid(.~ twoRegm) +
  xlab("Income") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom")





# Very variable, bude definitely trends between CCF and RF
# What is the situation of teh SA
ggplot(subset(df.all, avohaakut == "SA"), 
       aes(x = year ,  # % of stands with SA
           y = windRisk,
           group = year)) +
  geom_boxplot() +
  facet_wrap(. ~ scenario) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




# Does combination of management and scenario increases wind risk?

# What terrain confirugation of SA increases wind risk???


# -----------------------------------
# How does the the alternative regimes over stand 
# affect stand level wind risk??
# -----------------------------------

# Calculate mean by stand and 4 alternatives by year, mean not sum beacsue I have more 
# CCFs then RF
df.mean <- aggregate(windRisk ~ id + year + avoh_Simpl, df.all, mean)

# Does every stand have 4 regimes?? NO!!!
table(df.mean$avoh_Simpl)

# CCF RF_noT   RF_T     SA 
# 27340  24780   9220  29400 


# Filter only values that have all 4 regimes:
df.mean %>% 
  arrange(id) %>% 
  head()


# Calulate differences among alternative states
# Calculate the differences 
df.mean.diff <- 
  df.mean %>%
  group_by(id, year) %>%
  mutate(windRisk.sa = windRisk[avoh_Simpl == 'SA']) %>% 
  mutate(diff = windRisk.sa - windRisk) %>% 
  arrange(id, year)


# Filter only stands that have all 4 regimes:
df.mean.diff <-
  df.mean.diff %>% 
  group_by(id, year) %>% 
  filter(all(c("CCF","RF_noT", "SA", "RF_T") %in% avoh_Simpl))



# PLot the differences:
pp1<- ggplot(subset(df.mean.diff, id == 6667292),
       aes(x = year,
           y = diff,
           color = avoh_Simpl)) +
  geom_line(size=1.2) + 
  ggtitle("differences from SA") +
  ylim(c(-0.025, 0.09)) + ylab("difference from SA") +
  theme()



# PLot the differences:
pp2<- ggplot(subset(df.mean.diff,  id == 6667292),
       aes(x = year,
           y = windRisk,
           color = avoh_Simpl)) +
  geom_line(size=1.2) +
  ylim(c(-0.025, 0.09)) +
  ggtitle("windrisk over time")




# H_dom
p.H <- df.all%>%
  filter(id == 6667292) %>% 
  group_by(id,year, avoh_Simpl)%>%
  summarise(H_dom_mean=mean(H_dom))%>%
  ggplot(aes(x = year,
             y = H_dom_mean))+
  geom_line(aes(color=avoh_Simpl),size=1.2)+
    ggtitle("mean H_dom")


# BA
p.BA <- df.all%>%
  filter(id == 6667292) %>% 
  group_by(id,year, avoh_Simpl)%>%
  summarise(BA_mean=mean(BA))%>%
  ggplot(aes(x = year,
             y = BA_mean))+
  geom_line(aes(color=avoh_Simpl),size=1.2)+
  ggtitle("mean BA")


# V
p.V <- df.all%>%
  filter(id == 6667292) %>% 
  group_by(id,year, avoh_Simpl)%>%
  summarise(V_mean=mean(V))%>%
  ggplot(aes(x = year,
             y = V_mean))+
  geom_line(aes(color=avoh_Simpl),size=1.2)+
  ggtitle("mean V")




# Risk
windows()
ggarrange(pp1, pp2,
          ncol=2, 
          nrow=1, common.legend = TRUE, legend="bottom")

# Characteristics
windows()
ggarrange(p.H, p.BA, p.V,
          ncol=3, 
          nrow=1, common.legend = TRUE, legend="bottom")



# ------------------------------------
# How does time since thinning affect wind risk??
# ------------------------------------

# Column 'difference' shows "time since thinngs"

df.all %>% 
  #filter(id == 6667292) %>% 
  distinct(difference)


# CHeck single stand example: LRT5 - rotation with thinnings, id 6667292
# does this contains duplicated rows???
df.all %>% 
  filter(id == 6667292 & avohaakut == "LRT30") %>% 
  dplyr::select(id, year, THIN, H_dom, BA, THIN_filled_lagged, difference, scenSimpl2 )


# what is the range of thinnings in CCF? "CCF_3_45"
df.all %>% 
  filter(id == 6667292 ) %>%
  distinct(avohaakut)
  

#   avohaakut
#1        SA
#2  THwoTM20
#3     LRH30
#4     LRT30
#5  CCF_4_45
#6  CCF_3_45


df.all %>% 
  filter(id == 6667292 & avohaakut == "CCF_3_45") %>% 
  dplyr::select(id, year, THIN, H_dom, BA, THIN_filled_lagged, difference, scenSimpl2 ) #%>%
  #distinct(difference)



# Plot:
# does time since thinning predict wind risk??? 

windows()
df.sample %>% 
  ggplot(aes(x = difference,
             y = windRisk,
             color = avoh_Simpl)) +
 #geom_jitter(alpha = 0.3, size = 0.1)  +
 geom_smooth(method = "lm",
             formula = y~ log(x)) +
  xlab("Years since thinning")


# show all regimes???
df.all %>% 
  ggplot(aes(x = difference,
             y = windRisk,
             color = avohaakut)) +
  #geom_jitter(alpha = 0.3, size = 0.1)  +
  geom_smooth(method = "lm",
              formula = y~ log(x)) +
  xlab("Years since thinning")


table(df.all$difference)


# -----------------------------------
# Some of the CCF ahe THIN 0:
# -----------------------------------
# check what are the values??
unique(subset(df.all, simpleScen == "CCF")$time_thinning)
#[1] "0-5"

unique(subset(df.all, simpleScen == "CCF")$difference)


# Subset two regimes and recalculate teh THIN values:
df.s <- df.all %>% 
  filter(id == 6667292 & (avohaakut == "CCF_3_45" | avohaakut == "LRT30")) %>% 
  dplyr::select(id, year, THIN, H_dom, BA, THIN_filled_lagged, difference, avohaakut) #%>%
#distinct(difference)

str(df.s)

unique(df.s$THIN)


df.s.d<- 
  df.s %>% 
  distinct()

 
# -------------------------
# stand 'fidelity' to individual regimes??
# --------------------------
# how many different regimes each stand has?


stand.fidelity <-
  df.all %>% 
  group_by(id) %>% 
  distinct(avohaakut)  %>% 
  summarise(regimes_n = n()) #%>%
  #mutate(freq = 100* (stands_n / sum(stands_n))) %>% 
  #arrange(scenario) 

hist(stand.fidelity$regimes_n)

# how many regimes each stand has under different regimes?
stand.fid.scen <-
  df.all %>% 
  group_by(id, simpleScen) %>% 
  distinct(avohaakut)  %>% 
  summarise(regimes_n = n()) #%>%

ggplot(stand.fid.scen, aes(regimes_n)) +
  geom_histogram() + 
  facet_grid(.~ simpleScen)



# Which are those regimes???
# are all the regimes used overall, or not?
df.regimes <- 
  df.all %>% 
    group_by(id, simpleScen) %>% 
    distinct(avohaakut)  #%>% 
  

# How many unique regimes?
length(unique(df.regimes$avohaakut))
# Yes, all of teh regimes were used


# How many of each?
# how oftern individual regimes occurs over scenarios???
# Dominance of SA, in ALL& RF THwoTH20 seems to prevail
# in CCF it is CCF 1, CCF2, CCF3, CCF4even not the extensions

ggplot(df.regimes, 
       aes(x = avohaakut, fill = simpleScen)) +
  geom_bar(stat = "count") +
  #facet_grid(.~ simpleScen) +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5))



# order the data by count
df.regimes.c <- 
  df.regimes %>% 
  group_by(avohaakut, simpleScen) %>% 
    summarise(counts = n()) %>% 
    arrange(desc(counts)) 

# Change the levels order manually (does not work with mutate):
df.regimes.c$avohaakut <- factor(df.regimes.c$avohaakut, 
                                 levels = unique(df.regimes.c$avohaakut))

# Plot the mos useful regimes
windows(width = 7, height = 3.5)

df.regimes.c %>% 
  ggplot(aes(x = avohaakut, 
             y = counts,
             fill = simpleScen,
             label = counts)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, 
                                   size = 7,
                                   face="italic")) 
  

# Show just ones with frequebcy over 100:
df.regimes.c %>% 
  filter(counts> 100) %>% 
  ggplot(aes(x = avohaakut, 
             y = counts,
             fill = simpleScen,
             label = counts)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5)) + 
  geom_text(aes(x = avohaakut, 
                y = counts,
                group = simpleScen),
            angle = 90,
            check_overlap = FALSE,
            size = 3,
            vjust = 0.5,
            hjust = 0,
            position = position_stack(vjust = 0.3))


# How does the % of SA affect which 
# How does the frequency of regimes changes over scenarios (63?)
# Get disctinct regimes by 
df.regimes.sa <- 
  df.all %>% 
  group_by(id, simpleScen, scenNumb) %>% 
  distinct(avohaakut)  #%>% 
 
# by %SA/landscape?



df.regimes

# How many where??
table(df.regimes.c$simpleScen, df.regimes.c$avohaakut)

length(unique(df.regimes$id))  # 1470


# ---------------------------------------------------
# What is the sum of wind risk and total NFI value for scenario?
# --------------------------------------------------




# Is MF the aggregate of the NFa, MFb, MFc, MFd?
head(df.npi)


# Plot multifunctionnality
ggplot(df.npi, aes(x = NPI/10000,
                   y = MF,
                   group = TypeSimple,
                   color = TypeSimple)) + 
  geom_line() +
  ylab("multifunctionnality") +
  xlab("NPI") +
  ylim(0,3) + 
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, hjust = 1))


# Chack if different trends between using sum or min values??

wind.sum <- aggregate(windRisk ~ scenario, df.all, sum)
wind.mean <- aggregate(windRisk ~ scenario, df.all, mean)

wind.sum <-
  wind.sum %>% 
  left_join(df.npi, by = c("scenario" = "Type")) %>% 
  mutate(simpleScen = case_when(
    str_detect(scenario, "not_CCF") ~ "RF",
    str_detect(scenario, "ALL") ~ "ALL",
    str_detect(scenario, "CCF") ~ "CCF"))
  

wind.mean <-
  wind.mean %>% 
  left_join(df.npi, by = c("scenario" = "Type")) %>% 
  mutate(simpleScen = case_when(
    str_detect(scenario, "not_CCF") ~ "RF",
    str_detect(scenario, "ALL") ~ "ALL",
    str_detect(scenario, "CCF") ~ "CCF"))


# Plot NPI values: smae patterns by sum over year and as mean
# -------------------------
ggplot(wind.sum, aes(x = NPI,
                     y = windRisk,
                     group = simpleScen,
                     color = simpleScen)) + 
  geom_line() +
  ylab("total sum wind risk") +
  xlab("NPI k € by ha")


windows()  
ggplot(wind.mean, aes(x = NPI/1000000,
                     y = windRisk,
                     group = simpleScen,
                     color = simpleScen)) + 
  geom_line(lwd = 1.5) +
  ylab("mean wind risk") +
  xlab("NPI k € by ha")




# Plot data : wind risk by % of regime diversity
#ggplot(risk.mean, 
 #      aes(x = regimes_n.y ,  # % of stands with SA
 #          y = windRisk)) +
#  geom_point(aes(color = factor(simpleScen))) + 
 # geom_line(aes(color = factor(simpleScen))) + 
 # xlab("#regimes by landscape (%)") +
 # ylab("mean wind damage risk (%)") +
 # labs(color = "scenario") +
  # facet_grid(. ~ ) +
 # theme(axis.text.x = element_text(angle = 90, hjust = 1))



# ----------------------------------------------------
# Compare the wind risk between individual stands under different regimes???
# ---------------------------------------------------------------

# How does the % of SA affect windRisk???
# Calculate teh wind risk by scenario oevr time and plot with the freq data
stand.risk.mean <- aggregate(windRisk ~ scenario + id, df.all, mean)


# join the SA% data:

# Add the % of SA to each scenario
stand.risk.mean <- stand.risk.mean %>% 
 left_join(SA.perc, by= "scenario")

# clasify in 3 groups:
stand.risk.mean <- stand.risk.mean %>% 
  mutate(simpleScen = case_when(
    str_detect(scenario, "not_CCF") ~ "RF",
    str_detect(scenario, "ALL") ~ "ALL",
    str_detect(scenario, "CCF") ~ "CCF"))



# Plot data
ggplot(stand.risk.mean, 
       aes(x = simpleScen ,  # % of stands with SA
           y = windRisk)) +
  #geom_point(aes(color = factor(simpleScen))) + 
  #geom_line(aes(color = factor(simpleScen))) + 
  geom_boxplot(fill = "grey92") +
  xlab("SA by landscape (%)") +
  ylab("mean wind damage risk (%)") +
  labs(color = "scenario") +
  # facet_grid(. ~ ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




# How does the diversification (higher number of regimes) affect windRisk?


# How 

# Temporal dynamics of wind risk of management
ggplot(df.sim, 
       aes(x = as.factor(year),
           y = windRisk)) +
  geom_boxplot() + 
  facet_grid(. ~ regime) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Is the wind risk the same for all regimes in the first simulation year??
p.windRisk <- ggplot(subset(df.sim, year == 2016), 
                     aes(x = regime,
                         y = windRisk)) +
  geom_boxplot() +
  ggtitle("Wind risk 2016") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p.H_dom <- ggplot(subset(df.sim, year == 2016), 
                  aes(x = regime,
                      y = H_dom)) +
  geom_boxplot() +
  ggtitle("H_dom 2016")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


p.BA <- ggplot(subset(df.sim, year == 2016), 
               aes(x = regime,
                   y = BA)) +
  geom_boxplot()  +
  ggtitle("BA 2016")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggarrange(p.windRisk, p.H_dom, p.BA, 
          labels = c("A", "B", "C"),
          ncol = 3, nrow = 1)

# Why initial stand conditions are not the same 
#  between management regimes???
# ------------------------------------------
# How many stands with open edge each regime has??

# Sum up the area& count of stands with open_edge = TRUE
head(df.sim)

# how many stands with open_edge == TRUE are in every year by manage regime?

open.edge.count<- 
  df %>% 
  group_by(year, regime) %>% 
  filter(open_edge == TRUE) %>% 
  tally() %>% 
  arrange(regime)


ggplot(open.edge.count, 
       aes(x = as.factor(year),
           y = n, 
           group = regime,
           color = regime)) +
  geom_line() + 
  # facet_grid(. ~ regime) +
  ggtitle("Count of stands with open edge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




# Investigate when the THIN happen???
df.thin <- subset(df, regime == "BAUwT_GTR")


# Track down history of one stand:
# when does thinning occured??

subset(df.thin, standid == 12469153,
       select = c("standid", "year", "THIN"))




# Investigate when the THIN happen???
df.cc2<- subset(df, regime == "CCF_2")


# Track down history of one stand:
# when does thinning occured??

subset(df.cc2, standid == 12469153,
       select = c("standid", "year", "THIN"))




# -------------------------------
#
# Check for differences in wind risk values 
# for individual stand between SA and no SA overall wind risk values??
#
# -------------------------------
head(df.all)

stand.risk.rgm.df <- aggregate(windRisk ~ id + avohaakut, df.all, mean)


ggplot(stand.risk.rgm.df, aes(x = avohaakut,
                              y = windRisk)) +
  geom_boxplot()



# How many stands have actually SA option included?
length(unique(subset(df.all, avohaakut == "SA")$id))
#1470 

length(unique(df.all$id))
# 1470

# are some stands consistently choosed as suitable for SA? seems that not 
# as SA was simulated for each stand?

# Compare teh min between SA and no-SA regime by scenario???

# Create 4 basic groups depending on regime:
# SA, RF with and without thinng and CCF (always thinning included)
df.all <-
  df.all %>% 
  mutate(avoh_Simpl = case_when(
    str_detect(avohaakut, "SA")   ~ "SA",
    str_detect(avohaakut, "CCF_") ~ "CCF",
    str_detect(avohaakut, "LRH")  ~ "RF_noT",
    str_detect(avohaakut, "LRT")  ~ "RF_T",
    str_detect(avohaakut, "SR5")  ~ "RF_noT",
    str_detect(avohaakut, "SRT5") ~ "RF_T",
    str_detect(avohaakut, "TH")   ~ "RF_noT",
    str_detect(avohaakut, "TT")   ~ "RF_T"))
         
# Check if correct
subset(df.all, avohaakut == "TT")


# ten join into single table
stand.sa.min <- aggregate(windRisk ~ id + avoh_Simpl, 
                       subset(df.all, avohaakut == "SA"), min) %>% 
  mutate(regime = "SA")

stand.no.sa.min <- aggregate(windRisk ~ id + avoh_Simpl, 
                          subset(df.all, avohaakut != "SA"), min) %>% 
  mutate(regime = "no SA")

# BInd data into one long table
merged.sa <- rbind(stand.sa.min,
                   stand.no.sa.min)

unique(merged.sa$regime)
unique(merged.sa$avoh_Simpl)

length(unique(subset(merged.sa, regime == "SA")$id))



# Calculate the paired differences between regimes for stand??
# does every stand has all 4 categories???
table(merged.sa$avoh_Simpl)

# NO, but all of them nhave SA 
# CCF RF_noTHIN   RF_THIN        SA 
# 1367      1239       461      1470 

# Calculate the differences 
merged.diff <- 
  merged.sa %>%
  group_by(id) %>%
  mutate(windRisk.sa = windRisk[avoh_Simpl == 'SA']) %>% 
  mutate(diff = windRisk.sa - windRisk) %>% 
    arrange(id)


# Plot differences by groups
ggplot(subset(merged.diff, avoh_Simpl != "SA"), 
       aes(diff)) +
  geom_histogram() + 
  facet_grid(.~avoh_Simpl)








# Create paired ggplot
ggplot(merged.sa, aes(x = avoh_Simpl,
                      y = windRisk,
                      group = id)) +
  geom_point(aes(colour=avoh_Simpl),
             size=2.5,
             alpha=0.01, 
             position=position_dodge(width=0.1)) +
  geom_line(size=1, 
            alpha=0.01, 
            position=position_dodge(width=0.1))




# how many sctand I have in all??
length(unique(stand.sa.min$id))
# 1470

length(unique(stand.no.sa.min$id))
# 1416

# Let's check it for one stand:
# 6667291

aggregate(windRisk ~  avohaakut, 
          subset(df.all, id == 6667291), min)



d<-aggregate(windRisk ~  avohaakut, 
          subset(df.all, id == 12538077), min)


ggplot(d, aes(x = avohaakut,
              y = windRisk)) +
  geom_boxplot()



# ------------------------
# Analyse the data & 
#     Make boxplots:
# --------------------------

# set theme for ggplot2
theme_set(theme_classic())

# How does scenarios (63) differ in term of wind risk???
ggplot(df.all, 
       aes(x = as.factor(year),
           y = windRisk)) +
  geom_boxplot() + 
  facet_grid(. ~ simpleScen) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



ggplot(df.all, 
       aes(x = as.factor(simpleScen),
           y = windRisk)) +
  geom_boxplot(fill = "grey92") + 
  #facet_grid(. ~ ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# ------------------------
# Categorical variables: 
# Wind risk by time _since thining
# ------------------------

p.edge <- ggplot(df.all, 
       aes(x = as.factor(open_edge),
           y = windRisk,
           fill = open_edge)) +
  geom_boxplot() +  # fill = "grey92"
  facet_grid(. ~ simpleScen) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Open_edge") +
  theme(legend.position = "none")


p.thin <- ggplot(df.all, 
                 aes(x = as.factor(since_thin),
                     y = windRisk,
                     fill = since_thin)) +
  geom_boxplot() +  # fill = "grey92"
  facet_grid(. ~ simpleScen) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Time since thinning")+
  theme(legend.position = "none")



p.soilDepth <-
  ggplot(df.all, 
                   aes(x = as.factor(soil_depth_less30),
                       y = windRisk,
                       fill =soil_depth_less30 )) +
  geom_boxplot() +  # fill = "grey92"
  facet_grid(. ~ simpleScen) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Soil depth<30cm") +
  theme(legend.position = "none")


p.soilType <-
  ggplot(df.all, 
         aes(x = as.factor(soilType),
             y = windRisk, 
             fill =soilType )) +
  geom_boxplot() + #
  facet_grid(. ~ simpleScen) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Soil type") +
  theme(legend.position = "none")


p.species <-
  ggplot(df.all, 
                 aes(x = as.factor(species),
                     y = windRisk,
                     fill = species)) +
  geom_boxplot() +  # fill = "grey92"
  facet_grid(. ~ simpleScen) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Species") +
  theme(legend.position = "none")

p.PEAT <-
  ggplot(df.all, 
         aes(x = as.factor(PEAT),
             y = windRisk,
             fill = PEAT)) +
  geom_boxplot() +  # fill = "grey92"
  facet_grid(. ~ simpleScen) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("PEAT") +
  theme(legend.position = "none")




# Put data togetehr into one plot
grid.arrange(p.edge,
             p.thin,
             p.soilDepth,
             p.soilType,
             p.species,
             p.PEAT,
             nrow = 2, ncol = 3)



# Calculate teh means, min, max of wind risk over landscape by time
# -----------------------------------------

risk.min<- aggregate(windRisk ~ scenario, df.all, min)
risk.max<- aggregate(windRisk ~ scenario, df.all, max)
risk.range <- cbind(risk.min, risk.max$windRisk)
names(risk.range) <- c("scenario", "min", "max")


risk.range <- risk.range %>% 
  mutate(simpleScen = case_when(
    stringr::str_detect(scenario, "not_CCF") ~ "RF",
    stringr::str_detect(scenario, "ALL") ~ "ALL",
    stringr::str_detect(scenario, "CCF") ~ "CCF"))


ggplot(risk.range, aes(scenario)) +
  #geom_linerange(x =  windRisk.1,
   #              y = windRisk.2) +
  geom_linerange(aes(ymin=min,
                     ymax=max),
                 linetype=2,
                 color="blue")+
  geom_point(aes(y=min),size=3,color="red")+
  geom_point(aes(y=max),size=3,color="red")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))



# Get means by scenarios, merge with NPI data
risk.mean <- aggregate(windRisk ~ scenario, df.all, mean)





# Get the % of the SA per scenario to add as new variable
# do all scenarios have SA??? YES
SA.perc <- 
  prop.regimes %>% 
  filter(avohaakut == "SA") %>% 
  arrange(freq) #%>% 
#print(n = 70)


# Does the all have 100% SA????
#     ALL0      SA             1472 100   
#  62 CCF0      SA             1472 100   
#  63 not_CCF0  SA             1472 100     



# How does the % of SA affect windRisk???
# Calculate teh wind risk by scenario oevr time and plot with the freq data
risk.mean <- aggregate(windRisk ~ scenario, df.all, mean)


# join the SA% data:

# Add the % of SA to each scenario
risk.mean <- risk.mean %>% 
  left_join(SA.perc, by= "scenario") %>% 
  left_join(regime.n, by = "scenario")

# clasify in 3 groups:
risk.mean <- risk.mean %>% 
  mutate(simpleScen = case_when(
    str_detect(scenario, "not_CCF") ~ "RF",
    str_detect(scenario, "ALL") ~ "ALL",
    str_detect(scenario, "CCF") ~ "CCF"))


# Plot data: wind risk by % of SA
ggplot(risk.mean, 
       aes(x = freq,  # % of stands with SA
           y = windRisk)) +
  geom_point(aes(color = factor(simpleScen))) + 
  geom_line(aes(color = factor(simpleScen))) + 
  xlab("SA by landscape (%)") +
  ylab("mean wind damage risk (%)") +
  labs(color = "scenario") +
  # facet_grid(. ~ ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Plot data : wind risk by % of regime diversity
ggplot(risk.mean, 
       aes(x = regimes_n.y ,  # % of stands with SA
           y = windRisk)) +
  geom_point(aes(color = factor(simpleScen))) + 
  geom_line(aes(color = factor(simpleScen))) + 
  xlab("#regimes by landscape (%)") +
  ylab("mean wind damage risk (%)") +
  labs(color = "scenario") +
  # facet_grid(. ~ ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Compare the wind risk between individual stands under different regimes???
# ---------------------------------------------------------------

# How does the % of SA affect windRisk???
# Calculate teh wind risk by scenario oevr time and plot with the freq data
stand.risk.mean <- aggregate(windRisk ~ scenario + id, df.all, mean)


# join the SA% data:

# Add the % of SA to each scenario
#risk.mean <- risk.mean %>% 
#  left_join(SA.perc, by= "scenario")

# clasify in 3 groups:
stand.risk.mean <- stand.risk.mean %>% 
  mutate(simpleScen = case_when(
    str_detect(scenario, "not_CCF") ~ "RF",
    str_detect(scenario, "ALL") ~ "ALL",
    str_detect(scenario, "CCF") ~ "CCF"))



# Plot data
ggplot(stand.risk.mean, 
       aes(x = simpleScen ,  # % of stands with SA
           y = windRisk)) +
  #geom_point(aes(color = factor(simpleScen))) + 
  #geom_line(aes(color = factor(simpleScen))) + 
  geom_boxplot(fill = "grey92") +
  xlab("SA by landscape (%)") +
  ylab("mean wind damage risk (%)") +
  labs(color = "scenario") +
  # facet_grid(. ~ ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




# How does the diversification (higher number of regimes) affect windRisk?


# How 

# Temporal dynamics of wind risk of management
ggplot(df.sim, 
       aes(x = as.factor(year),
           y = windRisk)) +
  geom_boxplot() + 
  facet_grid(. ~ regime) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Is the wind risk the same for all regimes in the first simulation year??
p.windRisk <- ggplot(subset(df.sim, year == 2016), 
                     aes(x = regime,
                         y = windRisk)) +
  geom_boxplot() +
  ggtitle("Wind risk 2016") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p.H_dom <- ggplot(subset(df.sim, year == 2016), 
                  aes(x = regime,
                      y = H_dom)) +
  geom_boxplot() +
  ggtitle("H_dom 2016")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


p.BA <- ggplot(subset(df.sim, year == 2016), 
               aes(x = regime,
                   y = BA)) +
  geom_boxplot()  +
  ggtitle("BA 2016")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggarrange(p.windRisk, p.H_dom, p.BA, 
          labels = c("A", "B", "C"),
          ncol = 3, nrow = 1)


=======


# 
rm(list = ls())

# , eval = FALSE
library(data.table)
library(dplyr)
library(raster)
library(ggplot2)
library(sf)
library(stringr)
library(gridExtra)
library(tidyr)
library(ggpubr)


theme_set(theme_classic())
theme_update(panel.grid.major = element_line(colour = "grey95",
                                             size = 0.1,
                                             linetype = 2))

# Read datasets:
# ----------------------------------------
#df.all <- fread("C:/MyTemp/myGitLab/windDamage/output/df_sim_windRisk.csv")
df <- fread("/projappl/project_2003256/windDamage/output/final_df_solution8.csv")


# Create two regimes: SA and non-SA"
df.all <- 
  df.all %>% 
  mutate(twoRegm = case_when(avohaakut == "SA" ~ "SA",
                             avohaakut != "SA" ~ "no_SA"))

#  -----------------------------------------
# read stand geometry data
# ------------------------------------------
# stands that are not simulated
stands.remove <-c(13243875, 
                  13243879, 
                  13243881,
                  6685176,     # # H_dom is >150 m 
                  13243960)    #  H_dom is 430



# Subset stands only for normal H_dom values
df.all <- subset(df.all, !id %in% stands.remove)


# stands geometry
df.geom <- st_read("C:/MyTemp/avohaakut_db/14.534/14.534/mvj_14.534.shp")
df.geom <- subset(df.geom, select = c("KUVIO_ID"))
names(df.geom) <- c("id", "geometry")
df.geom$area <- st_area(df.geom)
df.geom <- subset(df.geom, id %in% unique(df.all$id))

# Total area of watershed:
tot.area = as.numeric(sum(df.geom$area))


# Read NPI values:
# -------------------------
# get the NPVI&NPI values over scenarios 
df.npi <- fread("C:/MyTemp/avohaakut_db/NPI/MF_NPI.csv")

# reorganize teh data to correspond to simulated scenarios:
df.npi <- 
  df.npi %>% 
  rename(scenario = Type) %>%   # rename Type to scenario
  mutate(scenario = gsub("not_CCF", "RF", scenario)) %>% 
  tidyr::separate(scenario,   # Separate text from the number
            into = c("scenSimpl2", "scenNumb"), 
            sep = "(?<=[A-Za-z])(?=[0-9])") %>% 
  dplyr::select(-TypeSimple) %>% 
  mutate(scenNumb = as.numeric(scenNumb)) %>% 
  mutate(NPI = NPI/tot.area*10)   # change the values to corresponds Kyle's values



# -------------------------------------
# remove excessive columns from simulated data
# add NPI & MF values
# ------------------------------
df <-
  df.all %>% 
  # remove excessive columns
  dplyr::select(-c(AREA, V_total_deadwood, 
                DEVEL_CLASS, SC,
                SOIL_CLASS, MAIN_SP, 
                name, THIN2,
                scenario,
                THIN_filled_lagged,
                windSpeed, PEAT.v, tempSum)) %>% 
  left_join(df.npi, by = c("scenSimpl2", "scenNumb"))


# remove original table to save the memory
rm(df.all)



# Complete factors:
# ---------------------
# CHaracterize stands extends:
# ---------------------
length(unique(df.geom$area))
mean(df.geom$area)
max(df.geom$area)
min(df.geom$area)

hist(df.geom$area/10000)


# Calculate the % of SA and add to table:
df.SA_prop <-
  df %>% 
  group_by(scenSimpl2, scenNumb, avohaakut) %>% 
  distinct(id) %>% 
  summarise(stands_n = n()) %>%
  filter(avohaakut == "SA") %>% 
  mutate(SA_prop = 100* (stands_n / 1470)) %>%
  dplyr::select(-c(avohaakut))


# Add SA % (frequency) to the simulated data table
df <- 
  df %>% 
  left_join(df.SA_prop, by = c("scenSimpl2", "scenNumb"))

# export simplified table

fwrite(df, "output/final_df.csv")



# -----------------------------------------
# Visualize volume dynamics over scenarios
# -----------------------------------------
names(df)

vol_cols <- c("V", 
              "V_stand_log",
              "V_stand_pulp",
              "Harvested_V",
              "Harvested_V_log_under_bark",
              "Harvested_V_pulp_under_bark")


# Check which Volume characteristics contains NA values??
for (i in vol_cols){
  print(i)
  print(anyNA(df[[i]]))
}


# -------------------------------------------
# plot sum of different volumes over x
# -------------------------------------------
# For harvested V - calculate sum, as this is not measured every time,
# otherwise calculate mean or max???

V_mean          <- aggregate(V ~ scenSimpl2 +  NPI, df, mean)
V_st_log_mean   <- aggregate(V_stand_log ~ scenSimpl2 +  NPI, df, mean)
V_st_pulp_mean  <- aggregate(V_stand_pulp ~ scenSimpl2 +  NPI, df, mean)

V_harv_sum      <- aggregate(Harvested_V ~ scenSimpl2 +  NPI, df, sum)
V_harv_log_sum  <- aggregate(Harvested_V_log_under_bark ~  scenSimpl2 +  NPI, df, sum)
V_harv_pupl_sum <- aggregate(Harvested_V_pulp_under_bark ~  scenSimpl2 +  NPI, df, sum)


# Merge data into one table
all.V <- V_mean %>% 
  left_join(V_st_log_mean) %>% 
  left_join(V_st_pulp_mean) %>% 
  left_join(V_harv_sum) %>% 
  left_join(V_harv_log_sum) %>% 
  left_join(V_harv_pupl_sum)


# Try to melt the data???
all.V_melt <- reshape2::melt(all.V, id.vars = c('NPI', 'scenSimpl2'))

# Add new variable: stand or harvested
all.V_melt <- all.V_melt %>% 
  mutate(type = case_when(
    stringr::str_detect(variable, "Harvested_V") ~ "harvested",
   TRUE ~ "stand"))



# Create two plots, merge later into one
p.stand <- 
  all.V_melt %>% 
  filter(type == "stand" & variable != "V") %>% 
  ggplot(aes(x = NPI,
                       y = value,
                       group = scenSimpl2,
                       color = scenSimpl2)) +
  geom_line(size = 1) + 
  xlab("NPI k € by ha") +
  ylab("mean stand volume") +
  facet_wrap(.~ variable) 



p.harvested <- 
  all.V_melt %>% 
  filter(type == "harvested" & variable != "Harvested_V") %>% 
  ggplot(aes(x = NPI,
             y = value/10000,
             group = scenSimpl2,
             color = scenSimpl2)) +
  geom_line(size = 1) + 
  xlab("NPI k € by ha") +
  ylab("sum harvested volume (*1000)") +
  facet_wrap(.~ variable) 



ggarrange(p.stand, p.harvested, ncol = 1, nrow = 2,
          common.legend = TRUE, legend="bottom")



# --------------------------------
# make plots individually to allow 
# control for the y lims - same for STAND and for harvested volume
# --------------------------------





# ------------------------------------------
# sample the data to speed up visualisation
# ------------------------------------------

# especially if many points are present

# Sample  random rows:
set.seed(1)
sample_row <- sample(1:nrow(df), 100000, replace=F)
df.sample <- df[sample_row,]


# -----------------------------------
# Plot wind risk over NPI gradient
# ----------------------------------

# get sum wind risk oevr scenarios
wind.sum <- aggregate(windRisk ~ scenSimpl2 + scenNumb + NPI, df, sum)


# Calculate multiple statistics by groups:
# min, mean, max
wind.summary <-
  df %>% 
  group_by(scenSimpl2, scenNumb, NPI) %>% 
  summarise(
    min = min(windRisk),
    mean = mean(windRisk),
    max = max(windRisk),
    sd = sd(windRisk))


library(ggplot2)
ggplot(wind.summary,
       aes(x = NPI/tot.area*10,
           y = mean,
           group = scenSimpl2,
           color = scenSimpl2)) +
  geom_ribbon(aes(x = NPI/tot.area*10,
                 ymax = mean + sd,
                 ymin = mean - sd),
             alpha = 0.3,
             fill = "grey90",
             color = "grey90") +
  geom_line(lwd = 1) +# color = scenSimpl2,
  facet_grid(.~ scenSimpl2) +
  xlab("NPI k € by ha") + #  
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5))
# include the min and max as shaded regions:
# https://stackoverflow.com/questions/25244241/line-plot-with-average-and-shadow-for-min-max 











# -------------------------
# Rescale the Volume (V) at risk
# -------------------------

# define teh variables to rescale

maxRisk = 4  # the max % of the windRisk - Suvanto
ratio_V = 2  # what is the proportion of timber fallwen? let say 1/2 


df <-
  df %>%
  mutate(rescal_V = V/ratio_V * windRisk/maxRisk)

max(df$V)

df %>% 
  filter(V > 842) # stand in 2111, 12976980

# Count number of stands with timber volume over 600 m3/ha - but this likely
# recalculated on area:
# 271 stands has it
df %>% 
  filter(V > 600) %>% 
  #distinct(avohaakut) %>% 
  count()


# --------------------
# Plot: does the % of SA increases winf risk?
# ---------------------
df %>% 
  group_by(scenNumb, scenSimpl2) %>% 
  summarize(rescal_V.mean = mean(rescal_V)) %>% 
  ggplot(aes(x = factor(scenNumb),
             y = rescal_V.mean,
             color = scenSimpl2,
             group = scenSimpl2)) + # ,
  geom_line() +
  xlab("Income") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") +
  ggtitle("Does % of SA increases V at risk?") 




# -------------------------------
# Get the harvested volume over SA gradient:
# -------------------------------

# List Volumes to explore:

vol_cols <- c("V", 
              "V_stand_log",
              "V_stand_pulp",
              "Harvested_V",
              "Harvested_V_log_under_bark",
              "Harvested_V_pulp_under_bark")



df %>% 
  group_by(scenNumb, scenSimpl2, NPI) %>% 
  summarize(V.mean = sum(Harvested_V, na.rm = T)) %>% 
  ggplot(aes(x = NPI,
             y = V.mean,
             color = scenSimpl2,
             group = scenSimpl2)) + # ,
  geom_line(lwd = 1)# +
  xlab("Income") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") +
  ggtitle("Harvested_V_sum") + 
  ylim(0,150)



# MAke function to plot different y values:
# --------------------------------  
  # Dion't know how to do it
  # 
  
set.seed(5)
df <- data.frame(x = rep(c(1:5), 2),
                 y1 = rnorm(10)*3+2,
                 y2 = rnorm(10),
                 group = rep(c("a", "b"), each = 5))  


p.y2<- ggplot(df, aes(x = x,
              y = y2,
              group = group,
              color = group)) +
  geom_line()
p.y1<- ggplot(df, aes(x = x,
                      y = y1,
                      group = group,
                      color = group)) +
  geom_line()

grid.arrange(p.y1, p.y2)


# vector of my Y
my.s<-c("y1", "y2")

windows(par(mfrow = c(2,1)))
# Loop over list of y to create different plots
outPlots<- list()
for (i in my.s) {
  print(i)
  my.plot <- 
    ggplot(df, aes_string(x = "x",
                          y = i,
                          group = "group",
                          color = "group")) +
    geom_line()
 # print(plot)
  outPlots <- append(outPlots, my.plot)
}

class(outPlots) 



# Stack overflow
library(reshape2)
df.melt <- melt(df, id.vars = c('x', 'group'))


ggplot(df.melt,
       aes(x=x,
           y=value,
           group=group,
           color=group))+
  geom_line()+
  facet_wrap(~variable,ncol = 1,scales = 'free')+theme_bw()




# Other example: using plto function
# ------------------------------
  

# ---------------------
# Timber volume
# ---------------------

V.p <- df %>% 
  group_by(scenNumb, scenSimpl2) %>% 
  summarize(V.mean = mean(V)) %>% 
  ggplot(aes(x = factor(scenNumb),
             y = V.mean,
             color = scenSimpl2,
             group = scenSimpl2)) + # ,
  geom_line(lwd = 1) +
  xlab("Income") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") +
  ggtitle("V total") + 
  ylim(0,350)


# ---------------------
# V.pulp
# -------------------
pulp.p <- df %>% 
  group_by(scenNumb, scenSimpl2) %>% 
  summarize(V.pulp.mean = mean(V_stand_pulp)) %>% 
  ggplot(aes(x = factor(scenNumb),
             y = V.pulp.mean,
             color = scenSimpl2,
             group = scenSimpl2)) + # ,
  geom_line(lwd = 1) +
  xlab("Income") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") +
  ggtitle("V pulp") + 
  ylim(0,200)


# ---------------------
# V.log
# -------------------

log.p <-
  df %>% 
  group_by(scenNumb, scenSimpl2) %>% 
  summarize(V.log.mean = mean(V_stand_log)) %>% 
  ggplot(aes(x = factor(scenNumb),
             y = V.log.mean,
             color = scenSimpl2,
             group = scenSimpl2)) + # ,
  geom_line(lwd = 1) +
  xlab("Income") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") +
  ggtitle("Log V") + 
  ylim(0,200)


#V.harv.log.p <-
# have NA values!!! replace by 0?? 
df %>% 
  group_by(scenNumb, scenSimpl2, NPI) %>% 
  summarize(V.harv.log.mean = mean(Harvested_V_log_under_bark, na.rm = T))# %>% 
  #ungroup() +
  ggplot(aes(x = NPI,
             y = V.harv.log.mean,
             color = scenSimpl2,
             group = scenSimpl2)) + # ,
  geom_line(lwd = 1) +
  xlab("Income") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") +
  ggtitle("Log V") + 
  ylim(0,200)

  
  # finnalize the harvested volume!! why I have NA values??


#
windows()
grid.arrange(#V.p,
  pulp.p,
  log.p,
  nrow = 1, ncol = 2)























# classify regimes in 4 groups:
# SA, CCF, RF_T, RF_WoT
# Get mean values for stands and years
# Calculate teh difference of teh wind risk from teh SA state
# plot differences over years, groupped by regimes
# Make spider plot



# -----------------------------------
# Species composition change: 2016 to 2011
# ----------------------------------
# Get histograms by tree species, age and BA in 2016:
df.2016 <- df.all %>% 
  filter(year == 2016)



# Check by landscapes:
window(8,4)
df.all %>% 
  filter(year == 2016) %>% 
  ggplot(aes(species,
             fill = species)) +
  geom_histogram(stat = "count") +
  facet_grid(scenSimpl2 ~ scenNumb) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ggtitle("Species composition: 2016")

# Did management changed stand composition at the end??
windows(8, 4)
df.all %>% 
  filter(year == 2111) %>%
  ggplot( aes(species,
              fill = species)) +
  geom_histogram(stat = "count") +
  facet_grid(scenSimpl2 ~ scenNumb) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ggtitle("Species composition: 2111")

  


# Get sum of area by species
# In 2016
windows(8, 4)
df.all %>% 
  group_by(scenSimpl2, scenNumb, species, year) %>% 
  summarise(sumAreaS = sum(area)) %>% 
  filter(year == 2016)  %>% 
  ggplot(aes(x = species,
             y = sumAreaS,
             fill = species)) +
  geom_col() +
  facet_grid(scenSimpl2 ~ scenNumb) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ggtitle("Species composition: 2016 (area)")


# In 2111
windows(8, 4)
df.all %>% 
  group_by(scenSimpl2, scenNumb, species, year) %>% 
  summarise(sumAreaS = sum(area)) %>% 
  filter(year == 2111)  %>% 
  ggplot(aes(x = species,
             y = sumAreaS,
             fill = species)) +
  geom_col() +
  facet_grid(scenSimpl2 ~ scenNumb) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ggtitle("Species composition: 2111 (area)")
  



# Check counts of open_edge???
windows(8, 4)
df.all %>% 
  group_by(scenSimpl2, scenNumb, open_edge, year) %>% 
  summarise(sumAreaS = sum(area)) %>% 
  filter(year == 2016)  %>% 
  ggplot(aes(x = open_edge,
             y = sumAreaS,
             fill = open_edge)) +
  geom_col() +
  facet_grid(scenSimpl2 ~ scenNumb) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ggtitle("Open edge: 2016 (area)")


windows(8, 4)
df.all %>% 
  group_by(scenSimpl2, scenNumb, open_edge, year) %>% 
  summarise(sumAreaS = sum(area)) %>% 
  filter(year == 2016 & open_edge == FALSE)  %>% 
  ggplot(aes(x = open_edge,
             y = sumAreaS,
             fill = open_edge)) +
  geom_col() +
  facet_grid(scenSimpl2 ~ scenNumb) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ggtitle("Open edge: 2091 (area)")




# -----------------------------
# Landscape configuration: 
# -----------------------------
# Measure the variability between stand height???
# how to measure contrast???


# How often h_dom 0 occurs??? difference between CF, RF, ALL?
min(df.all$H_dom) # 0.001


mu = aggregate(H_dom ~ scenSimpl2, df.all, mean)
  
df.all %>% 
  ggplot(aes(H_dom,
             group = scenSimpl2,
             fill = scenSimpl2,
             color = scenSimpl2)) + 
  geom_density(alpha = 0.1) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") 
 # geom_vline(mu, 
  #           aes(x=H_dom, 
  #               color=scenSimpl2),
  #           linetype="dashed")
 # facet_grid(scenSimpl2 ~ scenNumb)



df.all %>% 
  filter(H_dom == min(df.all$H_dom)) %>% 
  group_by(scenSimpl2, scenNumb, year) %>% 
  #filter(open_edge == FALSE) %>% 
  tally() %>% 
  group_by(scenSimpl2, scenNumb) %>% 
  summarize(mean.m = mean(n)) %>% 
  ggplot(aes(x = factor(scenNumb),
             y = mean.m,
             color = scenSimpl2,
             group = scenSimpl2)) + # ,
  geom_line(lwd = 1) +
  xlab("Income") + 
  ylab("# of stand with 0 height)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") +
  ggtitle("")# + 
#ylim(0,350)








# Plot open_edge FALSE = (closed stand) over SA %
# ---------------------
df.all %>% 
  group_by(scenSimpl2, scenNumb, open_edge, year) %>% 
  filter(open_edge == TRUE) %>% 
  tally() %>% 
  group_by(scenSimpl2, scenNumb) %>% 
  summarize(mean.m = mean(n)) %>% 
  ggplot(aes(x = factor(scenNumb),
             y = mean.m,
             color = scenSimpl2,
             group = scenSimpl2)) + # ,
  geom_line(lwd = 1) +
  xlab("Income") + 
  ylab("# of stand with open edge (mean)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") +
  ggtitle("")# + 
  #ylim(0,350)

















#------------------------------
#     Make boxplots:
# --------------------------

# plot different levels of wind risk: based on 3 models:
# How to make a plot with 3 y axis data??? for 3 wind risk:
# convert to long format???


boxplot(df.sample)




# How does scenarios (63) differ in term of wind risk???
# df.all

# Over the years:
ggplot(df.sample, 
       aes(x = as.factor(year),
           y = windRisk)) +
  geom_boxplot() + 
  facet_grid(. ~ simpleScen) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# By three main scenarios:
ggplot(df.sample, 
       aes(x = as.factor(simpleScen),
           y = windRisk)) +
  geom_boxplot(fill = "grey92") + 
  #facet_grid(. ~ ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# BY 63 scenarios:
ggplot(df.sample, 
       aes(x = year,
           y = windRisk,
           group = year)) +
  geom_boxplot(fill = "grey92") + 
  facet_grid(scenSimpl2 ~  scenNumb) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))








# --------------------------------
# Does the interannual differences in Volume affect wind risk on stand level??
# ordered differences in wind risk by landscape - total sum - over time
# ordered differences in Volume by landscape - total sum - over time
# plot by the % of SA and main 3 scenarios:
# ----------------------------
# Maybe to be continued ... ????





range(df.all$V)

hist(df.all$V)


# What is the amount of timber volume at risk???





# BOxplot

df.sample %>% 
  #group_by(scenNumb, scenSimpl2) %>% 
  #summarize(risk.mean = mean(windRisk)) %>% 
  ggplot(aes(x = factor(scenNumb),
             y = windRisk,
             fill = factor(scenSimpl2))) + # ,
  #group = factor(scenSimpl2)
  geom_boxplot(outlier.size=0.5, 
               outlier.alpha = 0.3) +  # aes(fill = factor(scenSimpl2))
  #facet_grid(scenSimpl2 ~  .) +
  xlab("Income") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom")





# How does the % of SA affect windRisk???
# Calculate teh wind risk by scenario oevr time and plot with the freq data
risk.mean <- aggregate(windRisk ~ scenNumb + scenSimpl2, df.all, mean)

ggplot(risk.mean, 
       aes(x = factor(scenNumb),
           y = windRisk,
           color = scenSimpl2,
           group = scenSimpl2)) + # ,
  #group = factor(scenSimpl2)
  geom_line(size = 1.5) +
  #facet_grid(scenSimpl2 ~  .) +
  xlab("Income") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom")




# -----------------------------------------
# How does the % of SA per scenario affect windrisk??
# !!! ??? how to get teh % of SA by scenario???
# Count the number of stands with SA regime
# --------------------------------------------
prop.regimes<-
  df.all %>% 
  group_by(scenario, avohaakut) %>% 
  distinct(id) %>% 
  summarise(stands_n = n()) %>%
#  filter(avohaakut == "SA") %>% 
#  head() 
  mutate(freq = 100* (stands_n / sum(stands_n))) %>% 
  arrange(scenario) 


  
# Get teh proportion of SA by the each scenario??
sa.share <- 
  df.all %>% 
  group_by(scenSimpl2,scenNumb, avohaakut) %>% 
  distinct(id) %>% 
  summarise(stands_n = n()) %>%
  filter(avohaakut == "SA") %>% 
  mutate(SA_share = 100* (stands_n / 1470)) #%>%
 # print(n = 63)
 # arrange(scenario) 

ggplot(sa.share, aes(x = scenNumb,
                     y = SA_share,
                     group = scenSimpl2,
                     color = scenSimpl2)) +
  geom_line()


write.csv(sa.share, "output/SA_share.csv")

# Calculate how many different regimes are by each scenario:
regime.n <-
  df.all %>% 
  group_by(scenario) %>% 
  distinct(avohaakut) %>% 
  summarise(regimes_n = n()) %>%
  #mutate(freq = 100* (stands_n / sum(stands_n))) %>% 
  arrange(scenario) 




# Get the % of the SA per scenario to add as new variable
# do all scenarios have SA??? YES
SA.perc <- 
  prop.regimes %>% 
  filter(avohaakut == "SA") %>% 
  arrange(freq) #%>% 
#print(n = 70)



# Does the all have 100% SA???? 
# YES

#     ALL0      SA             1472 100   
#  62 CCF0      SA             1472 100   
#  63 not_CCF0  SA             1472 100     




# Check if the regimes couls are correct:
# ALL11 should have 48 avohaakut regimes 
unique(subset(df.all, scenario == "ALL11")$avohaakut)


# How does the % of SA affect windRisk???
# Calculate teh wind risk by scenario oevr time and plot with the freq data
risk.mean <- aggregate(windRisk ~ scenario + year, df.all, mean)


# join the SA% data:

# Add the % of SA to each scenario
risk.mean <- 
  risk.mean %>% 
  left_join(SA.perc, by= "scenario") %>%
    
  left_join(regime.n, by = "scenario")

# clasify in 3 groups:
risk.mean <- risk.mean %>% 
  mutate(simpleScen = case_when(
    str_detect(scenario, "not_CCF") ~ "RF",
    str_detect(scenario, "ALL") ~ "ALL",
    str_detect(scenario, "CCF") ~ "CCF"))


# Plot data: wind risk by % of SA
ggplot(risk.mean, 
       aes(x = freq,  # % of stands with SA
           y = windRisk)) +
  geom_point(aes(color = factor(simpleScen))) + 
  geom_line(aes(color = factor(simpleScen))) + 
  xlab("SA by landscape (%)") +
  ylab("mean wind damage risk (%)") +
  labs(color = "scenario") +
  facet_grid(. ~ year) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Something is ahappening over last three study years:
# what is my tree height and tree age?



# ------------------------------------------
# Explore individual predictors:
# ---------------------------------------
# How does the H_dom changes over year???
# Increases, less variability in RF and with the lower % of the SA
ggplot(df.all, 
       aes(x = factor(year),  # % of stands with SA
           y = H_dom,
           group = factor(year))) +
  geom_boxplot() +
  facet_grid(scenSimpl2 ~  scenNumb) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(df.all, 
       aes(x = factor(scenNumb),
           y = H_dom,
           fill = factor(scenSimpl2))) + # ,
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom")



# What about BA?
# also increases over years
ggplot(df.all, 
       aes(x = year,  # % of stands with SA
           y = BA,
           group = year)) +
  geom_boxplot() +
  facet_wrap(. ~ scenario) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# BA
ggplot(df.all, 
       aes(x = factor(scenNumb),
           y = BA,
           fill = factor(scenSimpl2))) + # ,
  geom_boxplot() +  # aes(fill = factor(scenSimpl2))
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom")


# Volume
ggplot(df.all, 
       aes(x = factor(scenNumb),
           y = V,
           fill = factor(scenSimpl2))) + # ,
  geom_boxplot() +  # aes(fill = factor(scenSimpl2))
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom")



# Diameter: D_gm
ggplot(df.all, 
       aes(x = factor(scenNumb),
           y = D_gm,
           fill = factor(scenSimpl2))) + # ,
  geom_boxplot() +  # aes(fill = factor(scenSimpl2))
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom")




# ----------------------------------------------
# Specific questions:
# -----------------------------------------------

# Does particular management regime increase wind risk?

ggplot(df.sample, 
       aes(x = avohaakut ,  # % of stands with SA
           y = windRisk,
           group = avohaakut,
           fill = simpleScen)) +
  geom_boxplot() +
  facet_wrap(. ~ year) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# High variability in RF forestry, chack which one it is?

ggplot(subset(df.all, simpleScen == "RF" & avohaakut != "SA" ), 
       aes(x = avohaakut ,  # % of stands with SA
           y = windRisk,
           group = avohaakut)) +
  geom_boxplot() +
 facet_wrap(. ~ year) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(subset(df.all, simpleScen == "RF" & avohaakut != "SA" ), 
       aes(x = avohaakut ,  # % of stands with SA
           y = windRisk,
           group = avohaakut)) +
  geom_boxplot() +
  #facet_wrap(. ~ year) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# HOw does wind risk differs between SA/Non-SA regimes???
ggplot(df.sample, aes(y = windRisk,
                      x = simpleScen,
                      color = twoRegm)) +
  geom_boxplot() 

# evaluate over years:
# SA is very stable over time
ggplot(df.sample, aes(y = windRisk,
                      x = simpleScen,
                      color = twoRegm)) +
  geom_boxplot() +
  facet_grid(.~year)

# what is the trend over SA gradient? the difference in windRisk between SA and non-SA regimes:
risk.mean <- aggregate(windRisk ~ scenNumb + scenSimpl2 + twoRegm, df.all, mean)

ggplot(risk.mean, 
       aes(x = factor(scenNumb),
           y = windRisk,
           color = scenSimpl2,
           group = scenSimpl2)) + # ,
  #group = factor(scenSimpl2)
  geom_line(size = 1.5) +
  facet_grid(.~ twoRegm) +
  xlab("Income") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom")





# Very variable, bude definitely trends between CCF and RF
# What is the situation of teh SA
ggplot(subset(df.all, avohaakut == "SA"), 
       aes(x = year ,  # % of stands with SA
           y = windRisk,
           group = year)) +
  geom_boxplot() +
  facet_wrap(. ~ scenario) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




# Does combination of management and scenario increases wind risk?

# What terrain confirugation of SA increases wind risk???


# -----------------------------------
# How does the the alternative regimes over stand 
# affect stand level wind risk??
# -----------------------------------

# Calculate mean by stand and 4 alternatives by year, mean not sum beacsue I have more 
# CCFs then RF
df.mean <- aggregate(windRisk ~ id + year + avoh_Simpl, df.all, mean)

# Does every stand have 4 regimes?? NO!!!
table(df.mean$avoh_Simpl)

# CCF RF_noT   RF_T     SA 
# 27340  24780   9220  29400 


# Filter only values that have all 4 regimes:
df.mean %>% 
  arrange(id) %>% 
  head()


# Calulate differences among alternative states
# Calculate the differences 
df.mean.diff <- 
  df.mean %>%
  group_by(id, year) %>%
  mutate(windRisk.sa = windRisk[avoh_Simpl == 'SA']) %>% 
  mutate(diff = windRisk.sa - windRisk) %>% 
  arrange(id, year)


# Filter only stands that have all 4 regimes:
df.mean.diff <-
  df.mean.diff %>% 
  group_by(id, year) %>% 
  filter(all(c("CCF","RF_noT", "SA", "RF_T") %in% avoh_Simpl))



# PLot the differences:
pp1<- ggplot(subset(df.mean.diff, id == 6667292),
       aes(x = year,
           y = diff,
           color = avoh_Simpl)) +
  geom_line(size=1.2) + 
  ggtitle("differences from SA") +
  ylim(c(-0.025, 0.09)) + ylab("difference from SA") +
  theme()



# PLot the differences:
pp2<- ggplot(subset(df.mean.diff,  id == 6667292),
       aes(x = year,
           y = windRisk,
           color = avoh_Simpl)) +
  geom_line(size=1.2) +
  ylim(c(-0.025, 0.09)) +
  ggtitle("windrisk over time")




# H_dom
p.H <- df.all%>%
  filter(id == 6667292) %>% 
  group_by(id,year, avoh_Simpl)%>%
  summarise(H_dom_mean=mean(H_dom))%>%
  ggplot(aes(x = year,
             y = H_dom_mean))+
  geom_line(aes(color=avoh_Simpl),size=1.2)+
    ggtitle("mean H_dom")


# BA
p.BA <- df.all%>%
  filter(id == 6667292) %>% 
  group_by(id,year, avoh_Simpl)%>%
  summarise(BA_mean=mean(BA))%>%
  ggplot(aes(x = year,
             y = BA_mean))+
  geom_line(aes(color=avoh_Simpl),size=1.2)+
  ggtitle("mean BA")


# V
p.V <- df.all%>%
  filter(id == 6667292) %>% 
  group_by(id,year, avoh_Simpl)%>%
  summarise(V_mean=mean(V))%>%
  ggplot(aes(x = year,
             y = V_mean))+
  geom_line(aes(color=avoh_Simpl),size=1.2)+
  ggtitle("mean V")




# Risk
windows()
ggarrange(pp1, pp2,
          ncol=2, 
          nrow=1, common.legend = TRUE, legend="bottom")

# Characteristics
windows()
ggarrange(p.H, p.BA, p.V,
          ncol=3, 
          nrow=1, common.legend = TRUE, legend="bottom")



# ------------------------------------
# How does time since thinning affect wind risk??
# ------------------------------------

# Column 'difference' shows "time since thinngs"

df.all %>% 
  #filter(id == 6667292) %>% 
  distinct(difference)


# CHeck single stand example: LRT5 - rotation with thinnings, id 6667292
# does this contains duplicated rows???
df.all %>% 
  filter(id == 6667292 & avohaakut == "LRT30") %>% 
  dplyr::select(id, year, THIN, H_dom, BA, THIN_filled_lagged, difference, scenSimpl2 )


# what is the range of thinnings in CCF? "CCF_3_45"
df.all %>% 
  filter(id == 6667292 ) %>%
  distinct(avohaakut)
  

#   avohaakut
#1        SA
#2  THwoTM20
#3     LRH30
#4     LRT30
#5  CCF_4_45
#6  CCF_3_45


df.all %>% 
  filter(id == 6667292 & avohaakut == "CCF_3_45") %>% 
  dplyr::select(id, year, THIN, H_dom, BA, THIN_filled_lagged, difference, scenSimpl2 ) #%>%
  #distinct(difference)



# Plot:
# does time since thinning predict wind risk??? 

windows()
df.sample %>% 
  ggplot(aes(x = difference,
             y = windRisk,
             color = avoh_Simpl)) +
 #geom_jitter(alpha = 0.3, size = 0.1)  +
 geom_smooth(method = "lm",
             formula = y~ log(x)) +
  xlab("Years since thinning")


# show all regimes???
df.all %>% 
  ggplot(aes(x = difference,
             y = windRisk,
             color = avohaakut)) +
  #geom_jitter(alpha = 0.3, size = 0.1)  +
  geom_smooth(method = "lm",
              formula = y~ log(x)) +
  xlab("Years since thinning")


table(df.all$difference)


# -----------------------------------
# Some of the CCF ahe THIN 0:
# -----------------------------------
# check what are the values??
unique(subset(df.all, simpleScen == "CCF")$time_thinning)
#[1] "0-5"

unique(subset(df.all, simpleScen == "CCF")$difference)


# Subset two regimes and recalculate teh THIN values:
df.s <- df.all %>% 
  filter(id == 6667292 & (avohaakut == "CCF_3_45" | avohaakut == "LRT30")) %>% 
  dplyr::select(id, year, THIN, H_dom, BA, THIN_filled_lagged, difference, avohaakut) #%>%
#distinct(difference)

str(df.s)

unique(df.s$THIN)


df.s.d<- 
  df.s %>% 
  distinct()

 
# -------------------------
# stand 'fidelity' to individual regimes??
# --------------------------
# how many different regimes each stand has?


stand.fidelity <-
  df.all %>% 
  group_by(id) %>% 
  distinct(avohaakut)  %>% 
  summarise(regimes_n = n()) #%>%
  #mutate(freq = 100* (stands_n / sum(stands_n))) %>% 
  #arrange(scenario) 

hist(stand.fidelity$regimes_n)

# how many regimes each stand has under different regimes?
stand.fid.scen <-
  df.all %>% 
  group_by(id, simpleScen) %>% 
  distinct(avohaakut)  %>% 
  summarise(regimes_n = n()) #%>%

ggplot(stand.fid.scen, aes(regimes_n)) +
  geom_histogram() + 
  facet_grid(.~ simpleScen)



# Which are those regimes???
# are all the regimes used overall, or not?
df.regimes <- 
  df.all %>% 
    group_by(id, simpleScen) %>% 
    distinct(avohaakut)  #%>% 
  

# How many unique regimes?
length(unique(df.regimes$avohaakut))
# Yes, all of teh regimes were used


# How many of each?
# how oftern individual regimes occurs over scenarios???
# Dominance of SA, in ALL& RF THwoTH20 seems to prevail
# in CCF it is CCF 1, CCF2, CCF3, CCF4even not the extensions

ggplot(df.regimes, 
       aes(x = avohaakut, fill = simpleScen)) +
  geom_bar(stat = "count") +
  #facet_grid(.~ simpleScen) +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5))



# order the data by count
df.regimes.c <- 
  df.regimes %>% 
  group_by(avohaakut, simpleScen) %>% 
    summarise(counts = n()) %>% 
    arrange(desc(counts)) 

# Change the levels order manually (does not work with mutate):
df.regimes.c$avohaakut <- factor(df.regimes.c$avohaakut, 
                                 levels = unique(df.regimes.c$avohaakut))

# Plot the mos useful regimes
windows(width = 7, height = 3.5)

df.regimes.c %>% 
  ggplot(aes(x = avohaakut, 
             y = counts,
             fill = simpleScen,
             label = counts)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, 
                                   size = 7,
                                   face="italic")) 
  

# Show just ones with frequebcy over 100:
df.regimes.c %>% 
  filter(counts> 100) %>% 
  ggplot(aes(x = avohaakut, 
             y = counts,
             fill = simpleScen,
             label = counts)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5)) + 
  geom_text(aes(x = avohaakut, 
                y = counts,
                group = simpleScen),
            angle = 90,
            check_overlap = FALSE,
            size = 3,
            vjust = 0.5,
            hjust = 0,
            position = position_stack(vjust = 0.3))


# How does the % of SA affect which 
# How does the frequency of regimes changes over scenarios (63?)
# Get disctinct regimes by 
df.regimes.sa <- 
  df.all %>% 
  group_by(id, simpleScen, scenNumb) %>% 
  distinct(avohaakut)  #%>% 
 
# by %SA/landscape?



df.regimes

# How many where??
table(df.regimes.c$simpleScen, df.regimes.c$avohaakut)

length(unique(df.regimes$id))  # 1470


# ---------------------------------------------------
# What is the sum of wind risk and total NFI value for scenario?
# --------------------------------------------------




# Is MF the aggregate of the NFa, MFb, MFc, MFd?
head(df.npi)


# Plot multifunctionnality
ggplot(df.npi, aes(x = NPI/10000,
                   y = MF,
                   group = TypeSimple,
                   color = TypeSimple)) + 
  geom_line() +
  ylab("multifunctionnality") +
  xlab("NPI") +
  ylim(0,3) + 
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, hjust = 1))


# Chack if different trends between using sum or min values??

wind.sum <- aggregate(windRisk ~ scenario, df.all, sum)
wind.mean <- aggregate(windRisk ~ scenario, df.all, mean)

wind.sum <-
  wind.sum %>% 
  left_join(df.npi, by = c("scenario" = "Type")) %>% 
  mutate(simpleScen = case_when(
    str_detect(scenario, "not_CCF") ~ "RF",
    str_detect(scenario, "ALL") ~ "ALL",
    str_detect(scenario, "CCF") ~ "CCF"))
  

wind.mean <-
  wind.mean %>% 
  left_join(df.npi, by = c("scenario" = "Type")) %>% 
  mutate(simpleScen = case_when(
    str_detect(scenario, "not_CCF") ~ "RF",
    str_detect(scenario, "ALL") ~ "ALL",
    str_detect(scenario, "CCF") ~ "CCF"))


# Plot NPI values: smae patterns by sum over year and as mean
# -------------------------
ggplot(wind.sum, aes(x = NPI,
                     y = windRisk,
                     group = simpleScen,
                     color = simpleScen)) + 
  geom_line() +
  ylab("total sum wind risk") +
  xlab("NPI k € by ha")


windows()  
ggplot(wind.mean, aes(x = NPI/1000000,
                     y = windRisk,
                     group = simpleScen,
                     color = simpleScen)) + 
  geom_line(lwd = 1.5) +
  ylab("mean wind risk") +
  xlab("NPI k € by ha")




# Plot data : wind risk by % of regime diversity
#ggplot(risk.mean, 
 #      aes(x = regimes_n.y ,  # % of stands with SA
 #          y = windRisk)) +
#  geom_point(aes(color = factor(simpleScen))) + 
 # geom_line(aes(color = factor(simpleScen))) + 
 # xlab("#regimes by landscape (%)") +
 # ylab("mean wind damage risk (%)") +
 # labs(color = "scenario") +
  # facet_grid(. ~ ) +
 # theme(axis.text.x = element_text(angle = 90, hjust = 1))



# ----------------------------------------------------
# Compare the wind risk between individual stands under different regimes???
# ---------------------------------------------------------------

# How does the % of SA affect windRisk???
# Calculate teh wind risk by scenario oevr time and plot with the freq data
stand.risk.mean <- aggregate(windRisk ~ scenario + id, df.all, mean)


# join the SA% data:

# Add the % of SA to each scenario
stand.risk.mean <- stand.risk.mean %>% 
 left_join(SA.perc, by= "scenario")

# clasify in 3 groups:
stand.risk.mean <- stand.risk.mean %>% 
  mutate(simpleScen = case_when(
    str_detect(scenario, "not_CCF") ~ "RF",
    str_detect(scenario, "ALL") ~ "ALL",
    str_detect(scenario, "CCF") ~ "CCF"))



# Plot data
ggplot(stand.risk.mean, 
       aes(x = simpleScen ,  # % of stands with SA
           y = windRisk)) +
  #geom_point(aes(color = factor(simpleScen))) + 
  #geom_line(aes(color = factor(simpleScen))) + 
  geom_boxplot(fill = "grey92") +
  xlab("SA by landscape (%)") +
  ylab("mean wind damage risk (%)") +
  labs(color = "scenario") +
  # facet_grid(. ~ ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




# How does the diversification (higher number of regimes) affect windRisk?


# How 

# Temporal dynamics of wind risk of management
ggplot(df.sim, 
       aes(x = as.factor(year),
           y = windRisk)) +
  geom_boxplot() + 
  facet_grid(. ~ regime) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Is the wind risk the same for all regimes in the first simulation year??
p.windRisk <- ggplot(subset(df.sim, year == 2016), 
                     aes(x = regime,
                         y = windRisk)) +
  geom_boxplot() +
  ggtitle("Wind risk 2016") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p.H_dom <- ggplot(subset(df.sim, year == 2016), 
                  aes(x = regime,
                      y = H_dom)) +
  geom_boxplot() +
  ggtitle("H_dom 2016")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


p.BA <- ggplot(subset(df.sim, year == 2016), 
               aes(x = regime,
                   y = BA)) +
  geom_boxplot()  +
  ggtitle("BA 2016")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggarrange(p.windRisk, p.H_dom, p.BA, 
          labels = c("A", "B", "C"),
          ncol = 3, nrow = 1)

# Why initial stand conditions are not the same 
#  between management regimes???
# ------------------------------------------
# How many stands with open edge each regime has??

# Sum up the area& count of stands with open_edge = TRUE
head(df.sim)

# how many stands with open_edge == TRUE are in every year by manage regime?

open.edge.count<- 
  df %>% 
  group_by(year, regime) %>% 
  filter(open_edge == TRUE) %>% 
  tally() %>% 
  arrange(regime)


ggplot(open.edge.count, 
       aes(x = as.factor(year),
           y = n, 
           group = regime,
           color = regime)) +
  geom_line() + 
  # facet_grid(. ~ regime) +
  ggtitle("Count of stands with open edge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




# Investigate when the THIN happen???
df.thin <- subset(df, regime == "BAUwT_GTR")


# Track down history of one stand:
# when does thinning occured??

subset(df.thin, standid == 12469153,
       select = c("standid", "year", "THIN"))




# Investigate when the THIN happen???
df.cc2<- subset(df, regime == "CCF_2")


# Track down history of one stand:
# when does thinning occured??

subset(df.cc2, standid == 12469153,
       select = c("standid", "year", "THIN"))




# -------------------------------
#
# Check for differences in wind risk values 
# for individual stand between SA and no SA overall wind risk values??
#
# -------------------------------
head(df.all)

stand.risk.rgm.df <- aggregate(windRisk ~ id + avohaakut, df.all, mean)


ggplot(stand.risk.rgm.df, aes(x = avohaakut,
                              y = windRisk)) +
  geom_boxplot()



# How many stands have actually SA option included?
length(unique(subset(df.all, avohaakut == "SA")$id))
#1470 

length(unique(df.all$id))
# 1470

# are some stands consistently choosed as suitable for SA? seems that not 
# as SA was simulated for each stand?

# Compare teh min between SA and no-SA regime by scenario???

# Create 4 basic groups depending on regime:
# SA, RF with and without thinng and CCF (always thinning included)
df.all <-
  df.all %>% 
  mutate(avoh_Simpl = case_when(
    str_detect(avohaakut, "SA")   ~ "SA",
    str_detect(avohaakut, "CCF_") ~ "CCF",
    str_detect(avohaakut, "LRH")  ~ "RF_noT",
    str_detect(avohaakut, "LRT")  ~ "RF_T",
    str_detect(avohaakut, "SR5")  ~ "RF_noT",
    str_detect(avohaakut, "SRT5") ~ "RF_T",
    str_detect(avohaakut, "TH")   ~ "RF_noT",
    str_detect(avohaakut, "TT")   ~ "RF_T"))
         
# Check if correct
subset(df.all, avohaakut == "TT")


# ten join into single table
stand.sa.min <- aggregate(windRisk ~ id + avoh_Simpl, 
                       subset(df.all, avohaakut == "SA"), min) %>% 
  mutate(regime = "SA")

stand.no.sa.min <- aggregate(windRisk ~ id + avoh_Simpl, 
                          subset(df.all, avohaakut != "SA"), min) %>% 
  mutate(regime = "no SA")

# BInd data into one long table
merged.sa <- rbind(stand.sa.min,
                   stand.no.sa.min)

unique(merged.sa$regime)
unique(merged.sa$avoh_Simpl)

length(unique(subset(merged.sa, regime == "SA")$id))



# Calculate the paired differences between regimes for stand??
# does every stand has all 4 categories???
table(merged.sa$avoh_Simpl)

# NO, but all of them nhave SA 
# CCF RF_noTHIN   RF_THIN        SA 
# 1367      1239       461      1470 

# Calculate the differences 
merged.diff <- 
  merged.sa %>%
  group_by(id) %>%
  mutate(windRisk.sa = windRisk[avoh_Simpl == 'SA']) %>% 
  mutate(diff = windRisk.sa - windRisk) %>% 
    arrange(id)


# Plot differences by groups
ggplot(subset(merged.diff, avoh_Simpl != "SA"), 
       aes(diff)) +
  geom_histogram() + 
  facet_grid(.~avoh_Simpl)








# Create paired ggplot
ggplot(merged.sa, aes(x = avoh_Simpl,
                      y = windRisk,
                      group = id)) +
  geom_point(aes(colour=avoh_Simpl),
             size=2.5,
             alpha=0.01, 
             position=position_dodge(width=0.1)) +
  geom_line(size=1, 
            alpha=0.01, 
            position=position_dodge(width=0.1))




# how many sctand I have in all??
length(unique(stand.sa.min$id))
# 1470

length(unique(stand.no.sa.min$id))
# 1416

# Let's check it for one stand:
# 6667291

aggregate(windRisk ~  avohaakut, 
          subset(df.all, id == 6667291), min)



d<-aggregate(windRisk ~  avohaakut, 
          subset(df.all, id == 12538077), min)


ggplot(d, aes(x = avohaakut,
              y = windRisk)) +
  geom_boxplot()



# ------------------------
# Analyse the data & 
#     Make boxplots:
# --------------------------

# set theme for ggplot2
theme_set(theme_classic())

# How does scenarios (63) differ in term of wind risk???
ggplot(df.all, 
       aes(x = as.factor(year),
           y = windRisk)) +
  geom_boxplot() + 
  facet_grid(. ~ simpleScen) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



ggplot(df.all, 
       aes(x = as.factor(simpleScen),
           y = windRisk)) +
  geom_boxplot(fill = "grey92") + 
  #facet_grid(. ~ ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# ------------------------
# Categorical variables: 
# Wind risk by time _since thining
# ------------------------

p.edge <- ggplot(df.all, 
       aes(x = as.factor(open_edge),
           y = windRisk,
           fill = open_edge)) +
  geom_boxplot() +  # fill = "grey92"
  facet_grid(. ~ simpleScen) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Open_edge") +
  theme(legend.position = "none")


p.thin <- ggplot(df.all, 
                 aes(x = as.factor(since_thin),
                     y = windRisk,
                     fill = since_thin)) +
  geom_boxplot() +  # fill = "grey92"
  facet_grid(. ~ simpleScen) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Time since thinning")+
  theme(legend.position = "none")



p.soilDepth <-
  ggplot(df.all, 
                   aes(x = as.factor(soil_depth_less30),
                       y = windRisk,
                       fill =soil_depth_less30 )) +
  geom_boxplot() +  # fill = "grey92"
  facet_grid(. ~ simpleScen) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Soil depth<30cm") +
  theme(legend.position = "none")


p.soilType <-
  ggplot(df.all, 
         aes(x = as.factor(soilType),
             y = windRisk, 
             fill =soilType )) +
  geom_boxplot() + #
  facet_grid(. ~ simpleScen) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Soil type") +
  theme(legend.position = "none")


p.species <-
  ggplot(df.all, 
                 aes(x = as.factor(species),
                     y = windRisk,
                     fill = species)) +
  geom_boxplot() +  # fill = "grey92"
  facet_grid(. ~ simpleScen) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Species") +
  theme(legend.position = "none")

p.PEAT <-
  ggplot(df.all, 
         aes(x = as.factor(PEAT),
             y = windRisk,
             fill = PEAT)) +
  geom_boxplot() +  # fill = "grey92"
  facet_grid(. ~ simpleScen) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("PEAT") +
  theme(legend.position = "none")




# Put data togetehr into one plot
grid.arrange(p.edge,
             p.thin,
             p.soilDepth,
             p.soilType,
             p.species,
             p.PEAT,
             nrow = 2, ncol = 3)



# Calculate teh means, min, max of wind risk over landscape by time
# -----------------------------------------

risk.min<- aggregate(windRisk ~ scenario, df.all, min)
risk.max<- aggregate(windRisk ~ scenario, df.all, max)
risk.range <- cbind(risk.min, risk.max$windRisk)
names(risk.range) <- c("scenario", "min", "max")


risk.range <- risk.range %>% 
  mutate(simpleScen = case_when(
    stringr::str_detect(scenario, "not_CCF") ~ "RF",
    stringr::str_detect(scenario, "ALL") ~ "ALL",
    stringr::str_detect(scenario, "CCF") ~ "CCF"))


ggplot(risk.range, aes(scenario)) +
  #geom_linerange(x =  windRisk.1,
   #              y = windRisk.2) +
  geom_linerange(aes(ymin=min,
                     ymax=max),
                 linetype=2,
                 color="blue")+
  geom_point(aes(y=min),size=3,color="red")+
  geom_point(aes(y=max),size=3,color="red")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))



# Get means by scenarios, merge with NPI data
risk.mean <- aggregate(windRisk ~ scenario, df.all, mean)





# Get the % of the SA per scenario to add as new variable
# do all scenarios have SA??? YES
SA.perc <- 
  prop.regimes %>% 
  filter(avohaakut == "SA") %>% 
  arrange(freq) #%>% 
#print(n = 70)


# Does the all have 100% SA????
#     ALL0      SA             1472 100   
#  62 CCF0      SA             1472 100   
#  63 not_CCF0  SA             1472 100     



# How does the % of SA affect windRisk???
# Calculate teh wind risk by scenario oevr time and plot with the freq data
risk.mean <- aggregate(windRisk ~ scenario, df.all, mean)


# join the SA% data:

# Add the % of SA to each scenario
risk.mean <- risk.mean %>% 
  left_join(SA.perc, by= "scenario") %>% 
  left_join(regime.n, by = "scenario")

# clasify in 3 groups:
risk.mean <- risk.mean %>% 
  mutate(simpleScen = case_when(
    str_detect(scenario, "not_CCF") ~ "RF",
    str_detect(scenario, "ALL") ~ "ALL",
    str_detect(scenario, "CCF") ~ "CCF"))


# Plot data: wind risk by % of SA
ggplot(risk.mean, 
       aes(x = freq,  # % of stands with SA
           y = windRisk)) +
  geom_point(aes(color = factor(simpleScen))) + 
  geom_line(aes(color = factor(simpleScen))) + 
  xlab("SA by landscape (%)") +
  ylab("mean wind damage risk (%)") +
  labs(color = "scenario") +
  # facet_grid(. ~ ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Plot data : wind risk by % of regime diversity
ggplot(risk.mean, 
       aes(x = regimes_n.y ,  # % of stands with SA
           y = windRisk)) +
  geom_point(aes(color = factor(simpleScen))) + 
  geom_line(aes(color = factor(simpleScen))) + 
  xlab("#regimes by landscape (%)") +
  ylab("mean wind damage risk (%)") +
  labs(color = "scenario") +
  # facet_grid(. ~ ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Compare the wind risk between individual stands under different regimes???
# ---------------------------------------------------------------

# How does the % of SA affect windRisk???
# Calculate teh wind risk by scenario oevr time and plot with the freq data
stand.risk.mean <- aggregate(windRisk ~ scenario + id, df.all, mean)


# join the SA% data:

# Add the % of SA to each scenario
#risk.mean <- risk.mean %>% 
#  left_join(SA.perc, by= "scenario")

# clasify in 3 groups:
stand.risk.mean <- stand.risk.mean %>% 
  mutate(simpleScen = case_when(
    str_detect(scenario, "not_CCF") ~ "RF",
    str_detect(scenario, "ALL") ~ "ALL",
    str_detect(scenario, "CCF") ~ "CCF"))



# Plot data
ggplot(stand.risk.mean, 
       aes(x = simpleScen ,  # % of stands with SA
           y = windRisk)) +
  #geom_point(aes(color = factor(simpleScen))) + 
  #geom_line(aes(color = factor(simpleScen))) + 
  geom_boxplot(fill = "grey92") +
  xlab("SA by landscape (%)") +
  ylab("mean wind damage risk (%)") +
  labs(color = "scenario") +
  # facet_grid(. ~ ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




# How does the diversification (higher number of regimes) affect windRisk?


# How 

# Temporal dynamics of wind risk of management
ggplot(df.sim, 
       aes(x = as.factor(year),
           y = windRisk)) +
  geom_boxplot() + 
  facet_grid(. ~ regime) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Is the wind risk the same for all regimes in the first simulation year??
p.windRisk <- ggplot(subset(df.sim, year == 2016), 
                     aes(x = regime,
                         y = windRisk)) +
  geom_boxplot() +
  ggtitle("Wind risk 2016") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p.H_dom <- ggplot(subset(df.sim, year == 2016), 
                  aes(x = regime,
                      y = H_dom)) +
  geom_boxplot() +
  ggtitle("H_dom 2016")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


p.BA <- ggplot(subset(df.sim, year == 2016), 
               aes(x = regime,
                   y = BA)) +
  geom_boxplot()  +
  ggtitle("BA 2016")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggarrange(p.windRisk, p.H_dom, p.BA, 
          labels = c("A", "B", "C"),
          ncol = 3, nrow = 1)


>>>>>>> ee48d1ee34b9d28757a4413774a1f71775676709
