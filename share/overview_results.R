

# 
rm(list = ls())

# , eval = FALSE
library(data.table)
library(dplyr)
library(raster)
library(ggplot2)
library(sf)
library(stringr)


theme_set(theme_bw())


# Read datasets:
df.all <- fread("C:/MyTemp/myGitLab/windDamage/output/df_sim_windRisk.csv")

# 

# get the NPVI&NPI values over scenarios 
df.npi <- fread("C:/MyTemp/avohaakut_db/analyzed/scenario_NPI.csv")

# -----------------------------------------
# read stand geometry data
# ------------------------------------------
# stands that are not simulated
stands.remove <-c(13243875, 
                  13243879, 
                  13243881,
                  6685176)    # H_dom is >150 m



# Subset stands only for normal H_dom values
df.all <- subset(df.all, !id %in% stands.remove)


# stands geometry
df.geom <- st_read("C:/MyTemp/avohaakut_db/14.534/14.534/mvj_14.534.shp")
df.geom <- subset(df.geom, select = c("KUVIO_ID"))
names(df.geom) <- c("standid", "geometry")
df.geom$area <- st_area(df.geom)
#df.geom <- subset(df.geom, !standid %in% stands.remove)
df.geom <- subset(df.geom, standid %in% unique(df.all$id))

# Read Suvanto's raster
# Avohaakut: [1] 1.379309e-13 2.178822e-01
#r.windRisk <- raster("C:/MyTemp/myGitLab/windDamage/data/pred_prob_N4.tif")

# Transform tge stand geometry to fit over arster
#df.geom.t<-st_transform(df.geom, crs = st_crs(r.windRisk))


# -------------------------
#    Visualise results
# -------------------------
#



# ------------------------
# Analyse the data & 
#     Make boxplots:
# --------------------------
library(ggplot2)


# set theme for ggplot2
theme_set(theme_classic())

# How does scenarios (63) differ in term of wind risk???
# df.all

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



ggplot(df.all, 
       aes(x = as.factor(scenario),
           y = windRisk)) +
  geom_boxplot(fill = "grey92") + 
  # facet_grid(. ~ ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# How does the % of SA per scenario affect windrisk??
# !!! ??? how to get teh % of SA by scenario???
# Count the number of stands with SA regime

prop.regimes<-
  df.all %>% 
  group_by(scenario, avohaakut) %>% 
  distinct(id) %>% 
  summarise(stands_n = n()) %>%
  mutate(freq = 100* (stands_n / sum(stands_n))) %>% 
  arrange(scenario) 



# Calculate how many differe regimes are by each scenario:
regime.n <-
  df.all %>% 
  group_by(scenario) %>% 
  distinct(avohaakut.x) %>% 
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
#     ALL0      SA             1472 100   
#  62 CCF0      SA             1472 100   
#  63 not_CCF0  SA             1472 100     




# Check if the regimes couls are correct:
# ALL11 should have 48 avohaakut regimes 
unique(subset(df.all, scenario == "ALL11")$avohaakut)


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



# Check for monetary values??








# Merge df data with geometry
# Join the geometry table with simulated data

# ----------------------------------------
df.bau <- subset(df.sim, regime == "BAU")
# convert factor to integer
df.geom$standid <- as.numeric(as.character(df.geom$standid ))

# Merge data together
df.bau.g<-
  df.geom %>% 
  left_join(df.bau, by = "standid") %>% 
  filter(!is.na(year)) 


# Plot the variable over years:
# Plot attribute information:
windows()
ggplot(df.bau.g) +
  geom_sf(aes(fill = windRisk)) + 
  facet_wrap(.~year)



# Get data:
library(gapminder)

# Charge libraries:
library(ggplot2)
library(gganimate)
library(transformr)


# My data:

ggplot(df.bau.g) + 
  geom_sf(aes(fill = windRisk)) +
  scale_fill_continuous(low = "lightgreen", 
                        high = "darkgreen",
                        space = "Lab", 
                        na.value = "white", guide = "colourbar")+
  
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme_bw() +
  xlab("Longitude") + 
  ylab("Latitude") +
  # theme(axis.title=element_blank(),
  #      axis.text=element_blank(),
  #     axis.ticks=element_blank()) +
  # gganimate specific bits:
  labs(title = 'WindRISK BAU Year: {current_frame}') +
  transition_manual(year) +
  #transition_time(year) +
  ease_aes('linear')









## Explore where are SA regimes over landscape??
# ------------------------------------------

# How does the % of SA per scenario affect windrisk??
# !!! ??? how to get teh % of SA by scenario???
# Count the number of stands with SA regime

prop.regimes<-
  df.all %>% 
  group_by(scenario, avohaakut) %>% 
  distinct(id) %>% 
  summarise(stands_n = n()) %>%
  mutate(freq = 100* (stands_n / sum(stands_n))) %>% 
  arrange(scenario) 

# Calculate how many differe regimes are by each scenario:
regime.n <-
  df.all %>% 
  group_by(scenario) %>% 
  distinct(avohaakut) %>% 
  summarise(regimes_n = n()) %>%
  #mutate(freq = 100* (stands_n / sum(stands_n))) %>% 
  arrange(scenario) 



# --------------------
#     Plots
# --------------------


# Plot regimes over landscape

# Subset first different scenarios (% of SA)  
# ALL0, ALL10 ALL20
# add to geom data to plot it, in year 2016 as they are constant
all0 <- df.all %>% 
  filter(scenario == "ALL0" & year == 2016)
all0.sf <- left_join(df.geom, 
                     all0, 
                     by = c("standid" = "id"))

all10 <- df.all %>% 
  filter(scenario == "ALL10" & year == 2016)
all10.sf <- left_join(df.geom, all10, by = c("standid" = "id"))
all10.sf$avokReclas <- ifelse(all10.sf$avohaakut == "SA", "SA", "other")


all20 <- df.all %>% 
  filter(scenario == "ALL20" & year == 2016)
all20.sf <- left_join(df.geom, all20, by = c("standid" = "id"))
all20.sf$avokReclas <- ifelse(all20.sf$avohaakut == "SA", "SA", "other")




# Reclassify the tree species - add glm input data as well?
# chcek how the glm predicted wind risk was merged? based on stand id or just 
# as cbind? 



df.new <-
  df.new %>% 
  mutate(species = case_when(MAIN_SP == 1 ~ "pine",
                             MAIN_SP == 2 ~ "spruce",
                             TRUE ~ "other"))


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

# get the NPVI&NPI values over scenarios 
df.npi <- fread("C:/MyTemp/avohaakut_db/analyzed/scenario_NPI.csv")


# Complete the data!!  Convert to long format to allow treat this as groups

risk.mean <- df.npi %>% 
  left_join(risk.mean, by = c("Type" = "scenario")) %>% 
  mutate(simpleScen = case_when(
    stringr::str_detect(Type, "not_CCF") ~ "RF",
    stringr::str_detect(Type, "ALL") ~ "ALL",
    stringr::str_detect(Type, "CCF") ~ "CCF"))



plot(risk.mean, x = risk.mean$NPV, y = risk.mean$windRisk )


ggplot(df.all, 
       aes(x = H_dom,
           y = windRisk,
           group = avohaakut,
           col = avohaakut)) + 
  geom_point()


i = c("H_dom")
ggplot(data=df.all, aes_string(x = i, y = "windRisk")) + geom_point() 





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



# Check for monetary values??


