
# ----------------
# Create some pre-plots
# ----------------

# selet the some plots
# check if they differ


# 
rm(list = ls())



# ----------------------------------
# start the script: using rgdal library
# ----------------------------------

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
library(broom)
#library(RColorBrewer)


theme_set(theme_classic())
theme_update(panel.grid.major = element_line(colour = "grey95",
                                             size = 0.1,
                                             linetype = 2),
             strip.background = element_rect(color="grey95", 
                                             fill="grey95",
                                             size=0.1, 
                                             linetype="solid"))


# Read corrected simulated names
# ------------------------------
df<- data.table::fread("C:/MyTemp/myGitLab/windDamage/manuscript_regimes/output/df_sim_windRisk.csv", 
                       data.table=FALSE, 
                       stringsAsFactors = FALSE)


# Read geometry data
# ---------------------
df.geom <- st_read(paste(getwd(), "14.534/14.534/mvj_14.534.shp", sep = "/"))
df.geom <- subset(df.geom, select = c("KUVIO_ID"))
names(df.geom) <- c("id", "geometry")
df.geom <- subset(df.geom, id %in% unique(df$id))




# --------------------------
# Define the plotting 
# --------------------------

# DEfine own palette
# color blind
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# -------------------------------
# Plot differences in wind risk among regimes
# -------------------------------
##p.mean.windRisk.line.npi2 <-
  df %>% 
  ggplot(aes(y = windRisk, 
             x = avohaakut )) + #, 
             #shape = scenSimpl2,     
             #color = scenSimpl2,     
             #linetype = scenSimpl2,  
             #group = scenSimpl2,     
             #fill = scenSimpl2 )) +  
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
          legend.position = "right",
          strip.background =element_rect(fill="white", color = NA))
  


# filter CCF, keep only one, keep all RF
# ---------------------------
#unique(df$branching_new)
  
  
# Keep regimes, define them as factors to keep order
# some regimes do not have modification, only thin and not thin
keep.rgms <- c("CCF_1",                      # CCF, no
               "TH",                         # BAU, no
               "TT" ,                        # BAU thin, no
               "TTN" ,                       # BAU GRT,
               "SR5","SRT5",                 # BAU shorten
               "LRT5", "LRT10", "LRT15", "LRT30",    # longer thin
               "THwoTM20",                   # BAU shorten
               "THNS",                       # BAU GRT
               "THwoT",                      # BAU no thin
               "THwoT10",                    # BAU longer
               "LRH5", "LRH10", "LRH15", "LRH30", # BAU longer, no thin
               "SA"  )                       # SA


# Compare groups between themselves
df %>%   
  ggplot(aes(y = windRisk, 
             x = modif)) + #, 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "right",
        strip.background =element_rect(fill="white", color = NA))

  
# Check what is the development over years?
df %>%   
  ggplot(aes(y = windRisk, 
             x = modif)) + #, 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "right",
        strip.background =element_rect(fill="white", color = NA)) +
  facet_wrap(.~year)


# Make a line plot of the means over years??
df %>% 
  group_by(modif,
           year) %>% 
  summarize(mean.risk = mean(windRisk))  %>%
  ggplot(aes(y = mean.risk, 
             x = year,
             group = modif,
             color = modif,
             linetype = modif)) + #, 
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "right",
        strip.background =element_rect(fill="white", color = NA))



# standardize by to 'no adjustement'? 


# Calculate stand-wise differences in wind risk over years
df <- df %>% 
  group_by(id, avohaakut) %>% 
  arrange(id) %>% 
  mutate(risk.diff = windRisk - lag(windRisk)) # %>% 
#  dplyr::select(id, avohaakut, year, V, windRisk, Harvested_V, THIN, risk.diff) %>% 
 # print(n=40)


# differences between groups? 

df %>% 
group_by(modif,
         year) %>% 
  summarize(mean.diff = mean(risk.diff))  %>%
  ggplot(aes(y = mean.diff, 
             x = year,
             group = modif,
             color = modif,
             linetype = modif)) + #, 
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "right",
        strip.background =element_rect(fill="white", color = NA))


# order by differences, x = order number, y = ordered fifference

#
rep_time = nrow(df)/length(unique(df$year))

df %>% 
#  filter(modif == "extension") %>% 
  group_by(id, avohaakut) %>% 
  arrange(id, avohaakut, risk.diff) %>% 
  dplyr::mutate(n_ord = 1:20) %>%   # add ordenr numbers, this will recycle teh values
  #dplyr::select(id, avohaakut, year, V, windRisk, Harvested_V, THIN, risk.diff, n_ord)  %>% 
  ungroup() %>% 
  group_by(modif, n_ord) %>% 
  summarize(mean.diff = mean(risk.diff, na.rm = TRUE)) %>% 
  ggplot(aes(y = mean.diff,
             x = n_ord,
             linetype = modif,
             group = modif,
             color = modif)) +
  geom_line(lwd = 1)



# Heck the changes within longer extension times?
# need to classify the +/- times
df.rf <- 
  df %>%
  filter(mainRegime == "RF") %>% 
  mutate(change_time = case_when(
    grepl("15", avohaakut) ~ "15",
    grepl("5",  avohaakut) ~ "5",
    grepl("10", avohaakut) ~ "10",
    grepl("30", avohaakut) ~ "30",
    grepl("20", avohaakut) ~ "20")) %>% 
  mutate(change_time = replace_na(change_time, 0)) %>% 
  mutate(change_time = as.character(change_time))
    


# Get +- times for CCF
# Filter only basic CCF that I do not overcomplicated ho to make 
# a new category of the postponing
df.ccf0 <- 
  df %>% 
  filter(avohaakut %in% c("CCF_1","CCF_2","CCF_3","CCF_4")) %>%
  mutate(change_time = 0) %>%  # remove 'type' columns
  mutate(change_time = as.character(change_time))

# Make the new time change category for CCF_X_XX
df.ccf.x <- 
  df %>% 
  filter(mainRegime == "CCF" & !avohaakut %in% c("CCF_1","CCF_2","CCF_3","CCF_4")) %>% 
 # mutate(help_col = avohaakut) %>%  # Create new colums that I will remove afterwards
  mutate(new_col = str_replace(avohaakut, "F_", ""))  %>%  # replace the part of string to get rid of _
  separate(new_col, c("type", "change_time"), sep = "_") %>% 
  dplyr::select(!type) %>%     #
  mutate(change_time = as.character(change_time))

  
  
# Merge the three columns together
df2 <- rbind(df.rf, df.ccf0, df.ccf.x)

df2$change_time <-factor(df2$change_time, 
                         levels = c("0",  "5",  "10", "15","20", "25","30", "35", "40", "45"))


# Check wind risk for avohaakut that have progress in time
# select the regime, shorten/longer and delay time
p.ext <- df2 %>% 
  filter(modif == "extension") %>%
  group_by(change_time, avohaakut, modif) %>%
  summarize(mean_risk = mean(windRisk, na.rm = TRUE))  %>% 
  ggplot(aes(x = change_time,
             y = mean_risk )) +#,
             #group = change_time)) +
  geom_boxplot() +
  ylim(0, 0.03) +
  ggtitle("Extension")
 # facet_wrap(.~modif)

p.short <- df2 %>% 
  filter(modif == "shortening") %>%
  group_by(change_time, avohaakut, modif) %>%
  summarize(mean_risk = mean(windRisk, na.rm = TRUE))  %>% 
  ggplot(aes(x = change_time,
             y = mean_risk )) +#,
  #group = change_time)) +
  geom_boxplot() +
  ylim(0, 0.03) +
  ggtitle("Shortening")

p.cc <- df2 %>% 
  filter(modif == "CC") %>%
  group_by(change_time, avohaakut, modif) %>%
  summarize(mean_risk = mean(windRisk, na.rm = TRUE))  %>% 
  ggplot(aes(x = change_time,
             y = mean_risk )) +#,
  #group = change_time)) +
  geom_boxplot() +
  ylim(0, 0.03) +
  ggtitle("CC")








# Not means, just values

df2 %>% 
  filter(modif == "extension" | modif == "shortening"| modif == "no")  %>%
  group_by(change_time, avohaakut, modif) %>%
  summarize(mean_risk = mean(windRisk, na.rm = TRUE))  %>% 
  ggplot(aes(x = change_time,
             y = mean_risk )) + #, #+ #,
            # y = windRisk, 
             #group = modif,
             #fill = change_time)) +#,
  #group = change_time)) +
  geom_boxplot() +
  facet_wrap(.~modif) +
  ylim(0, 0.05) +
  theme(legend.position = "bottom")
  #ggtitle("CC")








# Prin plots
windows(width = 7, height = 2.5)
ggarrange(p.ext, p.short, p.cc, 
          ncol = 3, nrow = 1,
          #widths = c(1, 1),
          common.legend = TRUE,
          align = c("hv"),
          legend="bottom",
          labels= "AUTO",
          hjust = -5,
          vjust = 3,
          font.label = list(size = 10, 
                            face = "bold", color ="black"))



