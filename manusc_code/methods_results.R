
# ----------------------------
# Results and methods for paper
# -----------------------------
rm(list = ls())


# Add this to your R code:
#.libPaths(c("/projappl/project_2003256/project_rpackages", #.libPaths()))
libpath <- .libPaths()[1]

.libPaths(c("/projappl/project_2003256/project_rpackages", .libPaths()))


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
#library(RColorBrewer)


theme_set(theme_classic())
theme_update(panel.grid.major = element_line(colour = "grey95",
                                             size = 0.1,
                                             linetype = 2),
             strip.background = element_rect(color="grey95", 
                                             fill="grey95",
                                             size=0.1, 
                                             linetype="solid"))



# Read input data
# =========================
df <- fread("/projappl/project_2003256/windDamage/output/final_df_solution8.csv")


# stands geometry
df.geom <- st_read(paste0(getwd(),"/14.534/14.534/mvj_14.534.shp"))

#df.geom <- st_read("C:/MyTemp/avohaakut_db/14.534/14.534/mvj_14.534.#shp")

df.geom <- subset(df.geom, select = c("KUVIO_ID"))
names(df.geom) <- c("id", "geometry")
df.geom$area <- st_area(df.geom)
df.geom <- subset(df.geom, id %in% unique(df$id))



# replace all NA in volume by 0 - because not volume is available there
df<- 
  df %>% 
  dplyr::mutate(V_stand_log = replace_na(V_stand_log, 0)) %>% 
  dplyr::mutate(V_stand_pulp = replace_na(V_stand_pulp, 0)) %>% 
  dplyr::mutate(V_strat_max = replace_na(V_strat_max, 0)) %>% 
  dplyr::mutate(V_strat_max_log = replace_na(V_strat_max_log, 0)) %>% 
  dplyr::mutate(V_strat_max_pulp = replace_na(V_strat_max_pulp, 0)) %>% 
  dplyr::mutate(Harvested_V_log_under_bark = replace_na(Harvested_V_log_under_bark, 0)) %>% 
  dplyr::mutate(Harvested_V_pulp_under_bark = replace_na(Harvested_V_pulp_under_bark, 0)) 


# Replace the no_SA values in TwoRegms: change to Management
df <- 
  df %>% 
  dplyr::mutate(Management = twoRegm) %>% # copy the columns 
  dplyr::mutate(Management = replace(Management, # replace the values
                                     Management == "no_SA", "Active")) %>%
  dplyr::mutate(Management = replace(Management, # replace the values
                                     Management == "SA", "Set Aside"))




# --------------------------
# Define the plotting 
# --------------------------

# DEfine own palette
# color blind
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

plot_line_details <- function() {
  list( 
    geom_line(aes(linetype = scenSimpl2, 
                  color = scenSimpl2),
              lwd = 0.8),
    scale_color_manual(values = cbp1),
    labs(color = "Scenario",
         linetype = "Scenario"), # geom_line(size = 1) + 
    # facet_wrap(.~ twoRegm), # + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
          legend.position = "bottom")
  )
}

plot_lineCumul_details <- function() {
  list( 
    geom_area(position="stack", 
              stat="identity",
              alpha = 0.2,
              color = "white"), 
    geom_line(position = "stack", 
              size = 1.2),
    scale_linetype_manual(values = c("solid", "dotdash")),
    scale_color_manual(values = cbp1),
    scale_fill_manual(values = cbp1),
    facet_wrap(.~scenSimpl2),
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
          legend.position = "bottom")
  )
}




# -------------------------
# make plots with means 
# -------------------------


p.mean.windRisk.line.npi <-
  df %>% 
  filter( scenSimpl2 != "ALL") %>% #Management == "active" &
  group_by(scenSimpl2, 
           NPI, 
           Management) %>% 
  summarize(sum.risk = mean(windRisk))  %>%
  ggplot(aes(y = sum.risk, 
             x = NPI, 
             shape = Management,
             color = Management,
             linetype = Management,
             group = Management,
             fill = Management)) +
  geom_line(#position = "stack",
    size = 0.7) +
  geom_point(#position = "stack",
    size = 1) +
  facet_wrap(.~scenSimpl2) +
  ylim(0,0.06) +
  ggtitle("") +
  xlab("Net present income\n(k€/ha)") + #
  ylab("Wind damage probability\n(mean, %)") +
  scale_linetype_manual(values = c("solid",  "dotted")) +
  scale_color_manual(values = cbp1) +
  scale_fill_manual(values = cbp1) +
  labs(shape = "Management",
       color = "Management",
       linetype = "Management",
       fill = "Management") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "right",
        strip.background =element_rect(fill="white", color = NA))



# Wind risk over time
p.mean.windRisk.line.time <-
  df %>% 
  filter( scenSimpl2 != "ALL") %>% #Management == "active" &
  group_by(scenSimpl2, 
           year, 
           Management) %>% 
  summarize(my.y = mean(windRisk))  %>%
  ggplot(aes(y = my.y, 
             x = year, 
             shape = Management,
             color = Management,
             linetype = Management,
             group = Management,
             fill = Management)) +
  geom_line(#position = "stack",
    size = 0.7) +
  geom_point(#position = "stack",
    size = 1) +
  facet_wrap(.~scenSimpl2) +
  ylim(0,0.06) +
  ggtitle("") +
  xlab("\nTime") + #
  ylab("Wind damage probability\n(mean, %)") +
  scale_linetype_manual(values = c("solid",  "dotted")) +
  scale_color_manual(values = cbp1) +
  scale_fill_manual(values = cbp1) +
  labs(shape = "Management",
       color = "Management",
       linetype = "Management",
       fill = "Management") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "right",
        strip.background =element_rect(fill="white", color = NA))



ggarrange(p.mean.windRisk.line.npi, 
          p.mean.windRisk.line.time,  
          ncol = 2, nrow = 1,
          widths = c(1, 1),
          common.legend = TRUE,
          align = c("hv"),
          legend="bottom",
          labels= "AUTO",
          hjust = -5,
          vjust = 3,
          font.label = list(size = 10, 
                            face = "bold", color ="black"))



# Top layer standing timber at risk??
# Plot log volume:
# --------------------------
p.mean.V.log.line.npi <-
  df %>% 
  filter( scenSimpl2 != "ALL") %>% #Management == "active" &
  group_by(scenSimpl2, 
           NPI, 
           Management) %>% 
  summarize(my_y = mean(V_strat_max_log ))  %>%
  ggplot(aes(y = my_y, 
             x = NPI, 
             shape = Management,
             color = Management,
             linetype = Management,
             group = Management,
             fill = Management)) +
  geom_line(#position = "stack",
    size = 0.7) +
  geom_point(#position = "stack",
    size = 1) +
  facet_wrap(.~scenSimpl2) +
  ylim(0,210) +
  ggtitle("") +
  xlab("Net present income\n(k€/ha)") + #
  ylab("Standing log volume\n(mean, m^3)") +
  scale_linetype_manual(values = c("solid",  "dotted")) +
  scale_color_manual(values = cbp1) +
  scale_fill_manual(values = cbp1) +
  labs(shape = "Management",
       color = "Management",
       linetype = "Management",
       fill = "Management") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "right",
        strip.background =element_rect(fill="white", color = NA))


# mean V log over time
# --------------------------
p.mean.V.log.line.time <-
  df %>% 
  filter( scenSimpl2 != "ALL") %>% #Management == "active" &
  group_by(scenSimpl2, 
           year, 
           Management) %>% 
  summarize(my_y = mean(V_strat_max_log))  %>%
  ggplot(aes(y = my_y, 
             x = year, 
             shape = Management,
             color = Management,
             linetype = Management,
             group = Management,
             fill = Management)) +
  geom_line(#position = "stack",
    size = 0.7) +
  geom_point(#position = "stack",
    size = 1) +
  facet_wrap(.~scenSimpl2) +
  ylim(0,210) +
  ggtitle("") +
  xlab("Time/n)") + #
  ylab("Standing log volume\n(mean, m^3)") +
  scale_linetype_manual(values = c("solid",  "dotted")) +
  scale_color_manual(values = cbp1) +
  scale_fill_manual(values = cbp1) +
  labs(shape = "Management",
       color = "Management",
       linetype = "Management",
       fill = "Management") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "right",
        strip.background =element_rect(fill="white", color = NA))



# Pulp volume
# ===================

p.mean.V.pulp.line.npi <-
  df %>% 
  filter( scenSimpl2 != "ALL") %>% #Management == "active" &
  group_by(scenSimpl2, 
           NPI, 
           Management) %>% 
  summarize(my_y = mean(V_strat_max_pulp ))  %>%
  ggplot(aes(y = my_y, 
             x = NPI, 
             shape = Management,
             color = Management,
             linetype = Management,
             group = Management,
             fill = Management)) +
  geom_line(#position = "stack",
    size = 0.7) +
  geom_point(#position = "stack",
    size = 1) +
  facet_wrap(.~scenSimpl2) +
  ylim(0,210) +
  ggtitle("") +
  xlab("Net present income\n(k€/ha)") + #
  ylab("Standing pulp volume\n(mean, m^3)") +
  scale_linetype_manual(values = c("solid",  "dotted")) +
  scale_color_manual(values = cbp1) +
  scale_fill_manual(values = cbp1) +
  labs(shape = "Management",
       color = "Management",
       linetype = "Management",
       fill = "Management") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "right",
        strip.background =element_rect(fill="white", color = NA))


# mean V pulp over time
# --------------------------
p.mean.V.pulp.line.time <-
  df %>% 
  filter( scenSimpl2 != "ALL") %>% #Management == "active" &
  group_by(scenSimpl2, 
           year, 
           Management) %>% 
  summarize(my_y = mean(V_strat_max_pulp ))  %>%
  ggplot(aes(y = my_y, 
             x = year, 
             shape = Management,
             color = Management,
             linetype = Management,
             group = Management,
             fill = Management)) +
  geom_line(#position = "stack",
    size = 0.7) +
  geom_point(#position = "stack",
    size = 1) +
  facet_wrap(.~scenSimpl2) +
  ylim(0,210) +
  ggtitle("") +
  xlab("Time/n)") + #
  ylab("Standing pulp volume\n(mean, m^3)") +
  scale_linetype_manual(values = c("solid",  "dotted")) +
  scale_color_manual(values = cbp1) +
  scale_fill_manual(values = cbp1) +
  labs(shape = "Management",
       color = "Management",
       linetype = "Management",
       fill = "Management") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "right",
        strip.background =element_rect(fill="white", color = NA))


# The log and pulp timber volume over NPI and time at one pot 
ggarrange(p.mean.V.pulp.line.npi, p.mean.V.pulp.line.time, 
          p.mean.V.log.line.npi, p.mean.V.log.line.time, 
          ncol = 2, nrow = 2,
          #widths = c(1, 1),
          common.legend = TRUE,
          align = c("hv"),
          legend="bottom",
          labels= "AUTO",
          hjust = -5,
          vjust = 3,
          font.label = list(size = 10, 
                            face = "bold", color ="black"))




# ---------------------------------------
# does standing timber volume predict wind risk???
# ---------------------------------------

# sample the data
# Sample  random rows:
df1 <- filter(df,  scenSimpl2 != "ALL")

set.seed(1)
# create the vector index of the rows to select from original large dataset
sample_row <- sample(1:nrow(df1), 100000, replace=F)  # 100000 is number of rows to subset
# subset the selected rows from the original large.df
df.sample <- df1[sample_row,]



# Make correlation plot: how does timber volume predict wind risk??
ggplot(df.sample, aes(x = V,
                      y = windRisk,
                      group = scenNumb,
                      color = scenNumb)) + 
  geom_point() +
  geom_smooth(method=lm, se=FALSE, formula = y ~ poly(x, 3)) + 
  facet_grid(scenSimpl2~Management)



# Risk based on pulp volume
df.sample %>% 
  filter(Management == "Active") %>% 
  ggplot(aes(x =  V_strat_max_pulp, #V,
             y = windRisk,
            # group = scenSimpl2,
             #color = scenSimpl2,
             fill = scenSimpl2
             )) + 
  #geom_jitter(size = 0.3, alpha = 0.5) #+
  #geom_smooth(method=lm, se=FALSE, formula = y ~ poly(x, 2)) %>% #+ 
  #facet_grid(scenSimpl2 ~ scenNumb)


ggplot(df.sample, aes(x = windRisk)) +
  geom_density()


# Risk based on  log volume
df %>%  # .sample
  filter(Management == "Active"  & scenSimpl2 != "ALL") %>% 
  ggplot(aes(x = V_strat_max_log,  #V,
             y = windRisk,
             group = scenSimpl2,
             color = scenSimpl2)) + 
  #geom_jitter(size = 0.3, alpha = 0.5) +
  #geom_smooth(method=lm, se=FALSE, formula = y ~ poly(x, 2)) #+ 
  geom_smooth() #+  # se=FALSE
  #facet_wrap(scenSimpl2 ~ .)


df %>%  # .sample
  filter(Management == "Active"  & scenSimpl2 != "ALL") %>% 
  ggplot(aes(x = V_strat_max_pulp,  #V,
             y = windRisk,
             group = scenSimpl2,
             color = scenSimpl2)) + 
  #geom_jitter(size = 0.3, alpha = 0.5) +
  geom_smooth(method=lm, se=FALSE, formula = y ~ poly(x, 3)) #+ 
 # geom_smooth() #+  # se=FALSE
#facet_wrap(scenSimpl2 ~ .)



# -------------------------------------
# Explain why???
# ----------------------------------
# Tree species
# tree height
# Time since thinning
# open-neighbour


# Tree species:
# -------------------------
# Spruce is teh most vulnerable: frequency of spruce

# Calculate the mean # of spruces!!

#p.mean.H_dom.npi <-
  df %>% 
  filter( scenSimpl2 != "ALL") %>% #Management == "active" &
  group_by(scenSimpl2, 
           NPI, 
           Management) %>% 
  filter(species == "spruce") %>% 
  tally() %>% 
  group_by(Management, NPI) %>% 
  summarize(mean.m = mean(n)) %>% 
  #summarize(my_y = mean(H_dom))  %>%
  ggplot(aes(y = mean.m, 
             x = NPI, 
             shape = Management,
             color = Management,
             linetype = Management,
             group = Management,
             fill = Management)) +
  geom_line(#position = "stack",
    size = 0.7) +
  geom_point(#position = "stack",
    size = 1) +
  facet_wrap(.~Management) +
#  ylim(0,300) +
  ggtitle("") +
  xlab("Net present income\n(k€/ha)") + #
  ylab("Tree height\n(mean, dm)") +
  scale_linetype_manual(values = c("solid",  "dotted")) +
  scale_color_manual(values = cbp1) +
  scale_fill_manual(values = cbp1) +
  labs(shape = "Management",
       color = "Management",
       linetype = "Management",
       fill = "Management") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "right",
        strip.background =element_rect(fill="white", color = NA))



# H Dom
# ---------------------
p.mean.H_dom.npi <-
  df %>% 
  filter( scenSimpl2 != "ALL") %>% #Management == "active" &
  group_by(scenSimpl2, 
           NPI, 
           Management) %>% 
  summarize(my_y = mean(H_dom))  %>%
  ggplot(aes(y = my_y, 
             x = NPI, 
             shape = Management,
             color = Management,
             linetype = Management,
             group = Management,
             fill = Management)) +
  geom_line(#position = "stack",
    size = 0.7) +
  geom_point(#position = "stack",
    size = 1) +
  facet_wrap(.~scenSimpl2) +
  ylim(0,300) +
  ggtitle("") +
  xlab("Net present income\n(k€/ha)") + #
  ylab("Tree height\n(mean, dm)") +
  scale_linetype_manual(values = c("solid",  "dotted")) +
  scale_color_manual(values = cbp1) +
  scale_fill_manual(values = cbp1) +
  labs(shape = "Management",
       color = "Management",
       linetype = "Management",
       fill = "Management") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "right",
        strip.background =element_rect(fill="white", color = NA))




p.mean.H_dom.time <-
  df %>% 
  filter( scenSimpl2 != "ALL") %>% #Management == "active" &
  group_by(scenSimpl2, 
           year, 
           Management) %>% 
  summarize(my_y = mean(H_dom ))  %>%
  ggplot(aes(y = my_y, 
             x = year, 
             shape = Management,
             color = Management,
             linetype = Management,
             group = Management,
             fill = Management)) +
  geom_line(#position = "stack",
    size = 0.7) +
  geom_point(#position = "stack",
    size = 1) +
  facet_wrap(.~scenSimpl2) +
  ylim(0,300) +
  ggtitle("") +
  xlab("Time/n)") + #
  ylab("Tree height\n(mean, dm)") +
  scale_linetype_manual(values = c("solid",  "dotted")) +
  scale_color_manual(values = cbp1) +
  scale_fill_manual(values = cbp1) +
  labs(shape = "Management",
       color = "Management",
       linetype = "Management",
       fill = "Management") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "right",
        strip.background =element_rect(fill="white", color = NA))






