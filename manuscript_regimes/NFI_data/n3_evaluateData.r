# ----------------------------------
# Make plots:
# ----------------------------------


# Read data and make plots
# data form N-S gradient and climate change

# Make working example for no climate changes; then calculate values for CC scenario 
rm(list = ls())


setwd("C:/MyTemp/myGitLab/windDamage")

# Read libraries
library(dplyr)
library(tidyr)
library(data.table)
library(stringr)

# Correct order of variables, factors & levels
# need to have same names of columns?? YES
# when data are sourced (source()), they are all available in my script
source("C:/MyTemp/myGitLab/windDamage/myFunctions.R")




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
inPath = "C:/MyTemp/2021_WindRisk_biodiversity"
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
my_ids  <- unique(df.ls[[1]]$id)
sub_ids <- sample(my_ids, 1000) 

# convert from integer64 to numeric:
sub_ids <- as.numeric(sub_ids)

# subset teh same stands from each regime
#df.ls2 <- list()
df.ls2 <- lapply(df.ls, function(df) df %>% 
         filter(id %in% sub_ids))

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
lapply(df.ls2, function(df) length(unique(df$id))) # the final number varies between regimes


# keep only needed columns
df.ls2 <- lapply(df.ls2, function(df) df %>% 
         dplyr::select(cl_keep))


# Merge data together -----------------------------------------------
df.out <- do.call(rbind, df.ls2)

rm(df.ls)

# Classify climate change
df.out <- df.out %>% 
  mutate(climChange = case_when(
    grepl("RCP0", name)  ~ "no",
    grepl("RCP45", name) ~ "cc45",
    grepl("RCP85", name) ~ "cc85"))


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
                            levels = c("no", "cc45", "cc85"))


# Test hypotheses:  --------------------------------------------------------

# select only regimes to test my hypotheses: RF, adaptation, w/wo thinning, CCF
# keep climate change
# biodindicator

library(ggplot2)

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
# wind damage risk  vs individual species -------------------------

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
            mean_SQR     = mean(SIBERIAN_FLYING_SQUIRREL, na.rm = T)
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
         control_SQR     = mean_SQR[match('BAU', regime)],
         p_change_SQR    = mean_SQR /control_SQR * 100 - 100) 

  
 # plot XY scatter plots by regimes and fill with species

pt_details <- function() {
  list(
    geom_point(),
    ylab(''),
    xlab(''),
    geom_vline(xintercept = 0, color = "grey", lty = "dashed"), 
    geom_hline(yintercept = 0, color = "grey", lty = "dashed"),
    theme(axis.title  = element_text(size = 10, face="plain", family = "sans"),
          axis.text.x = element_text(angle = 90, vjust = 0.5, face="plain", size = 9, family = "sans"),
          axis.text.y = element_text(face="plain", size = 9, family = "sans"),
          legend.position = "right",
          strip.background =element_rect(fill="white", 
                                         color = NA))
    )
}


p1 <- 
  df.species.means %>% 
  ggplot(aes(x = p_change_CAPER,
             y = p_change_risk,
             color = regime)) + 
    ggtitle("a) capercaillie\n") +
    pt_details() +
  ylab("wind damage\nrisk change")
  

p2 <- df.species.means %>% 
  ggplot(aes(x = p_change_HAZ,
             y = p_change_risk,
             color = regime)) +
  pt_details() +
  ggtitle("b) hasel grouse\n")

p3 <- df.species.means %>% 
  ggplot(aes(x = p_change_THREE,
             y = p_change_risk,
             color = regime)) +
  pt_details() +
  ggtitle("c) three toed\nwoodpacker")

p4 <- df.species.means %>% 
  ggplot(aes(x = p_change_LESSER,
             y = p_change_risk,
             color = regime)) +
  pt_details() +
  ggtitle("d) lesser spotted\nwoodpecker") +
  ylab("wind damage\nrisk change")
 
p5 <- df.species.means %>% 
  ggplot(aes(x = p_change_TIT,
             y = p_change_risk,
             color = regime)) +
  pt_details() +
  ggtitle("e) long tailed\ntit")

p6 <- df.species.means %>% 
  ggplot(aes(x = p_change_SQR,
             y = p_change_risk,
             color = regime)) +
  pt_details() +
  ggtitle("f) siberian flying\nsquirrel")


windows(7,4)
ggarrange(p1,p2,p3,p4,p5,p6, 
          widths = c(1.05,1, 1, 1.05,1,1),
             ncol = 3,nrow = 2,
             common.legend = TRUE,
          legend = 'right')
  
  
  
  
  
  

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




