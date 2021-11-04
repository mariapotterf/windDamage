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
inPath = "C:/MyTemp/myGitLab/windDamage/manuscript_regimes"
inFolder = "output_CC"
#outFolder = 'output_CC'

df.names = list.files(paste(inPath, inFolder, sep = "/"), pattern = ".csv$")

# Read dataframes
df.ls <- lapply(df.names, function(name, ...) data.table::fread(paste(inPath, inFolder, name,  sep = "/"),  # 
                                                           data.table=FALSE, stringsAsFactors = FALSE))


# lapply(df.ls, function(df) unique(df$siteName))


# Modify the structure of df ------------------------------------------------------
# categories for modifications:
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

# Consider GRT and CCF and thinning as as adaptation?
df.ls2 <- lapply(df.ls1, function(df, ...) {
  df1 <- df %>%
    mutate(adapt = case_when(
      grepl('BAUwoT', regime) ~ 'noThin',
      grepl('_m5' , regime)  ~ 'shorten',
      grepl('_m20', regime)  ~ 'shorten',
      grepl('_5'  , regime)  ~ 'extended',
      grepl('_10' , regime)  ~ 'extended',
      grepl('_15' , regime)  ~ 'extended',
      grepl('_30' , regime)  ~ 'extended',
      grepl("CCF_", regime)  ~ "CCF",
      grepl("GTR", regime)  ~ "GTR",
      TRUE~ 'normal')) 
  return(df1)
}
)



# Classify the type of regime, type of adjustement (extension or shortening) ----------------
# and change in time (how many years) 
df.ls3 <- lapply(df.ls2, function(df, ...)  {
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
  
# Add dominant regime ---------------------------------------------------------

df.ls4 <- lapply(df.ls3, function(df, ...)  {
  df1 <- df %>%
      mutate(regime = replace(regime, regime == "BAU", "BAU_")) %>% # replace BAU to facilitate further classification
      mutate(mainType = case_when(
        grepl("BAUwGTR"     , regime) ~ "GTRwT",
        grepl("BAUwT_GTR"   , regime) ~ "GTRwT",
        grepl("BAU_"        , regime) ~ "BAUwT",
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


# Merge data together -----------------------------------------------
df.out <- do.call(rbind, df.ls4)

# remove the 2015 year
df.out <- df.out %>% 
  filter(year != 2015)

# add Geo gradient: -------------------
df.out <- df.out %>% 
  mutate(geo_grad = case_when(
    grepl("Korsnas", siteName)   ~ "center",
    grepl("Raasepori", siteName) ~ "south",
    grepl("Simo", siteName)      ~ "north" ))

 

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




#  Inspect CCF: does shortenig have more frequent thinnings? ---------------------------------
# shorter CCF = more frequent thinnings
df.out %>% 
  filter(mainType == "CCF" & geo_grad == "center" & id == "28248101" & climChange == "no") %>%
  dplyr::select(year, THIN, regime) %>% 
  filter(!is.na(THIN)) %>% 
  group_by(regime) %>% 
  arrange(regime) %>% 
  ggplot(aes(x = year,
             y = THIN,
             color = regime)) +
  geom_point()+
  facet_grid(regime~.)


  # distinct(climChange) 
  ggplot(aes(x = year,
             y = BA,
             col = regime)) +
  geom_line() +
  scale_x_continuous(breaks = seq(2020, 2100, by = 10)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) #+


  
  
  # DOes BAUwGTR have thinnings? YES !----------------------------------------
  df.out %>% 
    filter(regime == "BAUwGTR") %>% 
    distinct(id) 
  
  
  df.out %>% 
    filter(regime == "BAUwGTR" & id == 29008597) %>% 
    #distinct(id) # 29010995
    dplyr::select(year, THIN, V)
  
  # BAUwGTR has thinings! very little but have them. Maybe I can calculate how many thinnings each regimes have and how does this 
  # changes with clim Change?
  
  
  







  
# Test hypotheses:  --------------------------------------------------------

# select only Korsnas - do not put there N-S gradient
# select only regimes to test my hypotheses: RF, adaptation, w/wo thinning, CCF
# keep climate change
# for old trees: use indicator N_where_D_gt_40, COMBINED_HSI
# check for clarity just the last 30 years

# H1: 
# We suggest that shorter rotation length will reduce wind damage risk
# and conflict with biodiversity (HSI, old trees). 
# This effect will decrease with more sever climate change.

# all have thinings if not specified otherwise 

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

# keep only regimes of interest:
# BAU: 
# Extended
# Shortened
# no Thinning
# GTR
# CCF"

my_regimes <- c(# Benchmark:
             "BAU_","BAUwT",
                # extended:
             "BAU_5","BAUwT_5", 
             "BAU_10","BAUwT_10", 
             "BAU_15","BAUwT_15", #"BAUwT_30","BAU_30"  # extended
                # shortened:
             "BAU_m5", "BAUwT_m5", # shortent
                # CCF
             "CCF_2",
                # no thinning
             "BAUwoT",
                # Green tree retention
             "BAUwGTR","BAUwT_GTR")

# not selected egimes:
#[1]  "SA_DWextract"
#     "BAUwoT_m20"                 
#[13] "BAUwoT_10"
  # CCF_1, CCF_3, CCF_4



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




