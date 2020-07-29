
# ---------------------
# Make line plots
# ---------------------

# 
rm(list = ls())

# , eval = FALSE
library(data.table)
library(dplyr)
library(raster)
library(ggplot2)
library(sf)
library(stringr)


theme_set(theme_classic())


# Read datasets:
df.all <- fread("C:/MyTemp/myGitLab/windDamage/output/df_sim_windRisk.csv")

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
names(df.geom) <- c("standid", "geometry")
df.geom$area <- st_area(df.geom)
#df.geom <- subset(df.geom, !standid %in% stands.remove)
df.geom <- subset(df.geom, standid %in% unique(df.all$id))
df.geom$id <- as.numeric(as.character(df.geom$standid ))


# Split df.all data into simpleScenario and SA proportion (0-20):
# Split the string with numbers and characters into string and numbers:
df.all <- 
  df.all %>% 
  mutate(twoRegm = case_when(avohaakut == "SA" ~ "SA",
                             avohaakut != "SA" ~ "no_SA"))



# -------------------------------------------
# How does the % of SA (0-20) affect wind risk?
# -----------------------------------------
# Calculate as the sum by stand
windsum.agg <- aggregate(windRisk ~ id + scenSimpl2 + scenNumb, df.all, sum)


# Check how my windRisk historgram looks like?
hist(windsum.agg$windRisk)

# Make density plot
ggplot(windsum.agg, 
       aes(windRisk,
           fill = factor(scenSimpl2),
           group = factor(scenSimpl2))) +
 # geom_histogram()
  geom_density(alpha = 0.5) +
  theme(legend.position = "bottom")
  


# Make line plot: how to fit the function????

ggplot(windsum.agg, aes(x = scenNumb,
                        y = windRisk,
                        color = factor(scenSimpl2),
                        group = factor(scenSimpl2))) +
  #geom_jitter(alpha = 0.5, 
  #            size = 0.1) +
  geom_smooth(method = "gam",
              formula = y ~ s(x),
              #formula = y ~ splines::bs(x, 3),
              #formula = y ~ a*x^(-b),
              #formula = y~ a*exp(-b),
              #method.args = list(start= c(a=5, b=20), #c(a=20, b=0.01), 
               #                  control=nls.control(maxiter=200)),   
              #se = F,
              size = 0.5,
              show.legend = T)  +
  xlab("landscape intensity") + 
  ylab("sum of wind risk (%)") + 
  labs(color= "Scenarios") +
  theme(legend.position = "bottom")


# ------------------------------------------
# How does H_dom relates to windthrow??
# ----------------------------------------
# Calculate as the sum by stand

# Sample  random rows:

sample_row <- sample(1:nrow(df.all), 100000, replace=F)
df.sample <- df.all[sample_row,]


# Make scatter plot H_dom vs. wind risk
p.H_dom <- 
  # Make scatter plot D_gm vs. wind risk
  ggplot(df.sample, aes(x = H_dom,
                        y = windRisk,
                        color = factor(scenSimpl2))) +
  #geom_point() +
  geom_smooth(method = "nls",
              formula = y ~ a*x^(-b),
              method.args = list(start= c(a=0.1, b=0.01), #c(a=20, b=0.01), 
                                 control=nls.control(maxiter=200)),   
              se = F,
              size = 0.9) +
  facet_wrap(.~year) +
  theme(legend.position = "bottom") 


  
# Make scatter plot BA vs. wind risk
p.BA <- 
  # Make scatter plot D_gm vs. wind risk
  ggplot(subset(df.sample, !anyNA(BA)), 
         aes(x = BA,
                        y = windRisk,
                        color = factor(scenSimpl2))) +

  geom_smooth(method = "lm", #,
              formula = y ~ log(x)) +
              #method.args = list(start= c(a=10.1, b=10.01), #c(a=20, b=0.01), 
                           #      control=nls.control(maxiter=200)),   
             # se = F,
             # size = 0.9,
             # show.legend = T)  
  theme(legend.position = "bottom")


# Make scatter plot BA vs. wind risk
p.V <- 
  # Make scatter plot D_gm vs. wind risk
  ggplot(df.sample, aes(x = V,
                        y = windRisk,
                        color = factor(scenSimpl2))) +
  
  geom_smooth(method = "lm")+
  theme(legend.position = "bottom")#,
# formula = y ~ a*x^(-b),
#method.args = list(start= c(a=10.1, b=10.01), #c(a=20, b=0.01), 
#      control=nls.control(maxiter=200)),   
# se = F,
# size = 0.9,
# show.legend = T)  


# Make scatter plot BA vs. wind risk
p.D_gm <- 
  # Make scatter plot D_gm vs. wind risk
  ggplot(df.sample, aes(x = D_gm,
                        y = windRisk,
                        color = factor(scenSimpl2))) +
  
  geom_smooth(method = "lm") +#,
  theme(legend.position = "bottom")


library(gridExtra)
grid.arrange(p.H_dom, p.BA, p.V,  p.D_gm, nrow = 2)










  
# Make scatter plot D_gm vs. wind risk
ggplot(df.sample, aes(x = H_dom,
                      y = windRisk,
                      color = factor(scenSimpl2))) +
  #geom_point() +
  geom_smooth(method = "nls",
              formula = y ~ a*x^(-b),
              method.args = list(start= c(a=0.1, b=0.01), #c(a=20, b=0.01), 
                                 control=nls.control(maxiter=200)),   
              se = F,
              size = 0.9,
              show.legend = T)  


windsum.agg <- aggregate(windRisk ~ id + scenSimpl2 + scenNumb, df.all, sum)


# Check how my windRisk historgram looks like?
hist(windsum.agg$windRisk)

# Make density plot
ggplot(windsum.agg, 
       aes(windRisk,
           fill = factor(scenSimpl2),
           group = factor(scenSimpl2))) +
  # geom_histogram()
  geom_density(alpha = 0.5) +
  theme(legend.position = "bottom")



# Make line plot: how to fit the function????

ggplot(windsum.agg, aes(x = scenNumb,
                        y = windRisk,
                        color = factor(scenSimpl2),
                        group = factor(scenSimpl2))) +
  #geom_jitter(alpha = 0.5, 
  #            size = 0.1) +
  geom_smooth(method = "gam",
              formula = y ~ s(x),
              #formula = y ~ splines::bs(x, 3),
              #formula = y ~ a*x^(-b),
              #formula = y~ a*exp(-b),
              #method.args = list(start= c(a=5, b=20), #c(a=20, b=0.01), 
              #                  control=nls.control(maxiter=200)),   
              #se = F,
              size = 0.5,
              show.legend = T)  +
  xlab("% of SA over landscape") + 
  ylab("sum of wind risk (%)") + 
  labs(color= "Scenarios") +
  theme(legend.position = "bottom")



