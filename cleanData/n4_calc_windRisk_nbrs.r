
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


# how to filter the simulated data to get the list 
# maybe just paste central_id and nbrs_id together???

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
  filter(Management == "Set Aside" & Management_nbrs == "Active") %>% 
  filter(H_dom > 160 & H_dom_nbrs > 160)


# Plot wind risk in in set aside
# plot wind risk in surroundings neighbors


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





# PLot : 
# # Why is SA in RF always having lower wind risk? -----------------------------

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
  
  
  

