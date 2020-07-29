
# -----------------------
# Create spider plot
# -----------------------
# Is there differences between initail and final HSI values??


# Create sample data

df <- data.frame(year = rep(2016:2020, 3),
                 FM = rep(c("RF", "CCF", "GRT"), each = 5),
                 HSI = c(0.1,0.3,0.1,0.2,0.8,
                         0.5,0.3,0.1,0.3,0.1,
                         0.1,0.2,0.3,0.5,0.9))

# Calculate difference in HSI from the firts year
# Populate the new colum by values in first year
library(dplyr)
df %>% 
  group_by(FM) %>% 
  mutate(HSI_2016 = HSI[year == 2016]) %>% # fill value of the first year of the group to new column
  mutate(diff = HSI - HSI_2016) %>%      # # get the difference between current and initial HSI
  ggplot(aes(x = year,
             y = diff,
             color = FM)) + 
  geom_line() + 
  geom_hline(yintercept=0, 
             linetype="dashed", 
             color = "grey") + 
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1))
