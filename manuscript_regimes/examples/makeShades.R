# Make a different colur of shade by group

#library(dplyr)
library(tidyr)
library(ggplot2)
#library(dplyr)

# example for shaded line plot
dd1 <- data.frame(year = c(1:5),
                  grp = rep(c("a", "b", "d"), each = 5),
                  vals = c(5, 5.2, 5.6, 5.8, 6,
                           5, 4.9, 4.8, 4.7, 4.2,
                           5, 4.8, 4.4, 4,   3),
                  modif = rep('no', each = 15))

dd2 <- dd1
dd2$vals = dd1$vals*0.8
dd2$modif = 'yes'

# create a new factor


dd <- rbind(dd1, dd2)
dd$comb = paste(dd$modif, dd$grp, sep = "_")


dd %>% 
  #ungroup() %>% 
  ggplot(aes(x = year)) +
  ggplot2::geom_ribbon(
    data = ~ tidyr::pivot_wider(., names_from = grp,
                         values_from = vals),
    aes(ymin = d, #min(c), 
        ymax = a ,#, #max(a), fill = modif
        fill = modif
        )) +
  ylim(0,6.5) +
  geom_line(aes(y = vals,color = modif, linetype = grp),  # color = interaction(modif, grp)
            lwd  = 1.5)  +
  
  theme_bw() #+


