
# Regression coefficients for critical wind speeds
# Based on Heinonen et al. paper

rm(list = ls())

library(ggplot2)


# Scots pine
# -----------------------
b0 = 7.601
b.taper = 2213.940
b.height_m = -0.629
b.height_relUpwHeight_sqr = -0.0000482  # squared!
b.RelUpwHeight_sqr = 0.00269            # squared!!
b.gapSize = -0.398

# Get a vector of coefficients for PINE
coef.pine = c(b0, b.taper, b.height_m, b.height_relUpwHeight_sqr, b.RelUpwHeight_sqr, b.gapSize)




# Norway spruce
# -----------------------
b0 = -0.761
b.taper = 2445.533
b.height_m = -0.412
b.height_relUpwHeight_sqr = -0.000103
b.RelUpwHeight_sqr = 0.00648
b.gapSize = -0.414

# Get a vector of coefficients for SPRUCE
coef.spruce = c(b0, b.taper, b.height_m, b.height_relUpwHeight_sqr, b.RelUpwHeight_sqr, b.gapSize)




# Birch
# ------------------------
b0 = 9.150
b.taper = 2142.711
b.height_m = -0.632
b.height_relUpwHeight_sqr = -0.0000479
b.RelUpwHeight_sqr = 0.00269
b.gapSize = -0.403

# Get a vector of coefficients for BIRCH
coef.birch = c(b0, b.taper, b.height_m, b.height_relUpwHeight_sqr, b.RelUpwHeight_sqr, b.gapSize)



# Taper ranges:
# 1:125 = 0.008
# 1:67 = 0.01492537


# Dummy predictors
# calculate taper
# structure the table
# calculate critical wind speed
# If the wind speed is more than CWS of a tree, tree will die

n = 5
df <- data.frame(dbh_cm = c(12,14,20,30,42),
                 height_m = c(20,20,20,28,30),
                 RelUpwHeight_sqr = seq(10,50,10)^2,         # const, range, squared!!!
                 gapSize = rep(5, n))                        # min gap size, constant
                 

# Calculate the taper - in cm
df$taper = df$dbh_cm/df$height_m/100

# remove the dbh, and not used further in formula
df<-subset(df, select=-c(dbh_cm))


# expad the dataframe to have all predictors as columns
# relUpwHeight is squared in the original table
df$b0 = rep(1,n)
df$height_RelUpwSqr <- df$height_m * df$RelUpwHeight_sqr


# Reorder of columns to follow the regression formula
df <- df[c("b0", 
           "taper", 
           "height_m", 
           "height_RelUpwSqr",
           "RelUpwHeight_sqr",  
           "gapSize")]


# multiply the vector of coefficients with the dataframe of predictors
# the column and vectors have to hev same order!!!
df.mult = sweep(df, 2, coef.pine, FUN="*")


# Calculate critical wind speeds 
# CWS = sum of the partial multiplication in a regression formula
df$cws = rowSums(df.mult)


# Check the plot

ggplot(data = df, 
       aes(x= sqrt(RelUpwHeight_sqr), y = cws)) +
  geom_line()






weight = 2213.940












alpha = 7.601
weight = 2213.940

length = -0.032
height = -0.629
size = 0.345
area = 3
  
  
