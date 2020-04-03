
# ---------------------
#
# Susanne Suvanto coefficients
# 
# ---------------------

# calculate manually or just reuse teh formula??


# =========================
# fake data
# Check if the model works???
# ---------------------

set.seed(42)

row.num = 80

species         <- factor(rep(c("pine", "spruce", "other"),
                              each = row.num), 
                          levels = c("pine", "spruce", "other"))
height          <- c(runif(row.num, min = 10, max = 200),
                     runif(row.num, min = 0, max = 100),
                     runif(row.num, min = 30, max = 150))
time_thinning   <-  factor(sample(c("0-5", "6-10", ">10"),
                                  length(species), replace = TRUE), 
                           levels = c("0-5", "6-10", ">10"))
windSpeed       <- runif(length(species), min = 10, max = 30)
open_stand      <- ifelse(rbinom(length(species), 1, 0.9), "FALSE", "TRUE") 
soilType        <- sample(c("mineral coarse", "mineral fine",
                            "organic"),
                          length(species), replace = TRUE)
soilDepthLess30 <- ifelse(rbinom(length(species), 1, 0.5),
                          "FALSE","TRUE")
siteFertility   <- factor(sample(c("poor", "fertile"),
                                 length(species),
                                 replace = TRUE),
                          levels = c("poor", "fertile"))
tempSum         <- runif(length(species), min = 6, max = 16)
#damageDensity  <- rep(0, length(species))

wind_damage     <- rbinom(length(species), 1, 0.4)


# put data together
df<-data.frame(species, 
               height,
               time_thinning,
               windSpeed,
               open_stand,
               soilType,
               soilDepthLess30,
               siteFertility,
               tempSum,
               wind_damage)



# convert my categorial variables into binary classes
# create vector of colnames with categorical variables
categVars <- c("species", 
               "time_thinning", 
               "open_stand",
               "soilType", 
               "soilDepthLess30",
               "siteFertility")


# get suvanto coeffuiicients
# replace them in the fake model
# calculate predicted values * ar the same as manual values?

fake.m <- glm(formula = wind_damage ~ species + log(height)  +  
                time_thinning + 
                log(windSpeed) + open_stand + soilType +
                soilDepthLess30 + 
                siteFertility + tempSum +
                log(height):species, 
              data = df, 
              family = "binomial") # family = binomial(link = "logit")



# ----------------------------
# Get Suvanto's coefficients
# ---------------------------


# Convert logit to probabilities
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}




# Suvanto's coefficients (more accurate from Susanne code): 20 digits
intercept                    = - 14.690374506245104769
b1.spec.spruce               = - 8.494158565180855547
b2.spec.other                = - 9.314355152502169943
b3.log.height                = + 1.660897636823469137   # log
b4.last_thinning.6.10        = - 0.298186071853962231
b5.last_thinning.over.10     = - 0.844019963540904472
b6.log.wind                  = + 0.748957880201017501   # log
b7.open_stand_border         = + 0.310378186345018792
b8.soil_min.fine             = - 0.355681075669793900
b9.soil_organic              = - 0.216004202249671151
b10.soil_depth.less.30cm      = + 0.214100256449853671
b11.site_fertility            = - 0.425096042510456240
b12.temperature_sum           = + 0.095854694562656148
b13.spec.spruce.X.log.height  = + 1.634359050870280550 
b14.spec.other.X.log.height   = + 1.624775941830151726


# Put coefficients in a vector, to replace the coefficients in a formula
suvantoCoeffs<-c(intercept, 
                 b1.spec.spruce,
                 b2.spec.other,
                 b3.log.height,
                 b4.last_thinning.6.10,
                 b5.last_thinning.over.10,
                 b6.log.wind,
                 b7.open_stand_border,
                 b8.soil_min.fine,
                 b9.soil_organic,
                 b10.soil_depth.less.30cm,
                 b11.site_fertility,
                 b12.temperature_sum,
                 b13.spec.spruce.X.log.height,
                 b14.spec.other.X.log.height)


# -------------------------------
# Replace the dummy coefficients 
#    by the real ones
# -------------------------------
# First, copy the model
windRisk.m <- fake.m


# replace coefficients:
windRisk.m$coefficients<-suvantoCoeffs


# create new data, just change tree height variable
df2<-df
df2$height<- c(runif(row.num, min = 10, max = 200),
               runif(row.num, min = 0, max = 300),
               runif(row.num, min = 30, max = 150))


# Normally predicted values - converted from logit values
df2$predicted<-predict.glm(windRisk.m, 
                           df2, 
                           type="response")


# -------------------------------
# Manually calculate y
# require steps:
#    - convert cegorical variables to binary
#    - correctly order teh columns & create interactions
#    - multiply the columns by coefficients
#    - sum up the rows
#    - convert sum(row) values to logit to get the probability estimation
# -------------------------------

# convert my categorial variables into binary classes
# create vector of colnames with categorical variables
categVars <- c("species", 
               "time_thinning", 
               "open_stand",
               "soilType", 
               "soilDepthLess30",
               "siteFertility")


library(fastDummies)

df.bin <- fastDummies::dummy_cols(df,
                                  select_columns = categVars, # only categorical
                                  remove_first_dummy = TRUE)  # remove reference category

# remove the original variables and observed value of damages
df.bin<-df.bin[ , !(names(df.bin) %in% c(categVars,
                                         "wind_damage"))]


# complete the dataframe by interactions 
# to have the same amount of columns as number of coefficients
# add logarithm in a formula
# add columnf for intersectp => fill with 1
df.bin$interc <- 1

df.bin$log_height <- log(df.bin$height)
df.bin$log_Wspeed <- log(df.bin$windSpeed)

# add interactions
df.bin$spec.spruce.X.log.height <- df.bin$species_spruce * df.bin$log_height
df.bin$spec.other.X.log.height  <- df.bin$species_other  * df.bin$log_height


# to put it in correct order
colnames.ordered<-c("interc",
                    "species_spruce",
                    "species_other",
                    "log_height",
                    "time_thinning_6-10", 
                    "time_thinning_>10",
                    "log_Wspeed",
                    "open_stand_TRUE",
                    "soilType_mineral fine",
                    "soilType_organic",
                    "soilDepthLess30_TRUE",
                    "siteFertility_fertile",
                    "tempSum",
                    "spec.spruce.X.log.height",
                    "spec.other.X.log.height" )

# order the dataframe to corresponds columnwise to coefficients
# keep only specified columns
df.ord<-df.bin[colnames.ordered]


# Multiply the dataframe columns by vector of coefficients

# calculate partial df 
# the final column need to be summed up
part.df <- sweep(df.ord, 2, suvantoCoeffs, "*")

# sum by rows and add intercept value
df.ord$pred.manual <- logit2prob(rowSums(part.df))

# Convert logit to probabilities
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}



# sum by rows and add intercept value
df.ord$pred.manual <- logit2prob(rowSums(part.df))


# Calculate teh probability based on model: 
# the manual calculation (with logits) and this should be the same!!
# Calcutate probability values given the example data
df.ord$predict.wind <- predict.glm(windRisk.m, 
                                   df, 
                                   type = "response")


# Compare manual probability estimation and 
# calculated using the model
df.ord$diff<- round(df.ord$pred.manual,6) - round(df.ord$predict.wind,6)

range(df.ord$diff)

# ----------------------
# conclusions! easier to recreate model and just replace the coefficeint for it, 
# no need to convert to binary, get logits etlc...
# need to check it on one more data??

