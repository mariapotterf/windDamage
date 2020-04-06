

# Import the calculate open_edge datasets
# Calculate wind risk for management regimes
# create ggplots & main conclusions

# ===============================

























# -------------------------------

# !!! Add missing columns: - create fake data!!!!
# variables used fromSIMO instead from rasters:
# will be later replaced directly from SIMO
#
# SC - identify fetility class
# SOIL_CLASS - identify coarsiness of teh soil ^ soil_depth <30 TRUE/FALSE
# PEAT - identify where is peat
# 
# -----------------------------------
stand.merged$time_thinning <- factor(sample(c("0-5", "6-10", ">10"),
                                            nrow(stand.merged), replace = TRUE), 
                                     levels = c("0-5", "6-10", ">10"))

stand.merged$soiltyp <- factor(sample(c("mineral coarse", 
                                        "mineral fine",
                                        "organic"), nrow(stand.merged), replace = TRUE),
                               levels = c("mineral coarse", 
                                          "mineral fine",
                                          "organic")) 

# replace NA values - due to centroids not overlapping with raster
stand.merged$slFrtlC <- replace_na(stand.merged$slFrtlC, "poor")

# Soil depth: TRUE & FALSE
# soil depth < 30: 1 = TRUE, 0 = FALSE
stand.merged$solDpth <- factor(ifelse(rbinom(nrow(stand.merged), 1, 0.5),
                                      "FALSE","TRUE"))


# Subset columns crucial for glm()
my.cols.glm <- c("standid",
                 "area",
                 "year",
                 "species",
                 "H_dom", 
                 "time_thinning",
                 "windSpd",
                 "soiltyp",         
                 "solDpth",
                 "slFrtlC", 
                 "avgTemp")

# Replace the species values
# seems that these data have only pine???
# mainGRp is land type == forest = 1
# need to use MAIN_SP from simulated data !! 
# or mntrspc

stand.merged<-
  stand.merged %>% 
  mutate(species = case_when(mntrspc == 1 ~ "pine",
                             mntrspc == 2 ~ "spruce",
                             TRUE ~ "other")) %>% 
  mutate(H_dom = replace_na(H_dom, 0.01)) %>%   # no possible to get log(0)
  dplyr::select(my.cols.glm)  %>%      # select columns 
  mutate_if(is.character, as.factor)   # convert all characters to factor




# Change column names to correspond glm() 
colnames(stand.merged)[colnames(stand.merged) == 'windSpd'] <- 'windSpeed'
colnames(stand.merged)[colnames(stand.merged) == 'soiltyp'] <- 'soilType'
colnames(stand.merged)[colnames(stand.merged) == 'solDpth'] <- 'soilDepthLess30'
colnames(stand.merged)[colnames(stand.merged) == 'slFrtlC'] <- 'siteFertility'
colnames(stand.merged)[colnames(stand.merged) == 'avgTemp'] <- 'tempSum'



# subset just one year
stand.merged.2016 <- 
  stand.merged %>% 
  filter(year == 2016) 


# Calculate open edge:
stand.merged.2016<- findOpenEdge_sf(stand.merged.2016, 
                                    H_dom, 
                                    distance = 40, 
                                    pixel.width = 16)




colnames(stand.merged.2016)
nrow(stand.merged.2016)


# -----------------------------------
#
# Reorganize the input data to fit Suvanto model's requirement
# 
# -----------------------------------
stand.merged.2016$H_dom = stand.merged.2016$H_dom * 10       # H_dom is Suvanto is in decimeters, in SIMO in meters 
# H_dom is Suvanto is in decimeters, in SIMO in meters 

# Correct order of variables, factors & levels
# need to have same names of columns?? YES
# when data are sourced (source()), they are all available in my script


# Convert spatial file to NULL geometry = normal dataframe
my.df<-stand.merged.2016
st_geometry(my.df) <- NULL  # get rid of geometry


# Check the factors and levels in the dataset:
# keep only columns necessary for glm
keep <- c("species", 
          "H_dom", 
          "time_thinning", 
          "windSpeed", 
          "open_edge",
          "soilType",
          "soilDepthLess30", 
          "siteFertility",
          "tempSum")

str(my.df)


# subset only needed columns:
df.sub<-my.df[, keep]

# Add fake wind damage:
#df.sub$wind_damage_fake  <- rbinom(nrow(df.sub), 1, 0.4)



# -----------------------------------------
# Correct the factor levels 
#   for categoric variables
# ------------------------------------
df.sub$species          <- factor(df.sub$species, 
                                  levels = c("pine", 
                                             "spruce", 
                                             "other"))

df.sub$time_thinning    <- factor(df.sub$time_thinning, 
                                  levels = c("0-5", 
                                             "6-10", 
                                             ">10"))
df.sub$open_edge        <- factor(df.sub$open_edge,
                                  levels = c("FALSE", 
                                             "TRUE"))
df.sub$soilType         <- factor(df.sub$soilType,
                                  levels = c("mineral coarse", 
                                             "mineral fine",
                                             "organic"))
df.sub$soilDepthLess30  <- factor(df.sub$soilDepthLess30, 
                                  levels = c("FALSE", 
                                             "TRUE"))
df.sub$siteFertility    <- factor(df.sub$siteFertility,
                                  levels = c("poor", 
                                             "fertile"))


# ------------------------------------------
# Calculate predicted values for wind risk 
# ------------------------------------------
# For temperature sum, I have single value: not variabilit over the landscape: 1299.273
# try to inclrease the variability??

# if teh
df.sub$tempSum <- runif(nrow(df.sub), 
                        min = 12, 
                        max = 12)#*100


df.sub$windDamagePred <- predict.glm(windRisk.m,
                                     df.sub,
                                     type="response")

range(df.sub$windDamagePred)
# all wind damage prediction is 1!!!
# is iot becauise of low variability in fake values??


# compare msimulated input data with fake input data that allow the range 0-1

df$predicted <- predict.glm(windRisk.m,
                            df,
                            type="response")

# Try to change temperature sum???
runif(length(species), min = 6, max = 16)







library(fastDummies)

















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


# calculate partial df 
# the final column need to be summed up
part.df <- sweep(df.ord, 2, suvantoCoeffs, "*")

# sum by rows and add intercept value
df.ord$pred.manual <- logit2prob(rowSums(part.df))



# ---------------------------

# Can I directly import Suvanto's model?
# Thaen I can use just predict() function, 
# no need to split categorical data into binary

# how to know if my probability values are correctly calculated?
# try to subset Suvanto's raw data and again evaluate?
# originally, I was comparing predict() outcomes with manually calculated y values
# 







# -----------------------------------
# my data
# ---------------------------------

library(fastDummies)


# select categorical data
categVars <- c("species", 
               "time_thinning", 
               "open_edge",
               "soiltyp", 
               "solDpth",
               "slFrtlC")


# Define correct level order: 
df$species   <- factor(df$species,   levels = c("pine", "spruce", "other"))
df$solDpth   <- factor(df$solDpth,   levels = c("FALSE", "TRUE"))
df$open_edge <- factor(df$open_edge, levels = c("FALSE", "TRUE"))
df$slFrtlC   <- factor(df$slFrtlC,   levels = c("poor", "fertile"))

# all my stands are just pine!!! this makes different table output
df.bin <- fastDummies::dummy_cols(df,
                                  select_columns = categVars, # only categorical
                                  remove_first_dummy = TRUE)  # remove reference category

# remove the original variables and observed value of damages
df.bin<-df.bin[ , !(names(df.bin) %in% c(categVars))]


# Complete missing columns:
#  - intercept
#  - logarithms
#  - interactions
# Need to have the same number of columns as coefficients
df.bin$interc     <- 1
df.bin$log_height <- log(df.bin$H_dom)
df.bin$log_Wspeed <- log(df.bin$windSpd)


# add interactions
df.bin$spec.spruce.X.log.height <- df.bin$species_spruce * df.bin$log_height
df.bin$spec.other.X.log.height  <- df.bin$species_other  * df.bin$log_height


names(df.bin)


# ---------------------------
#     Order the columns: 
# ---------------------------

# to put it in correct order
colnames.ordered<-c("interc",
                    "species_spruce",
                    "species_other",
                    "log_height",
                    "time_thinning_6-10", 
                    "time_thinning_>10",
                    "log_Wspeed",
                    "open_edge_TRUE",
                    "soiltyp_mineral fine",
                    "soiltyp_organic",
                    "solDpth_TRUE",
                    "slFrtlC_fertile",
                    "avgTemp",
                    "spec.spruce.X.log.height",
                    "spec.other.X.log.height" )


# order the dataframe to corresponds columnwise to coefficients
# keeps only specified columns
df.ord<-df.bin[colnames.ordered]


# ---------------------------------
#      Get coefficients:
# ---------------------------------



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



# Multiple the dataframe by vector
# calculate partial df 
# the final column need to be summed up
part.df <- sweep(df.ord, 2, suvantoCoeffs, "*")


# Function: convert logit to probability
# Convert logit to probabilities
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}


# Convert sum of `y.logit` values to probability values
# sum by rows and add intercept value
df.ord$pred.manual <- logit2prob(rowSums(part.df))

#####

###!!!! predicted values are all equal 1 !!! What's is the problem???




