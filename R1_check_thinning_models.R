

# React on review1:

# how to adress the concers by revuewer #1?
# how to correct the contribution of the effect of time since thinning on CCF/Rf??

# Check Pukkala et al. 2016: CCF reduces wind damage risk:


# reproduce the models
# the mean tree heights are way smalller in CCF then in RF??

# use 3 models to model the probability of tree suffering wind damage
# relevant for inner trees in a stands

# M1 - predictors: treatment and tree species; eg. effect of treatment
# M2 - predictors: + time since thinning
# M3 - prodictors: + characteristics of tree, stand and upwind dhelter stand BA


# M1 ----------------------------------------------------------------------

dd <- data.frame(L = c(0,1,0,0,0,0))
pWT = 1/(1+ exp(-(-5.932 + 4.525*L+1.517*D+1.966*S+4.323*M+6.074*R+5.190*H)))


# get different for positive, negative value
x= 1:10
y_p = 1/(1+exp(x))
y_n = 1/(1+exp(-x))

plot(x = x, y= y_p)  # positive x value leads to exponental decay curve
plot(x = x, y= y_n)  # negative x value leads to asymptotic increase 

# m1: Effect of treatment: get only D and S
pWT = 1/(1+exp(-(-5.932+1.517))) # get coefficient for D: 0.011
pWT = 1/(1+exp(-(-5.932+1.966))) # for S, S has higher pWT then D 0.018
pWT = 1/(1+exp(-(-5.932+6.074))) # for R: 0.58; yea, agree with teh paper
