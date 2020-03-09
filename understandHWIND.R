

# 
# Peltola Critical wind speeds
# 1999


# ---------------------------------
#
# First, understand the taper parameters:
# 
# -------------------------


# Calculate tree taper:
#d(h)^2 = D^2*((H_max-h)/(H_max-hb))^1.6

# What is the stem diameter at specific tree height???
D_dbh = 10   # diameter at breast height (cm)
H_max = 10   # total height of the tree  (m)
h_dbh    = 1.3  # breast height
taper = D_dbh/(100*H_max)

# Create a vector of the  tree segments
h = h_dbh + c(1: (H_max-h_dbh))  # segments: length = 1m

# predict the diameter at specific height
d_h = sqrt(D_dbh^2*((H_max-h)/(H_max-h_dbh))^1.6)

plot(d_h ~ h, type = "l")





# -----------------------------

# Interpret tree taper as ratio?
# --------------------
taper = 1/100

h_dbh = 1.3
D_dbh = 10 
H_max = 10

# Create a vector of the  tree heights 
h = h_dbh + c(1: (H_max-h_dbh))  # segments: length = 1m

 


h = 100*c(1:10)
d_h = c(10:1)



d_h <- D_dbh - taper*(h-h_dbh)*100

# put height in the same units: centimeters
# what is the diameter at specific heights? what is teh volume of unique 
# tree segments?
20/2000
10/2000
50/2000



2*50*0.001


# -------------------------------------------------
#
# Which trees will die given wind speed of 12 m?/s
#
#      Understand the CWS model
#
# -------------------------------------------------

# Forces affecting the tree
# wind force: calculated fr the each point on the tree at specific height
Cd =  0.29    # no unit, drag coefficient
u_z = 12      # m/s, mean wind speed !!!
A_z =         # m2, area of exposed tree cover
fi =  1.226   # kg/m3, density of the air

# Wind force
F1_z = cd * fi * u_z^2 * A_z/2

# Wind velocity/# wind log profile/wind profile
# https://en.wikipedia.org/wiki/Log_wind_profile
# describes the vertical distribution of horizontal mean wind speeds
# friction velocity can be calculated knowing wind speed at two levels (z)
# read about profile: http://www.met.reading.ac.uk/~marc/it/wind/interp/log_prof/
# Estimated wind speeds at two levels:
# at the zero plave dispacement
# above the canopy 
# !!!
h = 25
u_z1      = 2       # m/s      
u_canopy =  12      # m/s

#frict_velocity =       # m/s , friction velocity
z0 =  0.06      # m, roughness length, 1/10 of the surface roughness, forest:0.5-1m
d =   h/3*2         # m, zero place displacement, height (m) above the ground with 0 wind speed (2/3 or 3/4 of average height of obstacles)

k =   0.41      # no unit, von Karman constant  
z =   1.3       # m, height abover the groundat the mean height of a 1-m segment  
#u_z = 12       # !!! m/s


# to calculate friction velocity, we need to have wind speed at two heights
z_canopy = h
z_d      = d + 2


# difference velocity X speed: velocity is a vector and includes direction


# plot the wind profiles at multiple segments of the tree
u_z1     = 2       # m/s      
u_canopy = 12      # m/s
segm_height = c(3,5,10,15,20,25)

frict_velocity = k*(u_canopy - u_z1)/log((z_canopy - d)/(z_d - d))   # this is likely incorrect in Peltola's paper, follow shear_velocity https://en.wikipedia.org/wiki/Shear_velocity


# predict speeds for individual segments
u_z = frict_velocity/k*log((segm_height-d)/z0)   # `log` in R stands for `ln` as natural logarithm
# u_z = frict_velocity/k*log((z-d)/z0)   # `log` in R stands for `ln` as natural logarithm

source("http://www.mv.helsinki.fi/home/aakkala/source_taper_curves.r")

  
# Green mass volume & weight
# --------------------------  
  
# Crown projection:
# two triangles: pine & birch
# one triangle for Norway spruce
# common base  = 2*the longest branch in the crown
crown_h = 
crown_r =  
crown_base = pi*crown_r^2  
crown_V = crown_base*crown_h/3  


# Green mass stem at height z:
# green mass of the stem = volume of the segment and mean green density of stem
# calculate the segment volume
stem_density = 850

  

# Canopy streamline:
# reduction of tree area du to higher wind
# -----------------------
St = (10/u_z)-0.10   



# Gravity force
# -----------------------
# by 1 m segments, summ the gravity force
M_z  =             # kg, green mass of stem and crown + snow mass at height z
g    =   9.806    # m/s2 gravitational constant  
F2_z = M_z * g



# wind and gravity forces: can be applied to every section of teh tree
# contribution of each 1 m segment of the tree section to the extrame stem baseturning moments
Tmax =  # Nm, extreme stem base turning moment
x_z =   # m, horizontal displacement of the stem from upright position
gust_factor =  # no unit, relates mean wind to extreme values
gap_factor =   # no unit, takes into account the possible gap  
T_max_z = gust_fact * gap_fact*(F1_z_z + F2_x_z)


# Gust factor: ration between mean and max wind value
# here values for pine
s = 3.8   # spacing between trees
h = 20    # mean tree height
x = 0     # distance from forest edge (in tre heights)
# mean turning point is calculated at edge,
  
# 
gust_mean  = (0.86*(s/h)-0.0385) + (-0.68*(s/h) + 0.4785)*(1.7239*(s/h)+0.0316)^x/h
gust_max = (2.7193*(s/h)-0.061)+(-1.273*(s/h)+0.9701)*(1.1127*(s/h)+0.0311)^x/h

# calculation within stands needs additional multiplier:
# set the x to 0 an 1; recalculate gust means
# !!!
# x = c(0,1)
# f_stand = gust_mean_x0/gust_mean_x  

# Gust factor
gust_factor = gust_max/gust_mean


# Gap factor: takes into account the existence of gaps, 
# as opposed on assumed open area, as in `gust` formulas
gap_size = 1 # size of upwind gap in tree heights
  # gap of 10 tree heights is consider as infinite size gap

gap_mean = (0.001+0.001*gap_size^0.562)/0.00465
gap_max = (0.0072+0.0064*gap_size^0.3467)/0.0214

gap_factor = gap_max/gap_mean

# Total maximum turning moment
# ----------------------------
# at the base of of the stem
# = sum of the max turning moments at all section sof the tree
Tmax = sum(Tmax_z)  # h = mean height of tree, z = 0


# Resistance to uprooting
# ------------------------
# predicted from the root=soil plate weight to derive a resistive moment
# tree is uprooted if the total max turnimg moment exceess the 
# support from soil=plate anchorage
#RS_sup =     #N.m = supporting moment
root_depth = 
rootLength_r = crown_base
root_density_fresh = 1000 # kg/m3  
rootVolume = 0.333*pi*rootLength_r^2*depth
rootMass =  rootVolume * root_density_fresh     # kg, fresh mass of root soil plate
RS_mean = 21   # m, mean depth of the root-soil plate volume (cone)
A_rsw =  30  # %, rot soils plate weight as a proportion of the total belowground anchorage  
  
RS_sup = g*rootMass*RS_mean/A_rsw


# Resistance to stem breakage
# ---------------------------
# assumed that wind-induced stress in the fiber is constant from base to canopy
# possible to calculate at z = 1.3m
# if the stress is higher that the modulus of rupure (MOR) from green timber,
# teh stem will break
# maximum turning moment of a stem to resist:
# tree breaks if total maximum turning moment exceed stem resistance
MOR = 39.1 # MPa, modulus of rupture
STEM_res = pi/32*MOR*dbh^3


# Critical wind speeds
# ---------------------------
# start with wind speed 1m/s at canopy top wind speed
# calculate max turning moment at the tree stem (tree_base = 0, stem_breakage = 1.3)
# if nothing happen, increase the canopy wind speed by 0.1 m/s
# CWS = speed which would lead to damage








# ---------------------------------------------
# Calculate how many trees in a stand will die given the CWS?


# input: 
species = c("spruce")
t_height = c(10)
t_dbh = c10


