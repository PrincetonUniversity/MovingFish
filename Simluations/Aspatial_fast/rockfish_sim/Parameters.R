# Parameters.R
#######################################
## Parameters & Building Structures ##
######################################
# units are 1000s of kilometers

step_size=0.002 #distance between points in space (1 km)
b=1/.073 # b = 1/beta. beta = 73 km but then needs to be adjusted for scale. so thousands of km. so 1/.073
R0=2.86 #growth parameter for recruitment
K=1 #carrying capacity parameter for juvenile density dependence
threshold = 0.001 #difference between generation populations. 
burn_in = 2000 # number of generations to run simulations before checking for equilibrium conditions
speeds = seq(0,.02,by=.002) # (from 0 to 200 km)
harvests = seq(0,1,by=.1)  # (from 0-100%)
f_ind = 1 #per capita reproductive rate
generations_total = 8000
generations_av = 2000

patch = seq(0,1,by=step_size)
world = seq(-.51,4.5, by = step_size)   # to run the MPA versions, world has to be at least 400 steps (max distance between MPAs in "cons" run)
w = length(world)

# MPAs are 20km wide and 75km apart. Step size is 2 km (1000*step_size). 

rock.yes = rep(1,.02/step_size) 
rock.no = rep(0,.076/step_size) # (more than 75. so conservative)

null.yes = rep(0,length(world))
null.no = rep(0, length(world))

move_window = 100

#------------------------------------------------------------------------#