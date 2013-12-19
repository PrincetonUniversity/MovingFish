######################################
## Parameters & Building Structures ##
######################################

step_size=0.01 #distance between points in space
b=.5 #parameter for Laplace dispersal kernel
R0=5 #growth parameter for recruitment
K=100 #carrying capacity parameter for juvenile density dependence
maxt = 150 # number of generations to run for initializing of harvest
# speeds = seq(0,1,.1)
speeds = seq(0,0.5,0.02)
harvests = 1
thresholds = seq(0,1,by=0.1)

f_ind = 1 #per capita reproductive rate

patch = seq(0,1,by=step_size)
world = seq(-.51,1.5, by = step_size)
w = length(world)

# haven't dealt with MPAs yet !

cons.yes = rep(1,2*b/step_size)
cons.no = rep(0,4*b/step_size)
fish.yes = rep(1,floor((1/3*b)/step_size))  # had to round because not complete step size. Rounded down. 
fish.no = rep(0,floor((2/3*b)/step_size))

null.yes = rep(0,length(world))
null.no = rep(0, length(world))

# build dataframes

summaries <- data.frame(Equil.pop = rep(NA,length=length(speeds)*length(thresholds)), Equil.pop.sd = rep(NA, length=length(speeds)*length(thresholds)), Equil.pop.se = rep(NA, length=length(speeds)*length(thresholds)), Equil.harvest = rep(NA,length=length(speeds)*length(thresholds)),Equil.harvest.sd = rep(NA,length=length(speeds)*length(thresholds)), Equil.harvest.se = rep(NA,length=length(speeds)*length(thresholds)), speed=rep(NA,length=length(speeds)*length(thresholds)), harvest = rep(NA,length=length(speeds)*length(thresholds)), thresh=rep(NA,length=length(speeds)*length(thresholds)), steps.harv = rep(NA,length=length(speeds)*length(thresholds)),steps.move = rep(NA,length=length(speeds)*length(thresholds)))


# index for row number
rownumber <- matrix(seq(1:(length(thresholds)*length(speeds))),ncol=length(speeds))
