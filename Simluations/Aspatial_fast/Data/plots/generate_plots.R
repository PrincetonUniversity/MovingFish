library(ggplot2)
library(ggthemes)
###############     
## Load data ##
###############

setwd('~/Desktop/MovingFish/Updated_simulations/ToRun/code/Aspatial_fast/Data')

noThresh <- read.csv('nothresh/noMPAnotThresh_add_2013-12-09.csv')
consMPA <- read.csv('nothresh/consMPAnotThresh_add_2013-12-09.csv')
fishMPA <- read.csv('nothresh/fishMPAnotThresh_add_2013-12-09.csv')
thresh_noMPA <- read.csv('thresh/noMPA_Thresh_add_2013-12-10.csv')

#########################################################
## Fig 1. Equil.pop as a function of harvest and speed ##
#########################################################

ggplot(noThresh, aes(x = speed, y = Equil.pop, group = harvest)) + geom_line(aes(color = harvest)) + geom_point(aes(color = harvest)) + theme_few() + scale_colour_gradient(high = "red", low = "green") + ylab("Equilibrium biomass") + xlab("Speed") 

ggsave(file="plots/x_Speed.pdf")


ggplot(noThresh, aes(x = harvest, y = Equil.pop, group = speed)) + geom_line(aes(color = speed)) + geom_point(aes(color = speed)) + theme_few() + scale_colour_gradient(high = "red", low = "green") +xlab("Percentage harvested") + ylab("Equilibrium biomass")

ggsave(file="plots/x_harvest.pdf")

############################################
## Figure 3: Synergy in no thresh, no mpa ##
# S = E_(h+c) - (E_h - E_c)               ##
############################################

h_only <- data.frame(E_h = noThresh$Equil.pop[which(noThresh$speed==0)], harvest =  noThresh$harvest[which(noThresh$speed==0)])
c_only <- data.frame(E_c = noThresh$Equil.pop[which(noThresh$harvest==0)], speed = noThresh$speed[which(noThresh$harvest==0)])

# pristine (no disturbance)
pristine <- noThresh$Equil.pop[which(noThresh$speed==0 & noThresh$harvest==0)]

# setting up data to loop through
modnothresh <- noThresh
modnothresh <- modnothresh[-which(modnothresh$speed==0 | modnothresh$harvest ==0),]

modnothresh$synergy <- rep(NA, nrow(modnothresh))

# loop
for(i in 1:nrow(modnothresh)){
  harvest_only <- pristine - h_only$E_h[which(h_only$harvest == modnothresh$harvest[i])]
  climate_only <- pristine - c_only$E_c[which(c_only$speed == modnothresh$speed[i])]
  together <- pristine - modnothresh$Equil.pop[i]
 modnothresh$synergy[i] <- together - (harvest_only + climate_only)
}

ggplot(modnothresh, aes(x = speed, y = harvest, fill = synergy)) + geom_tile()

# weird, i think because of all the ~ 0 areas. Anywhere that has equil.biomass < 1 will set synergy = NA

 modnothresh$synergy[which(modnothresh$Equil.pop < 100)] = NA

ggplot(modnothresh, aes(x = speed, y = harvest, fill = synergy)) + geom_tile()

# don't see any synergy, but that's because at the margins the synergy is so negative. can see it now because have gotten rid of those margins. 

ggplot(modnothresh, aes(x = speed, y = harvest, fill = synergy)) + geom_tile()


#########################################
## Figure 4: compare threshold biomass ##
#########################################

thresh_noMPA$thresh <- factor(thresh_noMPA$thresh)
thresh_noMPA$thresh <- factor(thresh_noMPA$thresh, levels=rev(levels(thresh_noMPA$thresh)))

ggplot(thresh_noMPA,aes(x=speed, y = thresh, fill = Equil.pop)) + geom_tile() + scale_fill_gradient2(low = "white", high = "black")  + theme_few() + ylab("Threshold") + xlab("Speed")

ggsave(file="plots/thresh_noMPA.pdf")

#######################################
## Figure 5: compare MPAs to no MPAs ##
#######################################

diffcons$run <- rep("One large MPA",nrow(diffcons))
difffish$run <- rep("Several small MPAs",nrow(difffish))

combined_diff <- rbind(diffcons, difffish)

cp <- ggplot(combined_diff, aes(x = speed, y = harvest, fill=Equil.pop )) + geom_tile()

cp + facet_grid(~ run) + scale_fill_gradient2(low = "white", high = "black")  + theme_few() + xlab("Speed") + ylab("Percentage popoulation harvested")  + opts(strip.text.y = theme_text(size = 8, colour = "red", angle = 90))

ggsave(file="plots/diff_MPAs.pdf")


####################################
## Figure 6: harvest inside MPAs. ##
####################################

cp <- ggplot(combined_diff, aes(x = speed, y = harvest, fill=Equil.harvest )) + geom_tile()

cp + facet_grid(~ run) + scale_fill_gradient2(low = "white", high = "black")  + theme_few()

ggplot(noThresh,aes(x=speed, y = harvest, fill = Equil.pop)) + geom_tile() + scale_fill_gradient2(low = "white", high = "black")  + theme_few() 

ggsave(file="plots/noThresh_harvest_contour.pdf")



#########################
## Miscellaneous plots ##
#########################

ggplot(consMPA,aes(x=speed, y = harvest, fill = Equil.pop)) + geom_tile() + scale_fill_gradient2(low = "white", high = "black")  + theme_few()

ggsave(file="consMPA_contour.pdf")

ggplot(fishMPA,aes(x=speed, y = harvest, fill = Equil.pop)) + geom_tile() + scale_fill_gradient2(low = "white", high = "black")  + theme_few()

ggsave(file="fishMPA_contour.pdf")

# difference between consMPA and fishMPA and no MPA

diffcons <- consMPA$Equil.pop - noThresh$Equil.pop

diffcons <- cbind(diffcons,consMPA$harvest, consMPA$speed, consMPA$Equil.harvest)
diffcons <- as.data.frame(diffcons)
names(diffcons) <- c("Equil.pop","harvest","speed","Equil.harvest")
ggplot(diffcons,aes(x=speed, y = harvest, fill = Equil.pop)) + geom_tile() + scale_fill_gradient2(low = "white", high = "black")  + theme_few()
ggsave(file="diff_cons_noMPA.pdf")

difffish <- fishMPA$Equil.pop - noThresh$Equil.pop

difffish <- cbind(difffish,fishMPA$harvest, fishMPA$speed, fishMPA$Equil.harvest)
difffish <- as.data.frame(difffish)
names(difffish) <- c("Equil.pop","harvest","speed", "Equil.harvest")
ggplot(difffish,aes(x=speed, y = harvest, fill = Equil.pop)) + geom_tile() + scale_fill_gradient2(low = "white", high = "black")  + theme_few()
ggsave(file="diff_fish_noMPA.pdf")
 
### putting all data together so can do a facet plot
noThresh$run <- rep("NoThresh_NoMPA",nrow(noThresh))
consMPA$run <- rep("NoThresh_ConsMPA", nrow(consMPA))
fishMPA$run <- rep("NoThresh-FishMPA",nrow(fishMPA))

# need to manipulate thresh so it can be comparable. 
thresh_noMPA$run <- rep("Thresh_NoMPA",nrow(thresh_noMPA))
modthresh <- thresh_noMPA

modthresh$harvest <- modthresh$thresh

combined <- rbind(noThresh, consMPA, fishMPA, modthresh)
combined$run <- factor(combined$run)

cp <- ggplot(combined, aes(x = speed, y = harvest, fill=Equil.pop )) + geom_tile() + scale_fill_gradient2(low = "white", high = "black")  + theme_few()

cp + facet_wrap(~ run, scales = "free_y",ncol=2)
combined_nothreshold <- combined[-which(combined$run == "Thresh_NoMPA"),] 

cp <- ggplot(combined_nothreshold, aes(x = speed, y = harvest, fill=Equil.pop )) + geom_tile()

cp + facet_grid(~ run) + scale_fill_gradient2(low = "white", high = "black")  + theme_few()