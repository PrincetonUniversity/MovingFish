# show that variability of population size is different between consmpa and fishmpa

setwd("/Users/efuller/Documents/Projects/Moving_fish/MovingFish/Simluations/Aspatial_fast")
source("Parameters.R")
source("Functions.R")
require(zoo)
require(plyr)
require(lattice)
require(dplyr)
require(reshape)

########
# Cons #
########

steps = 300   # setting number of steps over which to take equilibrium 
harvests = 0.08
speeds = c(0.1,0.3)
MPA = "cons"
source("Figures/variable_steps.R")


simsCons = sims
dfCons = df
seriesCons = series

finalsCons <- llply(seriesCons,mean)
sdCons <- llply(seriesCons,sd)
sdCons <- melt(sdCons)
df_finalsCons <- melt(finalsCons)
df_finalsCons$sd = sdCons$value
names(df_finalsCons) <- c("totalPop","Speed","sd")
df_finalsCons$Speed = speeds
xyplot(totalPop ~ Speed, data=df_finalsCons,type='l',lwd=2,xlab="Speed", main = "Cons steps")
ggplot(df_finalsCons,aes(x = Speed, y = totalPop)) + geom_errorbar(aes(ymin=totalPop - sd, ymax = totalPop + sd)) + geom_line() + geom_point()

xyplot(sums ~ time | speed, data = dfCons, type='l',xlab="time",ylab="Population biomass",lwd=2, main = "Cons steps")


########
# Fish #
########

steps = 300   # setting number of steps over which to take equilibrium 
MPA = "fish"
source("Figures/variable_steps.R")


simsfish = sims
dffish = df
seriesfish = series

finalsfish <- llply(seriesfish,mean)
sdfish <- llply(seriesfish,sd)
sdfish <- melt(sdfish)
df_finalsfish <- melt(finalsfish)
df_finalsfish$sd = sdfish$value
names(df_finalsfish) <- c("totalPop","Speed","sd")
df_finalsfish$Speed = speeds
xyplot(totalPop ~ Speed, data=df_finalsfish,type='l',lwd=2,xlab="Speed", main = "fish steps")
ggplot(df_finalsfish,aes(x = Speed, y = totalPop)) + geom_errorbar(aes(ymin=totalPop - sd, ymax = totalPop + sd)) + geom_line() + geom_point()

xyplot(sums ~ time | speed, data = dffish, type='l',xlab="time",ylab="Population biomass",lwd=2, main = "fish steps")

dffish$MPA = rep("many small MPAs",nrow(dffish))
dfCons$MPA = rep("few large MPAs",nrow(dfCons))

newdata <- rbind(dffish, dfCons)

subnewdata <- subset(newdata, speed == 1)

pdf(file="bounded_flux.pdf")
xyplot(sums ~ time | MPA, data = subnewdata, type='l',lwd=2,ylab="Population biomass")
dev.off()
