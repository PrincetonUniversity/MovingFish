#rm(list=ls())
library(plyr)
library(zoo)

# load parameters, functions
setwd("~/Desktop/MovingFish/Updated_simulations/ToRun/code/Aspatial_fast")
source("Parameters.R")
source("Functions.R")

# set MPA (cons.yes/no; fish.yes/no; null.yes/no)
mpa.yes = null.yes
mpa.no = null.no

# added a stipulation for equilibrium: either the difference between two time steps is as small as the initializing step or the difference in the rolling mean with a window of 100 values [move_window below] of the total population abundance is 0. With MPAs getting oscillations with perfect oscillations such that it's never 'at equilibrium' but is not trending. so is at equilibrim

move_window = 100

# initializing the population with no pressure (no harvesting, no climate)
init<-array(0,c(w,maxt)) 
init[which(patch==0.55),1]=50
MPA.current = rep(c(mpa.yes,mpa.no),length.out=length(world))
for(t in 2:maxt){
	output = m(n=init[,t-1], s = 0, mpa.yes = mpa.yes, mpa.no = mpa.no, MPA.current = MPA.current)
	init[,t]= output[[1]]
	MPA.current = output[[3]]
	}

# standard for equilibrium is the difference in the final step of the initialization 
init.diff <- diff(colSums(init))[149]


# Loop through various speeds and harvest intensities as definited in the "Parameters.R" file
# using while loops. Only saving two time steps 


for(q in 1:length(speeds)){
  
  for(j in 1:length(harvests)){
    init.h<-array(0,c(w,1))
    next.pop <- m(n=init[,maxt], s = 0, Fharv=harvests[j], mpa.yes = mpa.yes, mpa.no = mpa.no, MPA.current = MPA.current)
    init.h[,1]=next.pop[[1]]
    MPA.current = next.pop[[3]]
    h.diff <- abs(sum(init[,maxt]-init.h[,1]))
    roll.mean = 1	# set to start
    
    T = 1
    
    while(h.diff > init.diff | roll.mean > init.diff){
      T = T + 1
      next.pop <- m(n=init.h[,T-1], s = 0, Fharv=harvests[j], mpa.yes = mpa.yes, mpa.no = mpa.no, MPA.current = MPA.current)
      init.h <- cbind(init.h,next.pop[[1]])
      MPA.current <- next.pop[[3]]
      h.diff <- abs(sum(init.h[,T]-init.h[,T-1]))
      if(ncol(init.h) > 100){
      	rolls <- rollmean(colSums(init.h), k = move_window)
      	diff.rolls <- diff(rolls)
		    roll.mean <- diff.rolls[length(diff.rolls)]
    }
    }
    # add in moving climate
    move <- array(0,c(w,1))
    next.pop <- m(n=init.h[,T], s = speeds[q], Fharv=harvests[j], mpa.yes = mpa.yes, mpa.no = mpa.no, MPA.current = MPA.current)
    move[,1] <- next.pop[[1]]
    MPA.current <- next.pop[[3]]
    h.diff <- abs(sum(init.h[,T]-move[,1]))
    roll.mean = 1 # reset
    
    T = 1
    
    while(h.diff > init.diff & roll.mean > init.diff){
      T = T + 1
      next.pop <- m(n=move[,T-1], s = speeds[q], Fharv=harvests[j], mpa.yes = mpa.yes, mpa.no = mpa.no, MPA.current = MPA.current)
      
      move <- cbind(move, next.pop[[1]])
      MPA.current <- next.pop[[3]]
      h.diff <- abs(sum(move[,T]-move[,T-1]))
      if(ncol(move) > 300){
      	rolls <- rollmean(colSums(move), k = move_window)
      	diff.rolls <- diff(rolls)
		roll.mean <- diff.rolls[length(diff.rolls)]
		print(T)
    }
    }
       
      # simulate for 300 steps after equilibrium and take average
      harv <- vector(mode = 'logical', length = 300)
      for (i in (T+1):(T+300)){
        next.pop <- m(n=move[,i-1], s = speeds[q], Fharv=harvests[j], mpa.yes = mpa.yes, mpa.no = mpa.no, MPA.current = MPA.current)
        move <- cbind(move, next.pop[[1]])
        MPA.current = next.pop[[3]]
        harv[(i-T)] <- sum(next.pop[[2]])

      }
     # summary stats
      harv.mean <- mean(harv)
      harv.sd <- sd(harv)
      harv.se <- stderr(harv)
      pop.mean <- mean(colSums(move)[(T+1):(T+300)])
      pop.sd <- sd(colSums(move)[(T+1):(T+300)])
      pop.se <- stderr(colSums(move)[(T+1):(T+300)])
       
    summaries[rownumber[j,q],] <- c(
      pop.mean, pop.sd, pop.se, 
      harv.mean, harv.sd, harv.se, 
      speeds[q], harvests[j],ifelse(exists("Fthresh"),Fthresh,NA),
      ncol(init.h), ncol(move))
    print(paste("harvest is ",round(j/length(harvests),1)*100,"% done and speed is ",round(q/length(speeds),1)*100, "% done",sep=""))
    
    }
    
}

write.csv(summaries,file = paste("Data/noMPAnotThresh_add_",Sys.Date(),".csv",sep=""))
notify('Simulation is finished!')
