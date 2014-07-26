# MPA fluctuations, s = 0.1, h = 0.08
# rm(list=ls())
#setwd("Documents/Projects/Moving_fish/MovingFish/Simluations/Aspatial_fast/")
#source("Parameters.R");source("Parameters_nothresh.R");source("Functions.R");

if(MPA=="cons") {mpa.yes=cons.yes; mpa.no=cons.no} else {
	if(MPA=="fish") {mpa.yes=fish.yes; mpa.no=fish.no} else {
		if(MPA=="null") {mpa.yes=null.yes; mpa.no=null.no} else{
			if(exists("MPA")) warning(paste("MPA needs to be 'cons', 'fish', or 'null'.",sep=""))
		}
	}
}

# initializing the population with no pressure (no harvesting, no climate)
	init<-rep(0,w) # rows are world, columns are time
	init[which(patch==0.55)]=50
	MPA.start = rep(c(mpa.yes,mpa.no),length.out=length(world))
	
	output <- startUp(s=0,mpa.yes=mpa.yes,mpa.no=mpa.no,burn_in=burn_in, Fharv=NA, Fthresh=NA, init=init, MPA.start = MPA.start, effort_re_allocate=NA)
	init.s <- output[[1]]
	MPA.start <- output[[2]]
	
	# adding in parameters for these runs
	
for(q in 1:length(speeds)){
	for(j in 1:length(harvests)){
		# adding harvesting
			output <- startUp(s=0, mpa.yes=mpa.yes, mpa.no=mpa.no, burn_in=burn_in,
			Fharv=harvests[j], Fthresh=NA, init=init.s, MPA.start = MPA.start, 
			effort_re_allocate=effort_allocate)
		  	
		  	init.h <- output[[1]]
			MPA.start <- output[[2]]
		  	MPA.current <- MPA.start
		  	
	# adding speed
	burn_in <- generations_total - generations_av
	init <- init.h
	for(t in 1:(burn_in)){
		output = m(n=init, s = speeds[q], Fthresh=NA,Fharv=harvests[j],
		 mpa.yes = mpa.yes, mpa.no = mpa.no, 
		 MPA.current = MPA.current, effort_re_allocate=effort_allocate)
		init= output[[1]]
		MPA.current = output[[2]]
	}
	
	# make dataframe for simulation average
	pop <- rep(0,generations_av)
	for(keep in 1:generations_av){
		output = m(n=init, s = speeds[q], Fthresh=NA,Fharv=harvests[j],
		 mpa.yes = mpa.yes, mpa.no = mpa.no, 
		 MPA.current = MPA.current, effort_re_allocate = effort_allocate)
		init = output[[1]]
		MPA.current = output[[2]]
		pop[keep] = sum(output[[1]])

		}
	}
}

write.csv(pop, paste("Data/fluctuations/pop",MPA, model, effort_allocate, ".csv",sep=""))	

