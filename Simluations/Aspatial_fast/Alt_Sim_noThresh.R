# new version of simulation with periodicity function

# set MPAs
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
	
	output <- startOut(s=0,mpa.yes=mpa.yes,mpa.no=mpa.no,burn_in=burn_in, Fharv=NA, Fthresh=NA, init=init,MPA.start = MPA.start)
	init.s <- output[[1]]
	MPA.start <- output[[2]]

# Loop through various speeds and harvest intensities as definited in the "Parameters.R" file using while loops. Only saving two time steps 

for(q in 1:length(speeds)){
  for(j in 1:length(harvests)){
  	 output <-startOut(s=0, mpa.yes=mpa.yes, mpa.no=mpa.no, burn_in=burn_in, Fharv=harvests[j], Fthresh=NA, init=init.s, MPA.start=MPA.start)

  	cat("add in harvesting...\n")
	init.h <- output[[1]]
	MPA.start <- output[[2]]
  	    
   	cat("add in moving climate...\n")
    output <- startOut(s=speeds[q], mpa.yes=mpa.yes, mpa.no=mpa.no, burn_in=burn_in, Fharv=harvests[j], Fthresh=NA, init=init.h, MPA.start=MPA.start)
   
    # summary stats  
  	pop <- sum(output[[1]])
   	summaries[rownumber[j,q],] <- c(pop, speeds[q], harvests[j], ifelse(exists("Fthresh"), Fthresh,NA))
    
    cat(paste("harvest is ",round(j/length(harvests),1)*100,"% done and speed is ",round(q/length(speeds),1)*100, "% done\n",sep=""))
    }
}

write.csv(summaries,file = paste("Data/MPA",MPA,"_",Sys.Date(),".csv",sep=""))