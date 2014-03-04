# added a stipulation for equilibrium: either the difference between two time steps is as small as the initializing step or the difference in the rolling mean with a window of 100 values [move_window below] of the total population abundance is 0. With MPAs getting oscillations with perfect oscillations such that it's never 'at equilibrium' but is not trending. so is at equilibrim

if(MPA=="cons") {mpa.yes=cons.yes; mpa.no=cons.no} else {
	if(MPA=="fish") {mpa.yes=fish.yes; mpa.no=fish.no} else {
		if(MPA=="null") {mpa.yes=null.yes; mpa.no=null.no} else{
			if(exists("MPA")) warning(paste("MPA needs to be 'cons', 'fish', or 'null'.",sep=""))
		}
	}
}

# initialize the world
	output <- startOut(w,maxt,mpa.yes,mpa.no,world)
	 init <- output[[1]]
	MPA.start <- output[[2]]

# standard for equilibrium is the difference in the final step of the initialization 
	init.diff <- diff(colSums(init))[(maxt-1)]

# Loop through various speeds and harvest intensities as definited in the "Parameters.R" file using while loops. Only saving two time steps 

for(q in 1:length(speeds)){
  
  for(j in 1:length(harvests)){
    init.h<-array(0,c(w,1))
    next.pop <- m(n=init[,maxt], s = 0, Fharv=harvests[j], mpa.yes = mpa.yes, mpa.no = mpa.no, MPA.current = MPA.start)
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
    roll.mean = 10 # reset
    
    # run for an extra 300 steps to ensure we get out of transition between harvesting and move+harvesting
    
    for (i in 2:300){
      next.pop <- m(n=move[,i-1], s = speeds[q], Fharv=harvests[j], mpa.yes = mpa.yes, mpa.no = mpa.no, MPA.current = MPA.current)
      move <- cbind(move, next.pop[[1]])
      MPA.current = next.pop[[3]]
      harv[(i-T)] <- sum(next.pop[[2]])
    }  
    
    T = ncol(move)
    
    while(h.diff > init.diff & roll.mean > init.diff){
      T = T + 1
      next.pop <- m(n=move[,T-1], s = speeds[q], Fharv=harvests[j], mpa.yes = mpa.yes, mpa.no = mpa.no, MPA.current = MPA.current)
      
      move <- cbind(move, next.pop[[1]])
      MPA.current <- next.pop[[3]]
      h.diff <- abs(sum(move[,T]-move[,T-1]))
      rolls <- rollmean(colSums(move), k = move_window)
      diff.rolls <- diff(rolls)
		  roll.mean <- diff.rolls[length(diff.rolls)]

    }
    
    T = ncol(move)
		# then run until the MPA.current matches the MPA.start so we compare populations at the same point in spatial cycle
    req = 0   # this is the condition for stopping, can be modified if count gets too high
		while(length(which(MPA.current!=MPA.start)) > req){
		  T = T + 1
		  next.pop <- m(n=move[,T-1], s = speeds[q], Fharv=harvests[j], mpa.yes = mpa.yes, mpa.no = mpa.no, MPA.current = MPA.current)
		  
		  move <- cbind(move, next.pop[[1]])
		  MPA.current <- next.pop[[3]]
      print(T)
      print(length(which(MPA.current!=MPA.start)))		  
		}
    
		# simulate for 300 steps after equilibrium
		harv <- vector(mode = 'logical', length = 300)
		for (i in (T+1):(T+300)){
		  next.pop <- m(n=move[,i-1], s = speeds[q], Fharv=harvests[j], mpa.yes = mpa.yes, mpa.no = mpa.no, MPA.current = MPA.current)
		  move <- cbind(move, next.pop[[1]])
		  MPA.current = next.pop[[3]]
		  harv[(i-T)] <- sum(next.pop[[2]])
		}  
   
     # summary stats
      harv.mean <- mean(harv)
      #harv.mean <- NA
      harv.sd <- sd(harv)
      #harv.sd <- NA
      harv.se <- stderr(harv)
      #harv.se <-NA
      #pop.mean <- mean(move[,T])
      pop.mean <- mean(colSums(move)[(T+1):(T+300)])
      #pop.mean <- mean(max(colSums(move)[(T+1):(T+300)]),min(colSums(move)[(T+1):(T+300)])) # this deals with variability in taking mean of oscillation
      pop.sd <- sd(colSums(move)[(T+1):(T+300)])
      pop.se <- stderr(colSums(move)[(T+1):(T+300)])
      #pop.sd <- sd(move[,T])
      #pop.se <- stderr(move[,T])
      req <- req
  
    summaries[rownumber[j,q],] <- c(
      pop.mean, pop.sd, pop.se, 
      harv.mean, harv.sd, harv.se, 
      speeds[q], harvests[j],ifelse(exists("Fthresh"),Fthresh,NA),
      ncol(init.h), ncol(move), req)
    print(paste("harvest is ",round(j/length(harvests),1)*100,"% done and speed is ",round(q/length(speeds),1)*100, "% done",sep=""))
    
    }
    
}

write.csv(summaries,file = paste("Data/MPA",MPA,"_",Sys.Date(),".csv",sep=""))