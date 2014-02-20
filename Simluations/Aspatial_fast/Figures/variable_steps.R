# MPA simulation with variable numbers of steps for 'equilibrium' step. Set steps outside of this file

# adding in subset of parameters, so instead of running Parameters_nothresh.R, am manually entering
harvests = 0.1
speeds = seq(0.02,0.1,.01)
MPA = "cons"

sims = vector("list", length(speeds))

# setting up correct MPA
if(MPA=="cons") {mpa.yes=cons.yes; mpa.no=cons.no} else {
  if(MPA=="fish") {mpa.yes=fish.yes; mpa.no=fish.no} else {
    if(MPA=="null") {mpa.yes=null.yes; mpa.no=null.no} else{
      if(exists("MPA")) warning(paste("MPA needs to be 'cons', 'fish', or 'null'.",sep=""))
    }
  }
}

# initialize the world, set MPA
output <- startOut(w,maxt,mpa.yes,mpa.no,world)
init <- output[[1]]
MPA.start <- output[[2]]

# standard for equilibrium is the difference in the final step of the initialization 
init.diff <- diff(colSums(init))[(maxt-1)]

for(q in 1:length(speeds)){
  
  for(j in 1:length(harvests)){
    init.h<-array(0,c(w,1))
    next.pop <- m(n=init[,maxt], s = 0, Fharv=harvests[j], mpa.yes = mpa.yes, mpa.no = mpa.no, MPA.current = MPA.start)
    init.h[,1]=next.pop[[1]]
    MPA.current = next.pop[[3]]
    h.diff <- abs(sum(init[,maxt]-init.h[,1]))
    roll.mean = 1  # set to start
    
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
    
    # simulate for <steps> steps after equilibrium and take average
    harv <- vector(mode = 'logical', length = steps)
    for (i in (T+1):(T+steps)){
      next.pop <- m(n=move[,i-1], s = speeds[q], Fharv=harvests[j], mpa.yes = mpa.yes, mpa.no = mpa.no, MPA.current = MPA.current)
      move <- cbind(move, next.pop[[1]])
      MPA.current = next.pop[[3]]
      harv[(i-T)] <- sum(next.pop[[2]])
      
    }
  }
  
  sims[[q]] = move[,(ncol(move)-(steps-1)):ncol(move)]
}

# save the time series of population abundance 
series <- llply(sims,colSums)
df <- melt(series)
names(df) <- c("sums","speed")
df$speed=factor(df$speed)
df$time <- rep(seq(1,steps),length(speeds))
