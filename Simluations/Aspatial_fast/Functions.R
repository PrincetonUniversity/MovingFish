#Laplace dispersal kernel
k<-function(x,y,b) return(1/2*b*exp(-b*abs(x-y)))    

# dispersal matrix
d<-array(,c(w,w)) #dispersal probabilities to point i from every point j
for(i in 1:w) d[i,]=k(world[i],world,b)

#Beverton-Holt recruitment
f<-function(n,R0,K) return(R0*n/(1+(R0-1)/K*n))

#Standard error removing NAs
stderr <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))


moveMPA <- function(MPA.current = MPA.current, displaced = displaced, mpa.yes=mpa.yes, mpa.no=mpa.no, world){
  
  ##### Move MPAS
  # move MPA forward by <displaced> amount
  next_MPA = MPA.current[displaced:length(MPA.current)]
  lost <- MPA.current[1:(displaced-1)] 
  
  # are there any MPAs in <next_MPA>?
  #any(test[[i]]$next_MPA==1)
  # IF FALSE, then need to figure out how many 0s were lost in move, and make sure to preserve interval of zeros == mpa.no, then fill in mpa.yes,mpa.no to length of world. But also this is only for when mpa.no exists on both sides of MPA_next. If exactly to the edge of one reserve is lost, should shift down to next if statement
  if(any(next_MPA==1)==FALSE & any(lost==1)==FALSE){
    # these are the intervals that are left behind as world moves forward
    lost <- MPA.current[1:(displaced-1)] 
    # this is the last continuous chunk of numbers at the end of <lost>  
    length_last <- rep(tail(rle(lost)$value,1), tail(rle(lost)$lengths,1))
    # want to know how long <length_last> is so can make sure to get correct interval
    L_int <- length(length_last)
    # how many more zeros do we need before we start with mpa.yes again?
    zero_add <- sum(mpa.no==0) - L_int - sum(next_MPA==0)
    # this is what needs to be appended to <next_MPA>
    new_MPA <- c(rep(0,zero_add),rep(c(mpa.yes,mpa.no),length.out=length(world)))
    
  }else{
    
    # IF FALSE (there are some 1s) then we only care about the last interval of the <next_MPA>, is that protected or not?
    last_step = tail(next_MPA,1)
    # IF <last_step>==1
    if(last_step ==1){ 
      #then how many 1s are at the end of the <next_MPA> section?
      end_step = rep(tail(rle(next_MPA)$value,1), tail(rle(next_MPA)$lengths,1)) # number of 1s at very end of lost interval
      # length of <end_step>
      end_int = length(end_step)
      # need to prepend sum(mpa.yes==1) - <end_step> to beginning 
      prepend = rep(1, (sum(mpa.yes==1) - end_int))
      # and then fill out with mpa.no,mpa.yes for length of world          
      fillOut <- rep(c(mpa.no, mpa.yes), length = (length(world) - length(prepend)))
    }else{
      # IF <last_step>==0
      end_step = rep(tail(rle(next_MPA)$value,1), tail(rle(next_MPA)$lengths,1)) # number of 0s at very end of lost interval
      # length of <end_step>
      end_int = length(end_step)
      # need to prepend sum(mpa.no==0) - <end_step> to beginning 
      prepend = rep(0, (sum(mpa.no==0) - end_int))
      # and then fill out with mpa.no,mpa.yes for length of world          
      fillOut <- rep(c(mpa.yes, mpa.no), length = (length(world) ))
    }
    new_MPA = c(prepend,fillOut)
  }
  
  MPA_finish = c(next_MPA,new_MPA)
  MPA_finish = MPA_finish[1:length(world)] # reduce to just the size of the world
  
  return(MPA_finish)
}

m <- function(n, s, Fthresh = NA, Fharv = NA, mpa.yes = NA, mpa.no = NA, MPA.current=NA,effort_re_allocate=NA){
	# steps
	# 1. Harvest (check for thresholds, harvesting, MPA coverage)
	# 2. Patch moves (and MPAs are adjusted)
	# 3. Fish outside patch die
	# 4. Fish still alive (ie inside the patch) reproduce
	
	# harvesting occurs first - check to see how should re-allocate effort
	
	if(!is.na(effort_re_allocate)){	
		total_catch = sum(n*Fharv)
		available_total_pop = sum(n[which(MPA.current==0)]) # pop with no MPA coverage
		available_fish <- n[which(MPA.current==0)] # available_total_pop # proportion at each point
		catch_in_space <- total_catch*available_fish # allocate catch
		
		# NEED check that total catch is preserved
		#sum(catch_in_space == total_catch)
		
		harvest <- MPA.current
		harvest[harvest==0]<-catch_in_space
		harvest[harvest==1]<-0
		
		next_gen = n-harvest
		next_gen[next_gen<0] = 0
	}else{
		if(!is.na(Fthresh)) { # if thresholds
			next_gen = ifelse(n < Fthresh, n, n - (n - Fthresh) * Fharv) 
			}
		if(!is.na(Fharv) & is.na(Fthresh)) { # if harvesting, no thresholds
	    	next_gen = n*(1-Fharv)
	    	}
		if(is.na(Fharv) & is.na(Fthresh)) {next_gen = n} # if no harvesting of any kind
	    	
		# but put fish back if places that were harvested were in the MPA
		 next_gen[MPA.current == 1] <- n[MPA.current == 1]
	 } 
	
	# move the patch
	# calculate how far the patch will move through the population (if speed !=0)
	displaced = ifelse(s>0,s/step_size,1)
	
	# assign population that will still be inside the patch to moved patch
	next_n = next_gen[displaced:length(next_gen)]
	
	# fill in newly existing patch with 0s
	next_n = c(next_n,rep(0,length.out=(displaced-1)))
	
	
# move MPAs?
  if(s > 0){MPA_finish = moveMPA(MPA.current,displaced,mpa.yes,mpa.no,world)}else{MPA_finish= MPA.current}
	
	# let patch reproduce
	next_patch = vector(mode="numeric",length(world))
	
	# keep individuals still in patch + those now in it due to move
	next_patch[1:length(patch)] = next_n[1:length(patch)]
	
    harv = n-next_gen
    babies = next_patch*f_ind
    n2 = babies %*% d *step_size
    n2 = sapply(n2,f,R0,K)

 	  MPA = MPA_finish
	#plot(world,MPA*9,col="grey",main=paste("Speed=",s," Harvest rate=",Fharv,sep=""),type="h",ylim=c(0,10))
 	#lines(world,n2,lwd=2,col="blue")
 	return(list(n2,harv,MPA))
}

# goes until the slope is less than 0.001 as measured with linear regression. Returns final population 
startOut <- function(s, mpa.yes, mpa.no, burn_in, Fharv, Fthresh, init, MPA.start){
	# burn in:
	MPA.current <- MPA.start
	cat("burning in...\n")
		for(t in 1:burn_in){
			output = m(n=init, s = s, Fthresh=Fthresh,Fharv=Fharv, mpa.yes = mpa.yes, mpa.no = mpa.no, MPA.current = MPA.current)
			init= output[[1]]
			MPA.current = output[[3]]
			if(sum(output[[1]])<threshold){ # if below threshold, call it zero and end this loop
			return(list(rep(0,nrow(ts.init)),MPA.current))}

		}
	# now continue and require that the slope is < threshold
		slope <- 1 # initializing the difference between steps
		ts.i <- 1 # indexes the time series array
		ts.init <- as.data.frame(init) 
		
	# initializing first 2
	cat("initializing first two...\n")
		output = m(n=ts.init[,ts.i], s = s, Fthresh=Fthresh, Fharv=Fharv, mpa.yes = mpa.yes, mpa.no = mpa.no, MPA.current = MPA.current)
		ts.init = cbind(ts.init,output[[1]])
		MPA.current = output[[3]]
		ts.sums = data.frame(ts.sums = colSums(ts.init),time=1:ncol(ts.init))
		ts.i <-2
		output = m(n=ts.init[,ts.i], s = s, Fthresh=Fthresh, Fharv=Fharv, mpa.yes = mpa.yes, mpa.no = mpa.no, MPA.current = MPA.current)
		ts.init = cbind(ts.init,output[[1]])
		MPA.current=output[[3]]
		ts.sums = data.frame(ts.sums = colSums(ts.init),time=1:ncol(ts.init))
	
	cat("checking for slope < threshold...\n")	
	while(abs(slope) > threshold){
		
		ts.i = ts.i + 1
		output = m(n=ts.init[,ts.i], s = s, Fthresh=Fthresh, Fharv=Fharv, mpa.yes = mpa.yes, mpa.no = mpa.no, MPA.current = MPA.current)
		ts.init = cbind(ts.init,output[[1]])
		MPA.current=output[[3]]
		ts.sums = data.frame(ts.sums = colSums(ts.init),time=1:ncol(ts.init))
		sumlm <- summary(lm(ts.sums~time,data=ts.sums))
		slope = coef(sumlm)[2]
		# if below threshold, call it zero and end this loop
		if(sum(output[[1]])<threshold) return(list(rep(0,nrow(ts.init)),MPA.current))
		
			#par(mfrow=c(1,2))
			#plot(world,MPA.current*9,col="grey",main=paste("Speed=",s," Harvest rate=",Fharv,sep=""),type="h",ylim=c(0,10))
 			#lines(world,output[[1]],lwd=2,col="blue")
			#plot(ts.sums$time, ts.sums$ts.sums,type="o",pch=19,col="orange",main="population over time")
		}
	
	# if monotonic, return final abundance (sum of final time step)
	# if periodic, (i.e. any(diff(ts.init[,ts.i]>0)==TRUE)), then need to do periodicity and take average of last period. 
	
	cat("checking for periodicity...\n")
	if(any(diff(ts.sums$ts.sums)>0) & s>0){ # if periodic then...
	# make sure there are at least 3 periods, if not keep going until true
	cat("adding a few more periods...\n")
	while(count_ind(ts.sums$ts.sums)[[1]]==FALSE){
			ts.i = ts.i + 1
			output = m(n=ts.init[,ts.i], s = s, Fthresh=Fthresh, Fharv=Fharv, mpa.yes = mpa.yes, mpa.no = mpa.no, MPA.current = MPA.current)
			ts.init = cbind(ts.init,output[[1]])
			MPA.current=output[[3]]
			ts.sums = data.frame(ts.sums = colSums(ts.init),time=1:ncol(ts.init))
			if(sum(output[[1]])<threshold){ # if below threshold, call it zero and end this loop
			return(list(rep(0,nrow(ts.init)),MPA.current))}

				# plot(world,MPA.current*9,col="grey",main=paste("Speed=",s," Harvest rate=", Fharv, sep=""), type="h", ylim=c(0,10))
 				# lines(world,output[[1]],lwd=2,col="blue")
				 #plot(ts.sums$time, ts.sums$ts.sums, type="o", pch=19, col="orange", main = "population over time")
		cat(".")
		}
		
		# once more than 3 periods label periods and get average for final period
		final_mean <- findPeriod(ts.sums$ts.sums)
		return(list(final_mean,MPA.current))	
	}else{return(list(ts.init[,ts.i], MPA.current))}
}

# identify periods if periodicity in time series
findPeriod <- function(sums){ # sums = the time series of abundance (sum across world)
		require(plyr)
		check_ind <- count_ind(sums)
		strct <- check_ind[[3]]
		ind <- check_ind[[2]]
		# set up period id vector
			per <- rep(0,length(sums)) # vector to label period
		# initialize the dataframe of indices
			indices <- data.frame(start=1,end=strct[[1]][1]+strct[[1]][2])
			# the period length
		# initialize the period counter
			w = 0
		# identify the first period
			per[indices[1,1]:indices[1,2]] <- w + 1
		# add one to period counter
			w = w + 1
		# id rest of periods 
			for(j in 2:length(ind)){
				id <- ind[j]
				new_start <-indices[j-1,2]
				new_end <- new_start + strct[[1]][id]+strct[[1]][id+1]
				indices[j,] <- c(new_start, new_end)
				per[indices[j,1]:indices[j,2]] <- w + 1
				w = w + 1
				}
		# make new dataframe with all info
			all_data <- data.frame(sums=sums,period=per,time=1:length(sums))
		# take average by period
			av_pop <- ddply(all_data, .(period),summarize, equil = mean(sums),  len=length(sums))
			to_remove <- c(1,max(av_pop$period)) # remove first and last to make sure have a complete period
			new_av <- subset(av_pop,!(period %in% to_remove))
			# take last available period
			final_period <- subset(new_av, period==max(new_av$period))
	 		equil_mean <- final_period$equil
	 		return(equil_mean)
		}
		
# makes sure there's enough periods to count over
count_ind <- function(sums){
	# take average of existing vector
			av <- mean(sums)
		# make vector of differences from average
			d.a <- sums-av
		# find out which periods are positive and negative
			strct <- rle(d.a>0)
		# set an index to run. Needs to be even, because odd, won't have a final half of a period	
			ind <- seq(from=1,to=length(strct[[1]]),by=2) # the index of the rle vector 
			is.even <- length(strct[[1]])%%2 # if 1, not even, subtract the last element from ind
			if(is.even==1){ind<- head(ind,-1)}
			
	if(length(ind)>3){return(list(TRUE,ind,strct))}else{return(list(FALSE,ind,strct))}
}