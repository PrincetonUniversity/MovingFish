# m function doesn't quite work yet. right now there's no way to disperse in front of the patch and have the patch move on top of you, so harder to get population persistence. also why you get lower harvests, can only harvest in the patch. 

# maybe solution is to make dispersal kernal as big as the patch plus the biggest possible displacement (displacement from the fastest speed)

# thought I figured it out with doubling the space ahead of the patch for it to shift into so I can keep track of fish dispersed ahead of patch. But can't maintain a population with no harvesting and no speed. So something is wrong.. 

# i might have messed up the dispersal kernel.. should see what it looked like on other versions of the code. 

# solution, the world units were an index, and instead need to be at the same scale as the step size.. So each the patch needs to be equal to 1 on the world units, and 101 steps need to make up the world.. still don't completely understand, but can now get a population to grow. 

# figured it out by summing rows across dispersal matrix. Found that the dispersal step was knocking down the population hugely each time step such that it could never grow. 

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



m <- function(n, s, Fthresh = NA, Fharv = NA, mpa.yes = NA, mpa.no = NA, MPA.current=NA){
	
	# calculate how far the patch will move through the population (if speed !=0)
	displaced = ifelse(s>0,s/step_size,1)
	
	# assign population that will still be inside the patch to moved patch
	next_n = n[displaced:length(n)]
	
	# fill in newly existing patch with 0s
	next_n = c(next_n,rep(0,length.out=(displaced-1)))
	
# move MPAs?
  if(s > 0){MPA_finish = moveMPA(MPA.current,displaced,mpa.yes,mpa.no,world)}else{MPA_finish= MPA.current}
	
	# let patch reproduce
	next_patch = vector(mode="numeric",length(world))
	
	# keep individuals still in patch + those now in it due to move
	next_patch[1:length(patch)] = next_n[1:length(patch)]
	
	
	# harvesting and MPAs
	if(!is.na(Fthresh)) { 
		next_gen = ifelse(next_patch < Fthresh, next_patch, next_patch - (next_patch - Fthresh) * Fharv) 
		}
	
	if(!is.na(Fharv) & is.na(Fthresh)) {
    	next_gen = next_patch*(1-Fharv)
    	}
    
	if(is.na(Fharv) & is.na(Fthresh)) {next_gen = next_patch}
    	
    # evaluate MPA coverage
    next_gen[MPA_finish == 1] <- next_patch[MPA_finish == 1] 
    
    harv = next_patch-next_gen
    babies = next_gen*f_ind
    n2 = babies %*% d *step_size
    n2 = sapply(n2,f,R0,K)

 	  MPA = MPA_finish
	plot(world,MPA*9,col="grey",main=paste("Speed=",s," Harvest rate=",Fharv,sep=""),type="h")
 	lines(world,n2,lwd=2,col="blue",type="h")
 	return(list(n2,harv,MPA))
}

startOut <- function(w, maxt, mpa.yes,mpa.no,world){
# initializing the population with no pressure (no harvesting, no climate)
init<-array(0,c(w,maxt)) 
init[which(patch==0.55),1]=50
MPA.start = rep(c(mpa.yes,mpa.no),length.out=length(world))
for(t in 2:maxt){
	output = m(n=init[,t-1], s = 0, mpa.yes = mpa.yes, mpa.no = mpa.no, MPA.current = MPA.start)
	init[,t]= output[[1]]
	MPA.current = output[[3]]
	}

return(list(init, MPA.start))
}
