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

m <- function(n, s, Fthresh = NA, Fharv = NA, mpa.yes = NA, mpa.no = NA, MPA.current=NA){
	
	# calculate how far the patch will move through the population (if speed !=0)
	displaced = ifelse(s>0,s/step_size,1)
	
	# assign population that will still be inside the patch to moved patch
	next_n = n[displaced:length(n)]
	
	# fill in newly existing patch with 0s
	next_n = c(next_n,rep(0,length.out=(displaced-1)))
	
	# Move MPAS
	next_MPA = MPA.current[displaced:length(MPA.current)]

	# Move MPA by figuring out what the MPA sequence starts with (0 or 1) and counting the number of subsequent 0/1s then appending the MPA sequence starting with the opposite interval (if 1 is the first, append 0s). Won't have to count the number of 1s because know that they got left behind the patch. 
	
	if(next_MPA[1]==1){
		start_at <- as.numeric(rle(next_MPA)$length[1])	# find index of last 1 in first sequence
		next_MPA <- next_MPA[1:start_at]
		length_add <- length(world) - length(next_MPA) 		# how many indeces need filling?
		new_MPA <- rep(c(mpa.no,mpa.yes),length.out=length_add)	#
		next_MPA <- c(next_MPA,new_MPA)
		
	}else{
		start_at <- as.numeric(rle(next_MPA)$length[1])
		next_MPA <- next_MPA[1:start_at]
		length_add <- length(world) - length(next_MPA) 
		new_MPA <- rep(c(mpa.yes,mpa.no),length.out=length_add)
		next_MPA <- c(next_MPA,new_MPA)
	}	
	
	# let patch reproduce
	next_patch = vector(mode="numeric",length(next_n))
	
	# keep individuals still in patch + those now in it due to move
	next_patch[1:length(patch)] = next_n[1:length(patch)]
	babies = next_patch*f_ind
	n2 = babies %*% d *step_size
	n2 = sapply(n2,f,R0,K)
	
	# harvesting and MPAs
	if(!is.na(Fthresh)) { 
		next_gen = ifelse(n2 < Fthresh, n2, n2 - (n2 - Fthresh) * Fharv) 
		}
	
	if(!is.na(Fharv) & is.na(Fthresh)) {
    	next_gen = n2*(1-Fharv)
    	}
    
	if(is.na(Fharv) & is.na(Fthresh)) {next_gen = n2}
    	
    # evaluate MPA coverage
    next_gen[next_MPA == 1] <- n2[next_MPA == 1] 
    
    harv = n2-next_gen
 	n2 = next_gen 
 	MPA = next_MPA
 	#plot(world,MPA*max(n2),type='l',col="grey")
	plot(world,MPA*9,type='l',col="grey",main=paste("Speed=",s," Harvest rate=",Fharv,sep=""))
 	lines(world,n2,lwd=2,col="blue")
 	return(list(n2,harv,MPA))
}