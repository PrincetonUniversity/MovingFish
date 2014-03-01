# just doing the MPA moving thing
reps = 450
test = vector("list",length=reps)

displaced = ifelse(s>0,s/step_size,1)

test[[1]]$MPA_finish = MPA.start

for(i in 2:reps){  
# move MPA forward by <displaced> amount
  test[[i]]$next_MPA = test[[i-1]]$MPA_finish[displaced:length(test[[i-1]]$MPA_finish)]
  lost <- test[[i-1]]$MPA_finish[1:(displaced-1)] 
 # are there any MPAs in <next_MPA>?
  #any(test[[i]]$next_MPA==1)
  # IF FALSE, then need to figure out how many 0s were lost in move, and make sure to preserve interval of zeros == mpa.no, then fill in mpa.yes,mpa.no to length of world
  if(any(test[[i]]$next_MPA==1)==FALSE & any(lost==1)==FALSE){
    # these are the intervals that are left behind as world moves forward
      lost <- test[[i-1]]$MPA_finish[1:(displaced-1)] 
    # this is the last continuous chunk of numbers at the end of <lost>  
      length_last <- rep(tail(rle(lost)$value,1), tail(rle(lost)$lengths,1))
    # want to know how long <length_last> is so can make sure to get correct interval
      L_int <- length(length_last)
    # how many more zeros do we need before we start with mpa.yes again?
      zero_add <- sum(mpa.no==0) - L_int - sum(test[[i]]$next_MPA==0)
    # this is what needs to be appended to <next_MPA>
      test[[i]]$new_MPA <- c(rep(0,zero_add),rep(c(mpa.yes,mpa.no),length.out=w))
      
  }else{
  
  # IF FALSE (there are some 1s) then we only care about the last interval of the <next_MPA>, is that protected or not?
    last_step = tail(test[[i]]$next_MPA,1)
   # IF <last_step>==1
    if(last_step ==1){ 
      #then how many 1s are at the end of the <next_MPA> section?
        end_step = rep(tail(rle(test[[i]]$next_MPA)$value,1), tail(rle(test[[i]]$next_MPA)$lengths,1)) # number of 1s at very end of lost interval
      # length of <end_step>
      end_int = length(end_step)
    # need to prepend sum(mpa.yes==1) - <end_step> to beginning 
      prepend = rep(1, (sum(mpa.yes==1) - end_int))
    # and then fill out with mpa.no,mpa.yes for length of world          
      fillOut <- rep(c(mpa.no, mpa.yes), length = w )
    }else{
      # IF <last_step>==0
        end_step = rep(tail(rle(test[[i]]$next_MPA)$value,1), tail(rle(test[[i]]$next_MPA)$lengths,1)) # number of 0s at very end of lost interval
      # length of <end_step>
        end_int = length(end_step)
      # need to prepend sum(mpa.no==0) - <end_step> to beginning 
        prepend = rep(0, (sum(mpa.no==0) - end_int))
      # and then fill out with mpa.no,mpa.yes for length of world          
        fillOut <- rep(c(mpa.yes, mpa.no), length = (w)
    }
   test[[i]]$new_MPA = c(prepend,fillOut)
  }
  
test[[i]]$MPA_finish = c(test[[i]]$next_MPA,test[[i]]$new_MPA)
test[[i]]$MPA_finish = test[[i]]$MPA_finish[1:w]

}


foo=vector("numeric",length(rep))
foo[1] = 
  length(
    which(
      test[[1]]$MPA_finish!=test[[2]]$MPA_finish))

for(i in 2:(reps-1)){
  foo[i] = length(which(test[[i]]$MPA_finish!=test[[i+1]]$MPA_finish))
}

plot(foo,type='o',pch=19,cex=.5)
plot(foo,type="h",lwd=3.5,col=alpha("orange",0.5))

  #############
### Old stuff ####
  #############

# these are the intervals that are left behind as world moves forward
  lost <- test[[i-1]]$MPA_finish[1:(displaced-1)]   
# this is the last continuous chunk of numbers at the end of <lost>
  length_last <- rep(tail(rle(lost)$value,1), tail(rle(lost)$lengths,1))

# want to know how long <length_last> is so can make sure to get correct interval
  L_int <- length(length_last)

# <length_last> is all 1 value (either 0,1). IF

  # it's a 1, then count the number of 1s, figure out how many more are needed, and append then mpa.no, mpa.yes to fill out the world
    if(length_last[1]==1){
      # how many 1s are needed? 
      one_add <- sum(mpa.yes==1) - L_int - 
    }
  # it's a 0, count the number of 0s, figure out how many more are need, and append mpa.yes, mpa.no to fill out the world


test[[i]]$start_at <- as.numeric(rle(test[[i]]$next_MPA)$length[1])    
test[[i]]$penult_MPA <- test[[i]]$next_MPA[1:start_at]

if(test[[i]]$next_MPA[1]==1){
  one_add <- ifelse(tail(length_last,1)==1,sum(mpa.yes==1) = sum(lost==1) - sum()
  test[[i]]$new_MPA <- rep(c(mpa.no,mpa.yes),length.out=test[[i]]$length_add)
}else{
  zero_add <- sum(mpa.no==0) - sum(lost==0) - sum(test[[i]]$penult_MPA==0)
  finalAdd <- length(world) - length(test[[i]]$penult_MPA) - zero_add
  test[[i]]$new_MPA <- c(rep(0,zero_add),rep(c(mpa.yes,mpa.no),length.out=finalAdd))
}	

test[[i]]$MPA_finish <- c(test[[i]]$penult_MPA,test[[i]]$new_MPA)

}