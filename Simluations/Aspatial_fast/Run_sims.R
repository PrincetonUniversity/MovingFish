rm(list=setdiff(ls(), "notify"))
setwd("~/Desktop/MovingFish/Updated_simulations/ToRun/code/Aspatial_fast")

source('noMPA_SIM.R',verbose=TRUE)
notify('noMPA_SIM finished!')

source('MPAfish_SIM.R', verbose=TRUE)
notify('MPAfish_SIM finished!')

source('MPAcons_SIM.R', verbose=TRUE)
notify('MPAcons_SIM finished!')