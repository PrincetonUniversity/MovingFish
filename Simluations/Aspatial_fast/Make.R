rm(list=ls())
setwd("/Users/efuller/Documents/Projects/Moving_fish/MovingFish/Simluations/Aspatial_fast")


# load parameters, functions
  source("Parameters.R")
	source("Functions.R")
	
# run analysis
	# choose threshold or no threshold
	model = "noThresh"	# "noThresh"; "Thresh"
	
	# if no threshold, choose MPA: "null", "cons", "fish"
	MPA = "cons"

# analysis
	if(model=="noThresh") {sapply(c("Parameters_nothresh.R","Sim_noThresh.R"),source,.GlobalEnv)} else {
		if(model=="Thresh" & MPA == "null") {sapply(c("Parameters_thresh.R","Sim_thresh.R"),source,.GlobalEnv)} else {
			warning("model needs to be 'noThresh' or 'Thresh', MPA needs to be 'null'")
			}
		}

# 2014-02-10 
# have run no-thresh cons


