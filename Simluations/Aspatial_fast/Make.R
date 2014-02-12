rm(list=ls())
setwd("/Users/efuller/Documents/Projects/Moving_fish/MovingFish/Simluations/Aspatial_fast")


# load parameters, functions
	source("Functions.R")
	source("Parameters.R")
	
# run analysis
	# choose threshold or no threshold
	model = "noThresh"	# "noThresh"; "Thresh"
	
	# if no threshold, choose MPA: "null", "cons", "fish"
	MPA = "fish"

# analysis
	if(model=="noThresh") {source("Parameters_nothresh.R","Sim_noThresh.R")} else {
		if(model=="Thresh" & MPA == "null") {source("Parameters_thresh.R","Sim_thresh")} else {
			warning("model needs to be 'noThresh' or 'Thresh', MPA needs to be 'null'")
			}
		}