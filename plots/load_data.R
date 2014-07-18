# Load data for figures
library(RCurl)

# analytics

where_load <- function(load_it){

if(load_it=="github"){
	
	x=getURL("https://raw.githubusercontent.com/emfuller/MovingFish/master/plots/eqbiomass_gaus.csv?token=3235371__eyJzY29wZSI6IlJhd0Jsb2I6ZW1mdWxsZXIvTW92aW5nRmlzaC9tYXN0ZXIvcGxvdHMvZXFiaW9tYXNzX2dhdXMuY3N2IiwiZXhwaXJlcyI6MTQwMjk0OTk1OX0%3D--7f3a49902fd6f47b2b8cda6b306da6a86d348bd0")
ebm=read.csv(textConnection(x),row.names=1)
ebm=as.matrix(ebm,nrow=length(ebm[,1]))

# simulations

x = getURL("https://raw.githubusercontent.com/emfuller/MovingFish/master/Simluations/Aspatial_fast/Data/MPAnull_2014-06-04.csv?token=3235371__eyJzY29wZSI6IlJhd0Jsb2I6ZW1mdWxsZXIvTW92aW5nRmlzaC9tYXN0ZXIvU2ltbHVhdGlvbnMvQXNwYXRpYWxfZmFzdC9EYXRhL01QQW51bGxfMjAxNC0wNi0wNC5jc3YiLCJleHBpcmVzIjoxNDAyOTQ5OTA5fQ%3D%3D--ff1b6f98034f048ce14e19f908f34da14d7c8fcc")
	sim = read.csv(textConnection(x),row.names=1)

	x = getURL("https://raw.githubusercontent.com/emfuller/MovingFish/master/Simluations/Aspatial_fast/Data/MPAfish_NA_2014-06-05.csv?token=3235371__eyJzY29wZSI6IlJhd0Jsb2I6ZW1mdWxsZXIvTW92aW5nRmlzaC9tYXN0ZXIvU2ltbHVhdGlvbnMvQXNwYXRpYWxfZmFzdC9EYXRhL01QQWZpc2hfTkFfMjAxNC0wNi0wNS5jc3YiLCJleHBpcmVzIjoxNDAyOTQ5ODg2fQ%3D%3D--7e9625707bc33b373890fcdb0896448b7c7cd3c6")
	mpas = read.csv(textConnection(x), row.names=1)

x = getURL("https://raw.githubusercontent.com/emfuller/MovingFish/master/Simluations/Aspatial_fast/Data/MPAcons_NA_2014-06-02.csv?token=3235371__eyJzY29wZSI6IlJhd0Jsb2I6ZW1mdWxsZXIvTW92aW5nRmlzaC9tYXN0ZXIvU2ltbHVhdGlvbnMvQXNwYXRpYWxfZmFzdC9EYXRhL01QQWNvbnNfTkFfMjAxNC0wNi0wMi5jc3YiLCJleHBpcmVzIjoxNDAyOTQ5ODM4fQ%3D%3D--aed3c4a92c39f84a47289ea351899572f1ffb7bb")
	mpas2 = read.csv(textConnection(x), row.names=1)

x = getURL("https://raw.githubusercontent.com/emfuller/MovingFish/master/Simluations/Aspatial_fast/Data/Thresh_2014-06-06.csv?token=3235371__eyJzY29wZSI6IlJhd0Jsb2I6ZW1mdWxsZXIvTW92aW5nRmlzaC9tYXN0ZXIvU2ltbHVhdGlvbnMvQXNwYXRpYWxfZmFzdC9EYXRhL1RocmVzaF8yMDE0LTA2LTA2LmNzdiIsImV4cGlyZXMiOjE0MDI5NDk5Mjl9--0b46cc290706e13e37f2db47568f5752f1eb09ef")
	thresh = read.csv(textConnection(x), row.names=1)
}else{
	if(load_it=="local"){
		setwd("/Users/efuller/Documents/Projects/Moving_fish/MovingFish/Simluations/Aspatial_fast/Data/")
		sim = read.csv("MPAnull_2014-06-04.csv",row.names=1)
		mpas=read.csv("MPAfish_NA_2014-06-05.csv",row.names=1)
		mpas2 = read.csv("MPAcons_NA_2014-06-02.csv",row.names=1)
		thresh = read.csv("Thresh_2014-06-06.csv",row.names=1)
		ebm = read.csv("../../../plots/eqbiomass_gaus.csv",row.names=1)
		emb = as.matrix(ebm, nrow=length(ebm[,1]))
		mpacons_true <- read.csv("MPAcons_yes_2014-07-11.csv", row.names=1)
		mpafish_true <- read.csv("MPAfish_yes_2014-07-11.csv", row.names=1)
	}
}	
	
	return(list(sim,mpas,mpas2,thresh, mpacons_true, mpafish_true, ebm))
	}