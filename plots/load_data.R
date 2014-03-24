# Load data for figures
library(RCurl)

# analytics

x=getURL("https://raw.github.com/emfuller/MovingFish/master/eqbiomass_gaus.csv?token=3235371__eyJzY29wZSI6IlJhd0Jsb2I6ZW1mdWxsZXIvTW92aW5nRmlzaC9tYXN0ZXIvZXFiaW9tYXNzX2dhdXMuY3N2IiwiZXhwaXJlcyI6MTM5NTE4NTQ4N30%3D--65033562cc7e8c52f04dbf0ea06cae97e4008776")
ebm=read.csv(textConnection(x),row.names=1)
ebm=as.matrix(ebm,nrow=length(ebm[,1]))

# simulations

x = getURL("https://raw.githubusercontent.com/emfuller/MovingFish/master/Simluations/Aspatial_fast/Data/MPAnull_2014-03-02.csv?token=3235371__eyJzY29wZSI6IlJhd0Jsb2I6ZW1mdWxsZXIvTW92aW5nRmlzaC9tYXN0ZXIvU2ltbHVhdGlvbnMvQXNwYXRpYWxfZmFzdC9EYXRhL01QQW51bGxfMjAxNC0wMy0wMi5jc3YiLCJleHBpcmVzIjoxMzk2MzAyMTAzfQ%3D%3D--e0d48af04558661f5eb08b0b0fb433c5cb49cce2")
	sim = read.csv(textConnection(x),row.names=1)

	x = getURL("https://raw.githubusercontent.com/emfuller/MovingFish/master/Simluations/Aspatial_fast/Data/MPAfish_2014-03-02.csv?token=3235371__eyJzY29wZSI6IlJhd0Jsb2I6ZW1mdWxsZXIvTW92aW5nRmlzaC9tYXN0ZXIvU2ltbHVhdGlvbnMvQXNwYXRpYWxfZmFzdC9EYXRhL01QQWZpc2hfMjAxNC0wMy0wMi5jc3YiLCJleHBpcmVzIjoxMzk2MzAyMjAyfQ%3D%3D--2ffa1f31836051606220d2dcf43c552af582bb4a")
	mpas = read.csv(textConnection(x), row.names=1)

x = getURL("https://raw.githubusercontent.com/emfuller/MovingFish/master/Simluations/Aspatial_fast/Data/MPAcons_2014-03-04.csv?token=3235371__eyJzY29wZSI6IlJhd0Jsb2I6ZW1mdWxsZXIvTW92aW5nRmlzaC9tYXN0ZXIvU2ltbHVhdGlvbnMvQXNwYXRpYWxfZmFzdC9EYXRhL01QQWNvbnNfMjAxNC0wMy0wNC5jc3YiLCJleHBpcmVzIjoxMzk2MzAyMTc3fQ%3D%3D--2123ba5081f15a02f084be310e15ac4e222acb70")
	mpas2 = read.csv(textConnection(x), row.names=1)

x = getURL("https://raw.githubusercontent.com/emfuller/MovingFish/master/Simluations/Aspatial_fast/Data/Thresh_2014-03-02.csv?token=3235371__eyJzY29wZSI6IlJhd0Jsb2I6ZW1mdWxsZXIvTW92aW5nRmlzaC9tYXN0ZXIvU2ltbHVhdGlvbnMvQXNwYXRpYWxfZmFzdC9EYXRhL1RocmVzaF8yMDE0LTAzLTAyLmNzdiIsImV4cGlyZXMiOjEzOTYzMDIyMjF9--2de249c32a013ed15f229d3e2e49a30810332361")
	thresh = read.csv(textConnection(x), row.names=1)