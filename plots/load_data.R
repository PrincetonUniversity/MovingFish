# Load data for figures
library(RCurl)

# analytics

x=getURL("https://raw.github.com/emfuller/MovingFish/master/eqbiomass_gaus.csv?token=3235371__eyJzY29wZSI6IlJhd0Jsb2I6ZW1mdWxsZXIvTW92aW5nRmlzaC9tYXN0ZXIvZXFiaW9tYXNzX2dhdXMuY3N2IiwiZXhwaXJlcyI6MTM5NTE4NTQ4N30%3D--65033562cc7e8c52f04dbf0ea06cae97e4008776")
ebm=read.csv(textConnection(x),row.names=1)
ebm=as.matrix(ebm,nrow=length(ebm[,1]))

# simulations

x = getURL("https://raw.github.com/emfuller/MovingFish/master/Simluations/Aspatial_fast/Data/MPAnull_2014-03-02.csv?token=3235371__eyJzY29wZSI6IlJhd0Jsb2I6ZW1mdWxsZXIvTW92aW5nRmlzaC9tYXN0ZXIvU2ltbHVhdGlvbnMvQXNwYXRpYWxfZmFzdC9EYXRhL01QQW51bGxfMjAxNC0wMy0wMi5jc3YiLCJleHBpcmVzIjoxMzk1MTg1MzA2fQ%3D%3D--8e3f018b7a2cdc64244831017565809335d2e160")
	sim = read.csv(textConnection(x),row.names=1)

	x = getURL("https://raw.github.com/emfuller/MovingFish/master/Simluations/Aspatial_fast/Data/MPAfish_2014-03-02.csv?token=3235371__eyJzY29wZSI6IlJhd0Jsb2I6ZW1mdWxsZXIvTW92aW5nRmlzaC9tYXN0ZXIvU2ltbHVhdGlvbnMvQXNwYXRpYWxfZmFzdC9EYXRhL01QQWZpc2hfMjAxNC0wMy0wMi5jc3YiLCJleHBpcmVzIjoxMzk1MTg1MjYzfQ%3D%3D--78007e9dd447a7c3232f532eae1e0b82e0037767")
	mpas = read.csv(textConnection(x), row.names=1)

x = getURL("https://raw.github.com/emfuller/MovingFish/master/Simluations/Aspatial_fast/Data/MPAcons_2014-03-04.csv?token=3235371__eyJzY29wZSI6IlJhd0Jsb2I6ZW1mdWxsZXIvTW92aW5nRmlzaC9tYXN0ZXIvU2ltbHVhdGlvbnMvQXNwYXRpYWxfZmFzdC9EYXRhL01QQWNvbnNfMjAxNC0wMy0wNC5jc3YiLCJleHBpcmVzIjoxMzk1MTg0OTk5fQ%3D%3D--ec3e0f05373e64ef2f192b2ad265ca061749c738")
	mpas2 = read.csv(textConnection(x), row.names=1)

x = getURL("https://raw.github.com/emfuller/MovingFish/master/Simluations/Aspatial_fast/Data/Thresh_2014-03-02.csv?token=3235371__eyJzY29wZSI6IlJhd0Jsb2I6ZW1mdWxsZXIvTW92aW5nRmlzaC9tYXN0ZXIvU2ltbHVhdGlvbnMvQXNwYXRpYWxfZmFzdC9EYXRhL1RocmVzaF8yMDE0LTAzLTAyLmNzdiIsImV4cGlyZXMiOjEzOTUxODUzNzR9--c90173573d965821e9a6976a8f7be242d14544f5")
	thresh = read.csv(textConnection(x), row.names=1)