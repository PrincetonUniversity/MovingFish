# Load data for figures
library(RCurl)

# analytics

x=getURL("https://raw.githubusercontent.com/emfuller/MovingFish/master/plots/eqbiomass_gaus.csv?token=6224444__eyJzY29wZSI6IlJhd0Jsb2I6ZW1mdWxsZXIvTW92aW5nRmlzaC9tYXN0ZXIvcGxvdHMvZXFiaW9tYXNzX2dhdXMuY3N2IiwiZXhwaXJlcyI6MTM5NjU1NDcwN30%3D--206b5aaf6a06b82faa6f082379ee975236cea6c8")
ebm=read.csv(textConnection(x),row.names=1)
ebm=as.matrix(ebm,nrow=length(ebm[,1]))

# simulations

x = getURL("https://raw.githubusercontent.com/emfuller/MovingFish/master/Simluations/Aspatial_fast/Data/MPAnull_2014-03-02.csv?token=6224444__eyJzY29wZSI6IlJhd0Jsb2I6ZW1mdWxsZXIvTW92aW5nRmlzaC9tYXN0ZXIvU2ltbHVhdGlvbnMvQXNwYXRpYWxfZmFzdC9EYXRhL01QQW51bGxfMjAxNC0wMy0wMi5jc3YiLCJleHBpcmVzIjoxMzk2NTU0NzUzfQ%3D%3D--a0b15d528bb977d63d2a881aba4616db7ab3299b")
	sim = read.csv(textConnection(x),row.names=1)

	x = getURL("https://raw.githubusercontent.com/emfuller/MovingFish/master/Simluations/Aspatial_fast/Data/MPAfish_2014-03-02.csv?token=3235371__eyJzY29wZSI6IlJhd0Jsb2I6ZW1mdWxsZXIvTW92aW5nRmlzaC9tYXN0ZXIvU2ltbHVhdGlvbnMvQXNwYXRpYWxfZmFzdC9EYXRhL01QQWZpc2hfMjAxNC0wMy0wMi5jc3YiLCJleHBpcmVzIjoxMzk2NTQyNTM5fQ%3D%3D--8170eb7f17025444daac72c56988416a845e3441")
	mpas = read.csv(textConnection(x), row.names=1)

x = getURL("https://raw.githubusercontent.com/emfuller/MovingFish/master/Simluations/Aspatial_fast/Data/MPAcons_2014-03-04.csv?token=3235371__eyJzY29wZSI6IlJhd0Jsb2I6ZW1mdWxsZXIvTW92aW5nRmlzaC9tYXN0ZXIvU2ltbHVhdGlvbnMvQXNwYXRpYWxfZmFzdC9EYXRhL01QQWNvbnNfMjAxNC0wMy0wNC5jc3YiLCJleHBpcmVzIjoxMzk2NTQyNTU1fQ%3D%3D--7372d01eb9a479b0221b384d017102c39963905d")
	mpas2 = read.csv(textConnection(x), row.names=1)

x = getURL("https://raw.githubusercontent.com/emfuller/MovingFish/master/Simluations/Aspatial_fast/Data/Thresh_2014-03-02.csv?token=3235371__eyJzY29wZSI6IlJhd0Jsb2I6ZW1mdWxsZXIvTW92aW5nRmlzaC9tYXN0ZXIvU2ltbHVhdGlvbnMvQXNwYXRpYWxfZmFzdC9EYXRhL1RocmVzaF8yMDE0LTAzLTAyLmNzdiIsImV4cGlyZXMiOjEzOTY1NDI1NzJ9--db664efb844f7721642408171cd462105d52d5bb")
	thresh = read.csv(textConnection(x), row.names=1)