# Load data for figures
library(RCurl)

# analytics

x=getURL("https://raw.github.com/emfuller/MovingFish/d6b73bca90c71f0aedb616a241df24b536e0f80b/eqbiomass_gaus.csv?token=6224444__eyJzY29wZSI6IlJhd0Jsb2I6ZW1mdWxsZXIvTW92aW5nRmlzaC9kNmI3M2JjYTkwYzcxZjBhZWRiNjE2YTI0MWRmMjRiNTM2ZTBmODBiL2VxYmlvbWFzc19nYXVzLmNzdiIsImV4cGlyZXMiOjEzOTQ1NTkzNjF9--9f57d5392b49fe11ef00599873ecc6b66926ba10")
ebm=read.csv(textConnection(x),row.names=1)
ebm=as.matrix(ebm,nrow=length(ebm[,1]))

# simulations

x = getURL("https://raw.github.com/emfuller/MovingFish/master/Simluations/Aspatial_fast/Data/MPAnull_2014-03-02.csv?token=3235371__eyJzY29wZSI6IlJhd0Jsb2I6ZW1mdWxsZXIvTW92aW5nRmlzaC9tYXN0ZXIvU2ltbHVhdGlvbnMvQXNwYXRpYWxfZmFzdC9EYXRhL01QQW51bGxfMjAxNC0wMy0wMi5jc3YiLCJleHBpcmVzIjoxMzk0NDA4NjYyfQ%3D%3D--7f380bfb17f1cd58ff4f131134ed434700767def")
	sim = read.csv(textConnection(x),row.names=1)

	x = getURL("https://raw.github.com/emfuller/MovingFish/master/Simluations/Aspatial_fast/Data/MPAfish_2014-03-02.csv?token=3235371__eyJzY29wZSI6IlJhd0Jsb2I6ZW1mdWxsZXIvTW92aW5nRmlzaC9tYXN0ZXIvU2ltbHVhdGlvbnMvQXNwYXRpYWxfZmFzdC9EYXRhL01QQWZpc2hfMjAxNC0wMy0wMi5jc3YiLCJleHBpcmVzIjoxMzk0NDA4NjQxfQ%3D%3D--2dba33c2046114811967e01914fc795e1771f425")
	mpas = read.csv(textConnection(x))

x = getURL("https://raw.github.com/emfuller/MovingFish/master/Simluations/Aspatial_fast/Data/MPAcons_2014-03-04.csv?token=3235371__eyJzY29wZSI6IlJhd0Jsb2I6ZW1mdWxsZXIvTW92aW5nRmlzaC9tYXN0ZXIvU2ltbHVhdGlvbnMvQXNwYXRpYWxfZmFzdC9EYXRhL01QQWNvbnNfMjAxNC0wMy0wNC5jc3YiLCJleHBpcmVzIjoxMzk0NTUzMjQxfQ%3D%3D--9ddfd59a719d0410fa72a83cca37a0e789ea0d34")
	mpas2 = read.csv(textConnection(x))

x = getURL("https://raw.github.com/emfuller/MovingFish/master/Simluations/Aspatial_fast/Data/Thresh_2014-03-02.csv?token=3235371__eyJzY29wZSI6IlJhd0Jsb2I6ZW1mdWxsZXIvTW92aW5nRmlzaC9tYXN0ZXIvU2ltbHVhdGlvbnMvQXNwYXRpYWxfZmFzdC9EYXRhL1RocmVzaF8yMDE0LTAzLTAyLmNzdiIsImV4cGlyZXMiOjEzOTQ0MDg2OTF9--50c2acb25198937b8f95463cd2713c2db8f124b2")
	thresh = read.csv(textConnection(x))