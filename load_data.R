# Load data for figures

# analytics

x=getURL("https://raw.github.com/emfuller/MovingFish/38c6f3bb51dae267fe4056159609de100fb34826/eqbiomass_gaus.csv?token=6224444__eyJzY29wZSI6IlJhd0Jsb2I6ZW1mdWxsZXIvTW92aW5nRmlzaC8zOGM2ZjNiYjUxZGFlMjY3ZmU0MDU2MTU5NjA5ZGUxMDBmYjM0ODI2L2VxYmlvbWFzc19nYXVzLmNzdiIsImV4cGlyZXMiOjEzOTQ0ODQ5OTh9--d95944353679e18e44def40cd49ca41a22502039")
ebm=read.csv(textConnection(x))

# simulations

x = getURL("https://raw.github.com/emfuller/MovingFish/master/Simluations/Aspatial_fast/Data/MPAnull_2014-03-02.csv?token=3235371__eyJzY29wZSI6IlJhd0Jsb2I6ZW1mdWxsZXIvTW92aW5nRmlzaC9tYXN0ZXIvU2ltbHVhdGlvbnMvQXNwYXRpYWxfZmFzdC9EYXRhL01QQW51bGxfMjAxNC0wMy0wMi5jc3YiLCJleHBpcmVzIjoxMzk0NDA4NjYyfQ%3D%3D--7f380bfb17f1cd58ff4f131134ed434700767def")
	sim = read.csv(textConnection(x))

	x = getURL("https://raw.github.com/emfuller/MovingFish/master/Simluations/Aspatial_fast/Data/MPAfish_2014-03-02.csv?token=3235371__eyJzY29wZSI6IlJhd0Jsb2I6ZW1mdWxsZXIvTW92aW5nRmlzaC9tYXN0ZXIvU2ltbHVhdGlvbnMvQXNwYXRpYWxfZmFzdC9EYXRhL01QQWZpc2hfMjAxNC0wMy0wMi5jc3YiLCJleHBpcmVzIjoxMzk0NDA4NjQxfQ%3D%3D--2dba33c2046114811967e01914fc795e1771f425")
	mpas = read.csv(textConnection(x))

x = getURL("https://raw.github.com/emfuller/MovingFish/master/Simluations/Aspatial_fast/Data/MPAcons_2014-03-04.csv?token=3235371__eyJzY29wZSI6IlJhd0Jsb2I6ZW1mdWxsZXIvTW92aW5nRmlzaC9tYXN0ZXIvU2ltbHVhdGlvbnMvQXNwYXRpYWxfZmFzdC9EYXRhL01QQWNvbnNfMjAxNC0wMy0wNC5jc3YiLCJleHBpcmVzIjoxMzk0NTU5MTY0fQ%3D%3D--dbd27e32e8632468f8c1113dbd4b470bba84a535")
	mpas2 = read.csv(textConnection(x))

x = getURL("https://raw.github.com/emfuller/MovingFish/master/Simluations/Aspatial_fast/Data/Thresh_2014-03-02.csv?token=3235371__eyJzY29wZSI6IlJhd0Jsb2I6ZW1mdWxsZXIvTW92aW5nRmlzaC9tYXN0ZXIvU2ltbHVhdGlvbnMvQXNwYXRpYWxfZmFzdC9EYXRhL1RocmVzaF8yMDE0LTAzLTAyLmNzdiIsImV4cGlyZXMiOjEzOTQ0MDg2OTF9--50c2acb25198937b8f95463cd2713c2db8f124b2")
	thresh = read.csv(textConnection(x))