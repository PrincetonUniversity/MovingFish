# Load data for figures

# simulations

x = getURL("https://raw.githubusercontent.com/emfuller/MovingFish/master/Simluations/Aspatial_fast/Data/MPAcons_TRUE_2014-06-03.csv?token=3235371__eyJzY29wZSI6IlJhd0Jsb2I6ZW1mdWxsZXIvTW92aW5nRmlzaC9tYXN0ZXIvU2ltbHVhdGlvbnMvQXNwYXRpYWxfZmFzdC9EYXRhL01QQWNvbnNfVFJVRV8yMDE0LTA2LTAzLmNzdiIsImV4cGlyZXMiOjE0MDI5NTMwNzh9--365a025775983838780f900f604df9936434e232")
	cons_TRUE = read.csv(textConnection(x),row.names=1)
	
	
x = getURL("https://raw.githubusercontent.com/emfuller/MovingFish/master/Simluations/Aspatial_fast/Data/MPAfish_TRUE_2014-06-03.csv?token=3235371__eyJzY29wZSI6IlJhd0Jsb2I6ZW1mdWxsZXIvTW92aW5nRmlzaC9tYXN0ZXIvU2ltbHVhdGlvbnMvQXNwYXRpYWxfZmFzdC9EYXRhL01QQWZpc2hfVFJVRV8yMDE0LTA2LTAzLmNzdiIsImV4cGlyZXMiOjE0MDI5NTMxMDB9--b2b40b35467a5c7bf45ac95ab0eb43191aa71067")
	fish_TRUE = read.csv(textConnection(x),row.names=1)