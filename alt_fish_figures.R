# for synced online github data
x = getURL("https://raw.github.com/emfuller/MovingFish/master/Simluations/Aspatial_fast/Data/MPAnull_2014-03-02.csv?token=3235371__eyJzY29wZSI6IlJhd0Jsb2I6ZW1mdWxsZXIvTW92aW5nRmlzaC9tYXN0ZXIvU2ltbHVhdGlvbnMvQXNwYXRpYWxfZmFzdC9EYXRhL01QQW51bGxfMjAxNC0wMy0wMi5jc3YiLCJleHBpcmVzIjoxMzk0NDc3MzYwfQ%3D%3D--5fa73caaafffae7c95d930f61def04ee9b96930a")
sim = read.csv(textConnection(x))

fish = getURL("https://raw.github.com/emfuller/MovingFish/master/Simluations/Aspatial_fast/Data/MPAfish_2014-03-02.csv?token=3235371__eyJzY29wZSI6IlJhd0Jsb2I6ZW1mdWxsZXIvTW92aW5nRmlzaC9tYXN0ZXIvU2ltbHVhdGlvbnMvQXNwYXRpYWxfZmFzdC9EYXRhL01QQWZpc2hfMjAxNC0wMy0wMi5jc3YiLCJleHBpcmVzIjoxMzk0NDc3MzQ2fQ%3D%3D--9b6ceb0092173222543c95381dfbb0f42f0c884f")
fishmpa = read.csv(textConnection(fish))

cons = getURL("https://raw.github.com/emfuller/MovingFish/master/Simluations/Aspatial_fast/Data/MPAcons_2014-03-01.csv?token=3235371__eyJzY29wZSI6IlJhd0Jsb2I6ZW1mdWxsZXIvTW92aW5nRmlzaC9tYXN0ZXIvU2ltbHVhdGlvbnMvQXNwYXRpYWxfZmFzdC9EYXRhL01QQWNvbnNfMjAxNC0wMy0wMS5jc3YiLCJleHBpcmVzIjoxMzk0NDc3MzIwfQ%3D%3D--5f4f584256a4175eebe2ef5465e83c92ebe65cc0")
consmpa = read.csv(textConnection(cons))

thresh = getURL("https://raw.github.com/emfuller/MovingFish/master/Simluations/Aspatial_fast/Data/Thresh_2014-03-02.csv?token=3235371__eyJzY29wZSI6IlJhd0Jsb2I6ZW1mdWxsZXIvTW92aW5nRmlzaC9tYXN0ZXIvU2ltbHVhdGlvbnMvQXNwYXRpYWxfZmFzdC9EYXRhL1RocmVzaF8yMDE0LTAzLTAyLmNzdiIsImV4cGlyZXMiOjEzOTQ0ODIyMTV9--13d78257cda683d18eb8ecc71dcf0f4e6101b41d")
threshz = read.csv(textConnection(thresh))

# combine all data into one dataframe
sim$management = rep("No MPAs",nrow(sim))
consmpa$management = rep("Few Large MPAs", nrow(consmpa))
fishmpa$management = rep("Many Small MPAs", nrow(fishmpa))
threshz$management = rep("Thresholds", nrow(threshz))
allData <- rbind(consmpa,fishmpa)


# lattice plot settings

sb <- trellis.par.get("strip.background") 
sb[["col"]][1] <- "lightgray"
trellis.par.set("strip.background", sb) 

ltheme <- canonical.theme(color = FALSE) ## in-built B&W theme 
ltheme$strip.background$col <- "transparent" ## change strip bg 
lattice.options(default.theme = ltheme) ## set as default 

lw <- list(left.padding = list(x = 0, units = "inches")) 
lw$right.padding <- list(x = -0.1, units = "inches") 
lh <- list(bottom.padding = list(x = 0, units = "inches")) 
lh$top.padding <- list(x = -0.2, units = "inches")

lattice.options(layout.widths = lw, layout.heights = lh) 

# mpa plots

pdf(file="Writing/plots/MPAs.pdf",width=3,height=5)
contourplot(Equil.pop ~ speed * harvest | factor(management), data = allData, region=TRUE, layout = c(1,2), drape=TRUE,col.regions=rev(colorRampPalette(brewer.pal(9,"Greys"))(100)),colorkey=FALSE,labels=list(cex=.75,col="white"))
dev.off()

#threshold plot

pdf(file="Writing/plots/Threshold.pdf",width=3,height=3)
contourplot(Equil.pop ~ speed * -thresh | factor(management), data = threshz, region=TRUE, drape=TRUE,col.regions=rev(colorRampPalette(brewer.pal(9,"Greys"))(100)),colorkey=FALSE,labels=list(cex=.75,col="white"),scales=list( y=list(at=seq(-8,0,2),labels=round(rev(unique(threshz$thresh)/max(threshz$thresh)),3))),ylab=list(label="proportion below threshold"))
dev.off()
