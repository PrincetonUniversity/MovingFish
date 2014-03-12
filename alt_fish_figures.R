# for synced online github data
source("load_data.R")
require(lattice)
require(RColorBrewer)
threshz = thresh
fishmpa = mpas
consmpa = mpas2

# combine all data into one dataframe
sim$management = rep("No MPAs",nrow(sim))
consmpa$management = rep("Few Large MPAs", nrow(consmpa))
fishmpa$management = rep("Many Small MPAs", nrow(fishmpa))
threshz$management = rep("Thresholds", nrow(threshz))
allData <- rbind(consmpa,fishmpa, sim)


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
contourplot(Equil.pop ~ speed * harvest | factor(management), data = allData, region=TRUE, layout = c(2,2), drape=TRUE,col.regions=rev(colorRampPalette(brewer.pal(9,"Greys"))(100)),colorkey=FALSE,labels=list(cex=.75,col="white"))
dev.off()

#threshold plot

pdf(file="Writing/plots/Threshold.pdf",width=3,height=3)
contourplot(Equil.pop ~ speed * -thresh | factor(management), data = threshz, region=TRUE, drape=TRUE,col.regions=rev(colorRampPalette(brewer.pal(9,"Greys"))(100)),colorkey=FALSE,labels=list(cex=.75,col="white"),scales=list( y=list(at=seq(-8,0,2),labels=round(rev(unique(threshz$thresh)/max(threshz$thresh)),3))),ylab=list(label="proportion below threshold"))
dev.off()

### ggplots
require(ggplot2)
require(ggthemes)

ggplot(allData, aes(x = speed, y = harvest, fill = Equil.pop)) + geom_tile() + facet_wrap(~management, scale="free") + geom_tile(data = threshz, aes(x = speed, y = rev(thresh), fill = Equil.pop)) + theme_tufte() + scale_fill_gradient(low="black", high="white") + theme(legend.position="bottom", text=element_text(family="Helvetica", size = 14)) + guides(fill = guide_colorbar(barwidth = 18, barheight = 1, title.position="top", title="Equilibrium Biomass"))

