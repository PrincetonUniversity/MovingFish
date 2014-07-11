library(fields)
library(lattice)
library(colorspace)
library(RCurl)
source("biomass_synergy.R")
source("critical_parameters.R")
source("load_data.R")
require(stringr)
require(colorspace)
require(reshape2)
require(gridExtra)
require(ggplot2)
require(ggthemes)

where_load = "local"
data <- where_load("local")

sim <- data[[1]]
mpas <- data[[2]]
mpas2 <- data[[3]]
thresh <- data[[4]]

###################################### Figure 1
	analytical_ebm=ebm
	cvals=matrix(seq(0,1,by=0.01),nrow=1)

	nr=3
	ns=2
	rvals=c(rep(3,ns),rep(5,ns),rep(10,ns))
	rcols=c(rep('black',ns),rep('red',ns),rep('blue',ns))
	xvals=c(.1,.5)
	sigvals=rep(pi/2*xvals^2,nr)
	sigltys=rep(c(1,2),nr)
	l=length(rvals)

# save data as list
	h = vector("list",length=l)

	for(i in 1:l){
		r=rvals[i]
		sig2=sigvals[i]
		h[[i]]=apply(cvals,2,hstar_g,r=r,sig2=sig2)
	}
	
	melted_h <- melt(h)
	names(melted_h) <- c("h", "Param_combo")
	melted_h$speed <- rep(cvals, 6)
	melted_h$R <- rep(rvals, each = 101)
	melted_h$d <- round(rep(xvals, each =101),digits=2)

	plot1 <- ggplot(melted_h, aes(x=speed, y = h, group=Param_combo)) + 
		geom_line(aes(color=factor(R), linetype=factor(d)),size=1) + 
		theme_tufte() + 
		scale_color_manual(values=c("grey", "dark grey", "black")) + 
		theme(text=element_text(family="Helvetica", size=14), plot.margin=unit(c(0,0,0,0),"cm")) + 
		xlab("Climate Velocity") + ylab("Harvesting rate") + 
		labs(linetype=expression(symbol("\341")*d*symbol("\361")), color=expression("R"[0]), title="")
	
	png("Fig1.png",height=3,width=4,units="in",res=300)
	print(plot1)
	dev.off() 
	
################################################################## FIGURE 2

#### A
	melted_ebm <- melt(ebm)
	speeds <- rep(seq(0,1, 0.01), 100)
	harvests <- rep(seq(0.01,1,0.01), each = 101)
	melted_ebm$speed = speeds
	melted_ebm$harvest = harvests

	plot2a <- ggplot(melted_ebm, aes(x=speed, y = harvest, fill=value)) + 
		geom_tile() + theme_tufte() + 
		scale_fill_gradient(low="black", high="white") + 
		theme(text=element_text(family="Helvetica", size=14), plot.margin=unit(c(0,0,0,0),"cm")) + 
		guides(fill = guide_colorbar(barwidth=1, barheight=10, title.position="top", title="Equilibrium\nBiomass")) + 
		xlab("") + 
		ylab("Harvesting Rate") + 
		labs(title="A")

#### B

# making data
	ebm=analytical_ebm
	cvals=as.numeric(str_sub(row.names(ebm),start=3))
	hvals=as.numeric(str_sub(dimnames(ebm)[[2]],start=3))
	tol=.00001
	ebm=as.matrix(ebm,nrow=length(hvals))
	syn=synergy(ebm,2)
	syn=round(syn,3)
	syn[is.na(syn)]=0
	syn[syn<0]=-.03
	
# reshaping data for ggplot
	melt_syn <- melt(syn)
	names(melt_syn) <- c("Speed","Harvest","Synergy")
	speeds <- rep(seq(0.01,1, 0.01), 99)
	harvests <- rep(seq(0.02,1,0.01), each = 100)
	
	melt_syn$Speed = speeds
	melt_syn$Harvest = harvests
	
plot2b <- ggplot(melt_syn, aes(x=Speed, y = Harvest, fill=Synergy)) + 
	geom_tile() + 
	theme_tufte() + 
	scale_fill_gradient(low="black", high="white") + 
	theme(text=element_text(family="Helvetica", size=14), plot.margin=unit(c(0,0,0,0),"cm")) + 
	guides(fill = guide_colorbar(barwidth=1, barheight=10, title.position="top", title="Synergy")) + 
	xlab("") + 
	ylab("")  + 
	theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + labs(title="B")

#### Together
png("Fig2.png",height=3, width=8,units="in",res=300)
grid.arrange(plot2a, plot2b, ncol=2, sub = textGrob("Climate velocity", just="bottom"))
dev.off() 

################################################################### Figure 3
# data
	threshz = thresh
	fishmpa = mpas
	consmpa = mpas2

	sim$management = rep("No MPAs",nrow(sim))
	consmpa$management = rep("Few Large MPAs", nrow(consmpa))
	fishmpa$management = rep("Many Small MPAs", nrow(fishmpa))
	threshz$management = rep("Thresholds", nrow(threshz))
	threshz$ord_thresh <- threshz$thresh/max(threshz$thresh)
	plotA <- ggplot(sim, aes(x=speed, y = harvest, fill = Equil.pop)) + geom_raster(interpolate=TRUE) + theme_tufte() + scale_fill_gradient(low="black", high="gray95")  + labs(title="A") + xlab("") + ylab("") + theme(text=element_text(family="Helvetica", size=14), plot.margin=unit(c(0,0,0,0),"cm"), legend.position="none") 


	plotB <- ggplot(threshz, aes(x=speed, y = ord_thresh, fill = Equil.pop)) + geom_raster(interpolate=TRUE) + theme_tufte() + scale_fill_gradient(low="black", high="gray95") + labs(title="B")+ xlab("") + ylab("")+ theme(text=element_text(family="Helvetica", size=14), plot.margin=unit(c(0,0,0,0),"cm"),legend.position="none") + scale_y_reverse()


	
	plotC <-ggplot(fishmpa, aes(x=speed, y = harvest, fill = Equil.pop)) + 
		geom_raster(interpolate=TRUE) + theme_tufte() + 
		scale_fill_gradient(low="black", high="gray95")  + 
		labs(title="C") + xlab("") + ylab("") + 
		theme(text=element_text(family="Helvetica", size=14), plot.margin=unit(c(.0,0,0,0),"cm"),legend.position="none") 


	
	plotD <- ggplot(consmpa, aes(x=speed, y = harvest, fill = Equil.pop)) + 
		geom_raster(interpolate=TRUE) + theme_tufte() + 
		scale_fill_gradient(low="black", high="gray95") + 
		labs(title="D")+ xlab("") + ylab("") + 
		theme(legend.position="right", text=element_text(family="Helvetica", size = 14),plot.margin=unit(c(.0,0,0,0),"cm")) + 
		guides(fill = guide_colorbar(barwidth = 1, barheight = 18, title.position="top", title="Equilibrium\nBiomass"))
			
	
# grabbing the legend from the last plot

library(gridExtra)
g_legend<-function(a.gplot){
tmp <- ggplot_gtable(ggplot_build(a.gplot))
leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
legend <- tmp$grobs[[leg]]
return(legend)}

legend <- g_legend(plotD)
lwidth <- sum(legend$width)

png(file="fig3.png",width=8,height=6,res=300,units="in")
		grid.arrange(arrangeGrob(plotA, plotB , plotC,plotD + theme(legend.position="none")), legend, left ="\nHarvest", sub="Climate velocity\n", widths=unit.c(unit(1, "npc") - lwidth,lwidth),ncol=2)
		dev.off()

############## Appendix

#### Additional harvest simulations: compensation

source("append_data.R")


# data
	fishmpa = fish_TRUE
	consmpa = cons_TRUE

	consmpa$management = rep("Few Large MPAs", nrow(consmpa))
	fishmpa$management = rep("Many Small MPAs", nrow(fishmpa))
	
	plotA_fish <-ggplot(fishmpa, aes(x=speed, y = harvest, fill = Equil.pop)) + 
		geom_raster(interpolate=TRUE) + theme_tufte() + 
		scale_fill_gradient(low="black", high="gray95")  + 
		labs(title="A") + xlab("") + ylab("") + 
		theme(text=element_text(family="Helvetica", size=14), plot.margin=unit(c(.0,0,0,0),"cm"),legend.position="none") 


	
	plotA_cons <- ggplot(consmpa, aes(x=speed, y = harvest, fill = Equil.pop)) + 
		geom_raster(interpolate=TRUE) + theme_tufte() + 
		scale_fill_gradient(low="black", high="gray95") + 
		labs(title="B")+ xlab("") + ylab("") + 
		theme(legend.position="right", text=element_text(family="Helvetica", size = 14),plot.margin=unit(c(.0,0,0,0),"cm")) + 
		guides(fill = guide_colorbar(barwidth = 1, barheight = 18, title.position="top", title="Equilibrium\nBiomass"))
			
	
# grabbing the legend from the last plot

legend <- g_legend(plotA_cons)
lwidth <- sum(legend$width)

png(file="figA_mpa.png",width=6,height=6,res=300,units="in")
		grid.arrange(arrangeGrob(plotA_fish,plotA_cons + theme(legend.position="none")), legend, left ="\nHarvest", sub="Climate velocity\n", widths=unit.c(unit(1, "npc") - lwidth,lwidth),ncol=2)
		dev.off()

# checking there's a difference between fish and cons MPA

diff_mpa <- mpas2
diff_mpa$Equil.pop <- mpas2$Equil.pop-mpas$Equil.pop

ggplot(diff_mpa, aes(x=speed, y=harvest, fill=Equil.pop)) + geom_tile() # very little

# what about harvest re_allocated

diff_TRUE <- consmpa
diff_TRUE$Equil.pop <- consmpa$Equil.pop - fishmpa$Equil.pop

ggplot(diff_TRUE, aes(x=speed, y = harvest, fill=Equil.pop)) + geom_tile() # looks the same

# what about between harvest re-compensated

consTrue <- data[[5]]# not sure why there are NAs, making them 0
consTrue$Equil.pop[is.na(consTrue$Equil.pop)] <- 0
fishTrue <- data[[6]]

#compare cons without effort reallocated and effort removed
diff_cons <- mpas2
diff_cons$Equil.pop <- diff_cons$Equil.pop - consTrue$Equil.pop

ggplot(diff_cons, aes(x=speed, y=harvest, fill=Equil.pop)) + geom_tile() # no difference. 

diff_fish <- mpas
diff_fish$Equil.pop <- diff_fish$Equil.pop - fishTrue$Equil.pop
ggplot(diff_fish, aes(x=speed, y=harvest, fill=Equil.pop)) + geom_tile()

# generate plot for appendix using reallocated effort

	consTrue$management = rep("Few Large MPAs", nrow(consTrue))
	fishTrue$management = rep("Many Small MPAs", nrow(fishTrue))

plotA <- ggplot(sim, aes(x=speed, y = harvest, fill = Equil.pop)) + geom_raster(interpolate=TRUE) + theme_tufte() + scale_fill_gradient(low="black", high="gray95")  + labs(title="A") + xlab("") + ylab("") + theme(text=element_text(family="Helvetica", size=14), plot.margin=unit(c(0,0,0,0),"cm"), legend.position="none") 


	plotB <- ggplot(threshz, aes(x=speed, y = ord_thresh, fill = Equil.pop)) + geom_raster(interpolate=TRUE) + theme_tufte() + scale_fill_gradient(low="black", high="gray95") + labs(title="B")+ xlab("") + ylab("")+ theme(text=element_text(family="Helvetica", size=14), plot.margin=unit(c(0,0,0,0),"cm"),legend.position="none") + scale_y_reverse()


	
	plotC <-ggplot(fishTrue, aes(x=speed, y = harvest, fill = Equil.pop)) + 
		geom_raster(interpolate=TRUE) + theme_tufte() + 
		scale_fill_gradient(low="black", high="gray95")  + 
		labs(title="C") + xlab("") + ylab("") + 
		theme(text=element_text(family="Helvetica", size=14), plot.margin=unit(c(.0,0,0,0),"cm"),legend.position="none") 


	
	plotD <- ggplot(consTrue, aes(x=speed, y = harvest, fill = Equil.pop)) + 
		geom_raster(interpolate=TRUE) + theme_tufte() + 
		scale_fill_gradient(low="black", high="gray95") + 
		labs(title="D")+ xlab("") + ylab("") + 
		theme(legend.position="right", text=element_text(family="Helvetica", size = 14),plot.margin=unit(c(.0,0,0,0),"cm")) + 
		guides(fill = guide_colorbar(barwidth = 1, barheight = 18, title.position="top", title="Equilibrium\nBiomass"))
			
	
# grabbing the legend from the last plot

library(gridExtra)
g_legend<-function(a.gplot){
tmp <- ggplot_gtable(ggplot_build(a.gplot))
leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
legend <- tmp$grobs[[leg]]
return(legend)}

legend <- g_legend(plotD)
lwidth <- sum(legend$width)

png(file="fig3.png",width=8,height=6,res=300,units="in")
		grid.arrange(arrangeGrob(plotA, plotB , plotC,plotD + theme(legend.position="none")), legend, left ="\nHarvest", sub="Climate velocity\n", widths=unit.c(unit(1, "npc") - lwidth,lwidth),ncol=2)
		dev.off()
