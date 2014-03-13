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

###################################### Figure 1

### A
	analytical_ebm=ebm
	cvals=matrix(seq(0,1,by=0.01),nrow=1)

	nr=3
	ns=2
	rvals=c(rep(3,ns),rep(5,ns),rep(10,ns))
	rcols=c(rep('black',ns),rep('red',ns),rep('blue',ns))
	xvals=c(.1,.25)
	sigvals=rep(pi/2*xvals,nr)
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
	melted_h$d <- round(rep(sigvals, each =101),digits=2)

	plot1 <- ggplot(melted_h, aes(x=speed, y = h, group=Param_combo)) + 
		geom_line(aes(color=factor(R), linetype=factor(d)),size=1) + 
		theme_tufte() + 
		scale_color_manual(values=c("grey", "dark grey", "black")) + 
		theme(text=element_text(family="Helvetica", size=14), plot.margin=unit(c(1,0,0,0),"cm")) + 
		xlab("") + ylab("Harvesting rate") + 
		labs(linetype=expression(symbol("\341")*d*symbol("\361")), color=expression("R"[0]), title="A")
	

#### B
	melted_ebm <- melt(ebm)
	speeds <- rep(seq(0,1, 0.01), 101)
	harvests <- rep(seq(0,1,0.01), each = 101)
	melted_ebm$speed = speeds
	melted_ebm$harvest = harvests

	plot2 <- ggplot(melted_ebm, aes(x=speed, y = harvest, fill=value)) + 
		geom_tile() + theme_tufte() + 
		scale_fill_gradient(low="black", high="white") + 
		theme(text=element_text(family="Helvetica", size=14), plot.margin=unit(c(1,0,0,0),"cm")) + 
		guides(fill = guide_colorbar(barwidth=1, barheight=10, title.position="top", title="Equilibrium\nBiomass")) + 
		xlab("") + 
		ylab("") + 
		labs(title="B")

### TOGETHER
	grid.arrange(plot1, plot2, ncol=2, sub = textGrob("Climate velocity", just="bottom"))

################################################################## FIGURE 2
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
	speeds <- rep(seq(0.01,1, 0.01), 100)
	harvests <- rep(seq(0.01,1,0.01), each = 100)
	
	melt_syn$Speed = speeds
	melt_syn$Harvest = harvests
	
ggplot(melt_syn, aes(x=Speed, y = Harvest, fill=Synergy)) + 
	geom_tile() + 
	theme_tufte() + 
	scale_fill_gradient(low="black", high="white") + 
	theme(text=element_text(family="Helvetica", size=14), plot.margin=unit(c(0,0,0,0),"cm")) + 
	guides(fill = guide_colorbar(barwidth=1, barheight=10, title.position="top", title="Synergy")) + 
	xlab("Climate velocity") + 
	ylab("Harvesting rate")  + 
	opts(panel.grid.major=theme_blank(), panel.grid.minor=theme_blank())


################################################################### Figure 3
# data
	threshz = thresh
	fishmpa = mpas
	consmpa = mpas2

	sim$management = rep("No MPAs",nrow(sim))
	consmpa$management = rep("Few Large MPAs", nrow(consmpa))
	fishmpa$management = rep("Many Small MPAs", nrow(fishmpa))
	threshz$management = rep("Thresholds", nrow(threshz))
	allData <- rbind(consmpa,fishmpa, sim)

	ggplot(allData, aes(x = speed, y = harvest, fill = Equil.pop)) + 
		geom_raster(interpolate=TRUE) + 
		facet_wrap(~management, scales="free") + 
		theme_tufte() + 
		scale_fill_gradient(low="black", high="white") + 
		theme(legend.position="bottom", text=element_text(family="Helvetica", size = 11)) + 
		guides(fill = guide_colorbar(barwidth = 18, barheight = 1, title.position="top", title="Equilibrium Biomass"))




