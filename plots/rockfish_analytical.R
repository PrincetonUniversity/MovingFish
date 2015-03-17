library(fields)
library(lattice)
library(colorspace)
library(RCurl)
source("biomass_synergy.r")
source("critical_parameters.r")
require(stringr)
require(colorspace)
require(reshape2)
require(gridExtra)
require(ggplot2)
require(ggthemes)

d=73
sig2=pi/2*d^2
R=2.86
K=1
L=1000
c=1

lambdag(10,R,sig2,L)
# cstar=cstar_g(R,sig2,L)
cstar=154

cvals=seq(0,cstar,length.out=26)
Nc=length(cvals)
hvals=seq(0,1,length.out=26)
crit_hvals=array(,Nc)

for(i in 1:Nc){
	c=cvals[i]
		hstar=hstar_g_big(c,R,sig2,L)
		crit_hvals[i]=hstar
}

gen_time=7	
htoplot=data.frame(h=crit_hvals,speed=cvals/gen_time*10)

	
# plot(cvals,hvals)	

rockfishplot1 <- ggplot(htoplot, aes(x=speed, y = h)) + 
		geom_line(size=1) + 
		theme_tufte() + 
		# scale_color_manual(values=c("grey", "dark grey", "black")) + 
		theme(text=element_text(family="Helvetica", size=10)) + 	
		theme(axis.line = element_line(colour = 'black')) +
		scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))+
		 expand_limits(y = c(0,1), x=c(0,300)) +
		xlab("Climate velocity (km/decade)") + ylab("Harvesting rate") 

# png("rockfish_criticalh.png",height=3,width=4,units="in",res=300)	
pdf(file="FigA1.pdf",width=2.9,height=2)
print(rockfishplot1)
dev.off()
# png("rockfish_criticalh.png",height=3, width=4,units='in',res=300)
# print(rockfishplot1)
# dev.off()
	
# rockfish_analytical=eqbiomass(cvals,hvals,R,K,sig2,L,tol=NA)
rockfish_analytical=read.csv('rockfish_analytical_ebm.csv',sep='\t')
# making data
	ebm=rockfish_analytical
	ebm=as.matrix(ebm,nrow=length(hvals))
	melted_ebm <- melt(ebm)
	num=dim(ebm)[1]
	speeds <- rep(cvals, num)
	harvests <- rep(hvals, each = num)
	melted_ebm$Speed = speeds/gen_time*10
	melted_ebm$Harvest = harvests

	syn=synergy(ebm,2)
	syn=round(syn,3)
	syn[is.na(syn)]=0
	syn[syn<0]=-.03
	
	melt_syn <- melt(syn)
	names(melt_syn) <- c("Speed","Harvest","Synergy")	
	speeds <- rep(cvals[2:num], num-1)/gen_time*10
	harvests <- rep(hvals[2:num], each = num-1)
	melt_syn$Speed = speeds
	melt_syn$Harvest = harvests
	
  # change any negative values to 0, due to numerical rounding error - EB
  melt_syn$Synergy[which(melt_syn$Synergy<0)] = 0
  
  rockfishebmplot <- ggplot(melted_ebm, aes(x=Speed, y = Harvest, fill=value)) + 
		geom_tile() + theme_tufte() + 
		scale_fill_gradient(low="black", high="white") + 
		theme(text=element_text(family="Helvetica", size=10), plot.margin=unit(c(0,0,0,0),"cm")) + 
		scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))+
		guides(fill = guide_colorbar(barwidth=1, barheight=10, title.position="top", title="Equilibrium\nbiomass")) + 
		xlab("") + 
		ylab("Harvesting rate") + 
		labs(title="A")


rockfishsynplot <- ggplot(melt_syn, aes(x=Speed, y = Harvest, fill=Synergy)) + 
	geom_tile() + 
	theme_tufte() + 
	scale_fill_gradient(low="black", high="white") + 
	theme(text=element_text(family="Helvetica", size=10), plot.margin=unit(c(0,0,0,0),"cm")) + 
	scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))+
	guides(fill = guide_colorbar(barwidth=1, barheight=10, title.position="top", title="Synergy")) + 
	xlab("") + 
	ylab("")  + 
	theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + labs(title="B")

# #### Together
pdf("FigA2.pdf",height=2.6, width=6.1)
grid.arrange(rockfishebmplot, rockfishsynplot, ncol=2, sub = textGrob("Climate velocity (km/decade)", just="bottom",gp=gpar(fontsize=10)))
dev.off() 
# png("rockfisheqbiomass.png",height=3, width=8,units='in',res=300)
# grid.arrange(rockfishebmplot, rockfishsynplot, ncol=2, sub = textGrob("Climate velocity  (km/year)", just="bottom"))
# dev.off() 