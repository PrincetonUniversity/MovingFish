library(fields)
library(lattice)
library(colorspace)
library(RCurl)
source("biomass_synergy.R")
source("critical_parameters.R")
source("Simluations/Aspatial_fast/load_data.R")
require(stringr)
require(colorspace)
analytical_ebm=ebm

#####
#### plotting parameters
# cex.lab=3
# cex.axis=2
# line=5
# lwd=5
# yaxs='i'
# xaxs='i'
# cex.lab=15
# cex.axis=1
# line=1
# oma=c(0,0,0,2)

lwd=1
yaxs='i'
xaxs='i'
cex.lab=.75
cex.axis=.5
cex.legend=.5
tck=-.02
mar=c(2,2,1,1)
mgp=c(1,.25,0)

fig.width = 3	# for ecological applications, text needs to be at least 6 point font size

############
#############
#relationship between critical harvesting rate and critical climate velocity

cvals=matrix(seq(0,1,by=0.01),nrow=1)

nr=3
ns=2
rvals=c(rep(3,ns),rep(5,ns),rep(10,ns))
rcols=c(rep('black',ns),rep('red',ns),rep('blue',ns))
xvals=c(.1,.25)
sigvals=rep(pi/2*xvals,nr)
sigltys=rep(c(1,2),nr)
l=length(rvals)

quartz()
postscript("critical_rates.eps", horizontal = FALSE, onefile = FALSE, paper = "special", width = fig.width,height=fig.width,bg="white")

par(mar=mar,mgp=mgp)
plot(cvals,cvals,col='white',ylim=c(0,1),xlab="Climate velocity",ylab="Critical harvesting rate",bty='l',axes=FALSE,xaxs='i',yaxs='i',cex.lab=cex.lab)

for(i in 1:l){
	r=rvals[i]
	sig2=sigvals[i]
	h=apply(cvals,2,hstar_g,r=r,sig2=sig2)
	lines(cvals,h,col=rcols[i],lty=sigltys[i],lwd=lwd)
}

urvals=unique(rvals)
urcols=unique(rcols)
usigvals=unique(sigvals)
usigltys=unique(sigltys)

axis(1,lwd=lwd,cex.axis=cex.axis,tck=tck)
axis(2,lwd=lwd,cex.axis=cex.axis,tck=tck)

legend(.7,.97,col=c(urcols,rep('black',3),'white'),lty=c(rep(1,3),usigltys,1),lwd=lwd,legend=c(bquote(R[0]==.(urvals[1])),bquote(R[0]==.(urvals[2])), bquote(R[0]==.(urvals[3])),expression(symbol("\341")*d*symbol("\361")== .1),expression(symbol("\341")*d*symbol("\361")== .25),expression()),bty='n',cex=cex.legend)

graphics.off()

##########
##########
##### heatmap of the equilibrium biomass as a function of c and h

cvals=as.numeric(str_sub(row.names(ebm),start=3))
hvals=as.numeric(str_sub(dimnames(ebm)[[2]],start=3))

#which params to work with
tol=0.0001
H=sum(apply(ebm,2,max)>tol)+1
# hvals=hvals[1:H]
C=length(cvals)
toplot_ebm=as.matrix(ebm,nrow=C)
toplot_ebm[toplot_ebm<0.01]=0

#breaks and colors
lower=.001
cuts=c(0,lower,seq(.5,max(ebm)+.5,by=.5))
mycols=(sequential_hcl(length(cuts)-2,c.=c(0,0),l=c(30,100)))
mycols=c('black',mycols)

#axis labels
numxlabs=11
numylabs=3
xdiff=.1
ydiff=.2
myaxes=list(arrows=FALSE,x=list(at=rev(seq(1,C,length.out=(cvals[C]-cvals[1])/xdiff+1)),labels=as.character(round(seq(cvals[1],cvals[C],by=xdiff),2))),y=list(at=rev(seq(1,H,length.out=(hvals[H]-hvals[1])/ydiff)),labels=as.character(round(seq(hvals[1],hvals[H],by=ydiff),2))))

mgp=c(1,.4,0)
oma=c(0,0,0,1.5)

quartz()
postscript("eqbiomass.eps", horizontal = FALSE, onefile = FALSE, paper = "special", width = fig.width,height=fig.width,bg="white")

par(oma=oma,mar=mar,mgp=mgp)
image.plot(cvals,hvals,toplot_ebm,breaks=cuts,col=mycols,
	xlab="Climate velocity",ylab="Harvesting rate",
	cex.lab=cex.lab,cex.axis=cex.axis, 
	yaxs=yaxs,xaxs=xaxs,axes=TRUE,
	legend.shrink=1,legend.width=.1,zlim=range(cuts),
	axis.args=list(at=myaxes$z$at,labels=myaxes$z$labels,cex.axis=cex.axis,mgp=c(0,.6,0)),
	# legend.args=list(text="Equilibrium biomass",cex=cex.lab,side=2,line=0.05,las=0),
	bigplot=c(.13,.9,.15,.95),smallplot=c(.95,.99,.15,.95),horizontal=FALSE)
box()
graphics.off()

###########
###########
# synergy between harvesting and climate shift
ebm=analytical_ebm
cvals=as.numeric(str_sub(row.names(ebm),start=3))
hvals=as.numeric(str_sub(dimnames(ebm)[[2]],start=3))

tol=.00001

ebm=as.matrix(ebm,nrow=length(hvals))

syn=synergy(ebm,2)
syn=round(syn,3)

syn[is.na(syn)]=0
syn[syn<0]=-1

v=c(-2,seq(-.001,max(syn[!is.na(syn)]),length.out=100))
cols=(sequential_hcl(length(v)-2,h=260,c.=c(0,0),l=c(30,90),power=c(.9)))
cols=c('black',cols)

myaxes=list(arrows=FALSE,col=1,
z=list(at=round(seq(0,max(syn[!is.na(syn)]),by=.02),3),labels=as.character(round(seq(0,max(syn[!is.na(syn)]),by=.02),3))))

quartz()
postscript(file='synergy.eps',horizontal = FALSE, onefile = FALSE, paper = "special", width = fig.width,height=fig.width,bg="white")

par(oma=oma,mar=mar,mgp=mgp)
image.plot(cvals,hvals,syn,breaks=v,col=cols,
	xlab="Climate velocity",ylab="Harvesting rate",
	cex.lab=cex.lab,cex.axis=cex.axis, 
	yaxs=yaxs,xaxs=xaxs,axes=TRUE,
	legend.shrink=1,legend.width=.1,zlim=range(v[-1]),
	# axis.args=list(at=labs,labels=labs,cex.axis=cex.axis,mgp=c(0,.6,0)),
	axis.args=list(at=myaxes$z$at,labels=myaxes$z$labels,cex.axis=cex.axis,mgp=c(0,.6,0)),
	# legend.args=list(text="Synergy",cex=cex.lab,side=2,line=0.5,las=0),
	bigplot=c(.13,.9,.15,.95),smallplot=c(.95,.99,.15,.95),horizontal=FALSE)

	
	graphics.off()


##########
########## 
##### heatmap of the equilibrium biomass as a function of c and h : SIMULATIONS

cvals=unique(sim$speed)
hvals=unique(sim$harvest)

H=length(hvals)
C=length(cvals)
L=length(sim$Equil.pop)

ebm=array(,c(C,H))

for(i in 1:L){
	hind=which(hvals==sim$harvest[i])
	cind=which(cvals==sim$speed[i])
	ebm[cind,hind]=sim$Equil.pop[i]
}

toplot_ebm=ebm

#breaks and colors
lower=min(ebm)+1
cuts=c(0,seq(lower,max(ebm)+.5,length.out=100))
mycols=(sequential_hcl(length(cuts)-2,c.=c(0,0),l=c(30,100)))
mycols=c('black',mycols)

#axis labels
numxlabs=11
numylabs=3
xdiff=.1
ydiff=.2
myaxes=list(arrows=FALSE,
	x=list(at=rev(seq(1,C,length.out=(cvals[C]-cvals[1])/xdiff+1)),labels=as.character(round(seq(cvals[1],cvals[C],by=xdiff),2))),
	y=list(at=rev(seq(1,H,length.out=(hvals[H]-hvals[1])/ydiff)),labels=as.character(round(seq(hvals[1],hvals[H],by=ydiff),2))),
	z=list(at=seq(0,max(ebm),by=250),labels=as.character(seq(0,max(ebm),by=250))))

quartz()
postscript(file='eqbiomass_sim.eps', horizontal = FALSE, onefile = FALSE, paper = "special", width = fig.width,height=fig.width,bg="white")

par(oma=oma,mar=mar,mgp=mgp)
image.plot(cvals,hvals,toplot_ebm,breaks=cuts,col=mycols,
	xlab="Climate velocity",ylab="Harvesting rate",
	cex.lab=cex.lab,cex.axis=cex.axis, 
	yaxs=yaxs,xaxs=xaxs,axes=TRUE,
	legend.shrink=1,legend.width=.1,zlim=range(cuts),
	axis.args=list(at=myaxes$z$at,labels=myaxes$z$labels,cex.axis=cex.axis,mgp=c(0,.6,0)),
	#legend.args=list(text="Equilibrium biomass",cex=(cex.lab-1),side=2,line=0.5,las=0),
	bigplot=c(.13,.9,.15,.95),smallplot=c(.95,.99,.15,.95),horizontal=FALSE)
box()
graphics.off()

##########
##########
##### heatmap of the equilibrium biomass as a function of c and h : fisheries MPA

cvals=unique(mpas$speed)
hvals=unique(mpas$harvest)

H=length(hvals)
C=length(cvals)
L=length(mpas$Equil.pop)

ebm=array(,c(C,H))

for(i in 1:L){
	hind=which(hvals==mpas$harvest[i])
	cind=which(cvals==mpas$speed[i])
	ebm[cind,hind]=mpas$Equil.pop[i]
}

toplot_ebm=ebm

#breaks and colors
lower=min(ebm)+1
cuts=c(0,seq(lower,max(ebm)+.5,length.out=100))
mycols=(sequential_hcl(length(cuts)-2,c.=c(0,0),l=c(30,100)))
mycols=c('black',mycols)


#axis labels
numxlabs=11
numylabs=3
xdiff=.1
ydiff=.2
myaxes=list(arrows=FALSE,
	x=list(at=rev(seq(1,C,length.out=(cvals[C]-cvals[1])/xdiff+1)),labels=as.character(round(seq(cvals[1],cvals[C],by=xdiff),2))),
	y=list(at=rev(seq(1,H,length.out=(hvals[H]-hvals[1])/ydiff)),labels=as.character(round(seq(hvals[1],hvals[H],by=ydiff),2))),
	z=list(at=seq(0,max(ebm),by=250),labels=as.character(seq(0,max(ebm),by=250))))

quartz()
postscript(file='eqbiomass_fishmpa.eps', horizontal = FALSE, onefile = FALSE, paper = "special", width = fig.width,height=fig.width,bg="white")

par(oma=oma,mar=mar,mgp=mgp)
image.plot(cvals,hvals,toplot_ebm,breaks=cuts,col=mycols,
	xlab="Climate velocity",ylab="Harvesting rate",
	cex.lab=cex.lab,cex.axis=cex.axis, 
	yaxs=yaxs,xaxs=xaxs,axes=TRUE,
	legend.shrink=1,legend.width=.1,zlim=range(cuts),
	axis.args=list(at=myaxes$z$at,labels=myaxes$z$labels,cex.axis=cex.axis,mgp=c(0,.6,0)),
	#legend.args=list(text="Equilibrium biomass",cex=cex.lab,side=2,line=0.5,las=0),
	bigplot=c(.13,.9,.15,.95),smallplot=c(.95,.99,.15,.95),horizontal=FALSE)
box()
graphics.off()


##### heatmap of the equilibrium biomass as a function of c and h : conservation MPAs 
cvals=unique(mpas2$speed)
hvals=unique(mpas2$harvest)

H=length(hvals)
C=length(cvals)
L=length(mpas2$Equil.pop)

ebm2=array(,c(C,H))

for(i in 1:L){
	hind=which(hvals==mpas2$harvest[i])
	cind=which(cvals==mpas2$speed[i])
	ebm2[cind,hind]=mpas2$Equil.pop[i]
}

toplot_ebm2=ebm2

#breaks and colors
lower=min(ebm2)+10
cuts=c(0,seq(lower,max(ebm2)+.5,length.out=100))
mycols=(sequential_hcl(length(cuts)-2,c.=c(0,0),l=c(30,100)))
mycols=c('black',mycols)


#axis labels
numxlabs=11
numylabs=3
xdiff=.1
ydiff=.2
myaxes=list(arrows=FALSE,
	x=list(at=rev(seq(1,C,length.out=(cvals[C]-cvals[1])/xdiff+1)),labels=as.character(round(seq(cvals[1],cvals[C],by=xdiff),2))),
	y=list(at=rev(seq(1,H,length.out=(hvals[H]-hvals[1])/ydiff)),labels=as.character(round(seq(hvals[1],hvals[H],by=ydiff),2))),
	z=list(at=seq(0,max(ebm),by=250),labels=as.character(seq(0,max(ebm),by=250))))

quartz()
postscript(file='eqbiomass_consmpa.eps', horizontal = FALSE, onefile = FALSE, paper = "special", width = fig.width,height=fig.width,bg="white")


par(oma=oma,mar=mar,mgp=mgp)
image.plot(cvals,hvals,toplot_ebm2,breaks=cuts,col=mycols,
	xlab="Climate velocity",ylab="Harvesting rate",
	cex.lab=cex.lab,cex.axis=cex.axis, 
	yaxs=yaxs,xaxs=xaxs,axes=TRUE,
	legend.shrink=1,legend.width=.1,zlim=range(cuts),
	axis.args=list(at=myaxes$z$at,labels=myaxes$z$labels,cex.axis=cex.axis,mgp=c(0,.6,0)),
	#legend.args=list(text="Equilibrium biomass",cex=cex.lab,side=2,line=0.5,las=0),
	bigplot=c(.13,.9,.15,.95),smallplot=c(.95,.99,.15,.95),horizontal=FALSE)
box()
graphics.off()

postscript("eqbiomass_consmpa.eps", horizontal = FALSE, onefile = FALSE, paper = "special", width = fig.width,height=fig.width,bg="white")

par(oma=oma)
image.plot(cvals,hvals,toplot_ebm2,breaks=cuts,col=mycols,
	xlab="Climate velocity",ylab="Harvesting rate",
	cex.lab=cex.lab,cex.axis=cex.axis, 
	yaxs=yaxs,xaxs=xaxs,axes=TRUE,
	legend.shrink=1,legend.width=.1,zlim=range(cuts),
	axis.args=list(at=myaxes$z$at,labels=myaxes$z$labels,cex.axis=cex.axis),
	#legend.args=list(text="Equilibrium biomass",cex=cex.lab,side=2,line=0.5,las=0),
	bigplot=c(.13,.85,.15,.95),smallplot=c(.91,.95,.15,.95),horizontal=FALSE)
box()

graphics.off()

##########
########## 
##### heatmap of the equilibrium biomass as a function of c and h : THRESHOLD

cvals=unique(thresh$speed)
threshvals=unique(thresh$thresh)

H=length(threshvals)
C=length(cvals)
L=length(thresh$Equil.pop)

ebm=array(,c(C,H))

for(i in 1:L){
	tind=which(threshvals==thresh$thresh[i])
	cind=which(cvals==thresh$speed[i])
	ebm[cind,tind]=thresh$Equil.pop[i]
}
# threshvals=c(threshvals,0)
# toplot_ebm=cbind(toplot_ebm,rep(0,C))
toplot_ebm=ebm

#breaks and colors
lower=min(ebm)+1
cuts=c(0,seq(lower,max(ebm)+.5,length.out=100))
mycols=(sequential_hcl(length(cuts)-2,c.=c(0,0),l=c(30,100)))  
mycols=c('black',mycols)

#axis labels
numxlabs=11
numylabs=3
xdiff=.2
ydiff=.2

myaxes=list(arrows=FALSE,col=1,
# x=list(at=rev(seq(1,C,length.out=(cvals[C]-cvals[1])/xdiff+1)),labels=as.character(round(seq(cvals[1],cvals[C],by=xdiff),2))),
# y=list(at=rev(seq(1,H,length.out=(threshvals[1]-threshvals[H])/ydiff+1)),labels=as.character(rev(round(seq(threshvals[H],threshvals[1],by=ydiff),2)))),
z=list(at=seq(0,max(ebm),by=250),labels=as.character(seq(0,max(ebm),by=250))))

plt=c(.13,.9,.15,.95)

quartz()
postscript(file='eqbiomass_thresh.eps', horizontal = FALSE, onefile = FALSE, paper = "special", width = fig.width,height=fig.width,bg="white")
par(oma=oma,mar=mar,mgp=mgp,plt=plt)
image(cvals,(threshvals),toplot_ebm[1:C,H:1],breaks=cuts,col=mycols,
	xlab="Climate velocity",ylab="Threshold",
	cex.lab=cex.lab,cex.axis=cex.axis,axes=F)
	
axis(1,cex.axis=cex.axis)
axis(2, at=seq(min(threshvals),max(threshvals), length = 6),labels=rev(seq(0,1,by=0.2)),cex.axis=cex.axis)
box()
image.plot(cvals,(threshvals),toplot_ebm,legend.shrink=1,legend.width=.1,zlim=range(cuts),
	axis.args=list(at=myaxes$z$at,labels=myaxes$z$labels,cex.axis=cex.axis,mgp=c(0,.6,0)),
	#legend.args=list(text="Equilibrium biomass",cex=cex.lab,side=2,line=0.5,las=0),
	breaks=cuts,col=mycols,legend.only=TRUE,
	bigplot=c(.13,.9,.15,.95),smallplot=c(.95,.99,.15,.95),horizontal=FALSE)
graphics.off()


