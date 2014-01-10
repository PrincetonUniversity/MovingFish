library(fields)
library(lattice)
library(colorspace)
setwd("/Users/eleanorbrush/Desktop")
source("/Users/eleanorbrush/Desktop/biomass_synergy.R")
source("/Users/eleanorbrush/Desktop/critical_parameters.R")


### parameters from Zhou
r=3
w=pi/4
L=1

####
#the world
step_size=0.01
L=1
world=matrix(seq(-L/2,L/2,by=step_size),nrow=1)
emmaR0=5
emmaK=100

#####
#### plotting parameters
cex.lab=3
cex.axis=2
line=5
lwd=5
yaxs='i'
xaxs='i'
cex.lab=1.5
cex.axis=1
line=1
oma=c(0,0,0,2)

##########
##########
##### wireframe of the equilibrium biomass as a function of c and h
r=emmaR0
K=emmaK
sig2=1
tol=.00001
diff=.01
cvals=seq(0,1,by=diff)
hvals=seq(0,1,by=diff)


# ebm=eqbiomass(cvals,hvals,r,K,sig2,tol=tol)
load("/Users/eleanorbrush/Desktop/ebm.rdata")

#which params to work with
H=sum(apply(ebm,2,max)>tol)+2
hvals=hvals[1:H]
C=length(cvals)
toplot_ebm=ebm[1:C,1:H]

#breaks and colors
lower=.001
cuts=c(0,lower,seq(.5,max(ebm)+.5,by=.5))
# s=5
# maxgray=length(cuts)-2+s-1
# vecgray=(s:maxgray)/maxgray
# mycols=c('black',gray(vecgray))
# colorbar.labels=as.character(round(cuts[-1],digits=1))
# mycolorkey=list(col=mycols,at=cuts,height=1,lty=1)
# myalpha=.7
mycols=(sequential_hcl(length(cuts)-2,c.=c(0,0),l=c(30,100)))
mycols=c('black',mycols)

#axis labels
numxlabs=11
numylabs=3
xdiff=.1
ydiff=.2
myaxes=list(arrows=FALSE,x=list(at=rev(seq(1,C,length.out=(cvals[C]-cvals[1])/xdiff+1)),labels=as.character(round(seq(cvals[1],cvals[C],by=xdiff),2))),y=list(at=rev(seq(1,H,length.out=(hvals[H]-hvals[1])/ydiff)),labels=as.character(round(seq(hvals[1],hvals[H],by=ydiff),2))))

quartz()
pdf(file='eqbiomass.pdf',width=6.83)

# wireframe(toplot_ebm[C:1,H:1],xlab=list("Rate of environmental shift",rot=10),ylab=list("Harvesting rate",rot=-35),zlab=list('Equilibrium biomass',rot=90),at=cuts,col.regions=mycols,alpha.regions=myalpha,scales=myaxes,drape=FALSE,screen=list(z=30,x=-80),par.settings = list(axis.line = list(col = "transparent")))
par(oma=oma)
image.plot(cvals,hvals[1:H],toplot_ebm[,1:H],breaks=cuts,col=mycols,
	xlab="Rate of environmental shift",ylab="Harvesting rate",
	cex.lab=cex.lab,cex.axis=cex.axis, 
	yaxs=yaxs,xaxs=xaxs,axes=TRUE,
	legend.shrink=1,legend.width=.1,zlim=range(cuts),
	axis.args=list(at=myaxes$z$at,labels=myaxes$z$labels,cex.axis=cex.axis),
	legend.args=list(text="Equilibrium biomass",cex=cex.lab,side=2,line=0.5,las=0),
	bigplot=c(.13,.85,.15,.95),smallplot=c(.91,.95,.15,.95),horizontal=FALSE)
box()
dev.off()


##########
########## 
##### wireframe of the equilibrium biomass as a function of c and h : SIMULATIONS

sim=read.csv("~/Desktop/noMPAnotThresh_add_2013-12-09.csv")

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
lower=min(ebm)
cuts=c(0,seq(lower,max(ebm)+.5,length.out=100))
# s=5
# maxgray=length(cuts)-2+s-1
# vecgray=(s:maxgray)/maxgray
# mycols=c('black',gray(vecgray))
# colorbar.labels=as.character(round(cuts[-1],digits=1))
# mycolorkey=list(col=mycols,at=cuts,height=1,lty=1)
# myalpha=.7
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
pdf(file='eqbiomass_sim.pdf',width=6.83)

# wireframe(toplot_ebm[C:1,H:1],xlab=list("Rate of environmental shift",rot=10),ylab=list("Harvesting rate",rot=-35),zlab=list('Equilibrium biomass',rot=90),at=cuts,col.regions=mycols,alpha.regions=myalpha,scales=myaxes,drape=FALSE,screen=list(z=30,x=-80),par.settings = list(axis.line = list(col = "transparent")))
par(oma=oma)
image.plot(cvals,hvals,toplot_ebm,breaks=cuts,col=mycols,
	xlab="Rate of environmental shift",ylab="Harvesting rate",
	cex.lab=cex.lab,cex.axis=cex.axis, 
	yaxs=yaxs,xaxs=xaxs,axes=TRUE,
	legend.shrink=1,legend.width=.1,zlim=range(cuts),
	axis.args=list(at=myaxes$z$at,labels=myaxes$z$labels,cex.axis=cex.axis),
	legend.args=list(text="Equilibrium biomass",cex=cex.lab,side=2,line=0.5,las=0),
	bigplot=c(.13,.85,.15,.95),smallplot=c(.91,.95,.15,.95),horizontal=FALSE)
box()
dev.off()

##########
##########
##### wireframe of the equilibrium biomass as a function of c and h : MPAs
mpas=read.csv("~/Desktop/fishMPAnotThresh_add_2013-12-09.csv")

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
lower=min(ebm)
cuts=c(0,seq(lower,max(ebm)+.5,length.out=100))
# s=5
# maxgray=length(cuts)-2+s-1
# vecgray=(s:maxgray)/maxgray
# mycols=c('black',gray(vecgray))
# colorbar.labels=as.character(round(cuts[-1],digits=1))
# mycolorkey=list(col=mycols,at=cuts,height=1,lty=1)
# myalpha=.7
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
pdf(file='eqbiomass_mpa.pdf',width=6.83)

# wireframe(toplot_ebm[C:1,H:1],xlab=list("Rate of environmental shift",rot=10),ylab=list("Harvesting rate",rot=-35),zlab=list('Equilibrium biomass',rot=90),at=cuts,col.regions=mycols,alpha.regions=myalpha,scales=myaxes,drape=FALSE,screen=list(z=30,x=-80),par.settings = list(axis.line = list(col = "transparent")))
par(oma=oma)
image.plot(cvals,hvals,toplot_ebm,breaks=cuts,col=mycols,
	xlab="Rate of environmental shift",ylab="Harvesting rate",
	cex.lab=cex.lab,cex.axis=cex.axis, 
	yaxs=yaxs,xaxs=xaxs,axes=TRUE,
	legend.shrink=1,legend.width=.1,zlim=range(cuts),
	axis.args=list(at=myaxes$z$at,labels=myaxes$z$labels,cex.axis=cex.axis),
	legend.args=list(text="Equilibrium biomass",cex=cex.lab,side=2,line=0.5,las=0),
	bigplot=c(.13,.85,.15,.95),smallplot=c(.91,.95,.15,.95),horizontal=FALSE)
box()
dev.off()


##### wireframe of the equilibrium biomass as a function of c and h : MPAs v2
mpas2=read.csv("~/Desktop/consMPAnotThresh_add_2013-12-09.csv")

cvals=unique(mpas$speed)
hvals=unique(mpas$harvest)

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
lower=min(ebm2)
cuts=c(0,seq(lower,max(ebm2)+.5,length.out=100))
# s=5
# maxgray=length(cuts)-2+s-1
# vecgray=(s:maxgray)/maxgray
# mycols=c('black',gray(vecgray))
# colorbar.labels=as.character(round(cuts[-1],digits=1))
# mycolorkey=list(col=mycols,at=cuts,height=1,lty=1)
# myalpha=.7
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
pdf(file='eqbiomass_mpa_v2.pdf',width=6.83)

# wireframe(toplot_ebm[C:1,H:1],xlab=list("Rate of environmental shift",rot=10),ylab=list("Harvesting rate",rot=-35),zlab=list('Equilibrium biomass',rot=90),at=cuts,col.regions=mycols,alpha.regions=myalpha,scales=myaxes,drape=FALSE,screen=list(z=30,x=-80),par.settings = list(axis.line = list(col = "transparent")))
par(oma=oma)
image.plot(cvals,hvals,toplot_ebm2,breaks=cuts,col=mycols,
	xlab="Rate of environmental shift",ylab="Harvesting rate",
	cex.lab=cex.lab,cex.axis=cex.axis, 
	yaxs=yaxs,xaxs=xaxs,axes=TRUE,
	legend.shrink=1,legend.width=.1,zlim=range(cuts),
	axis.args=list(at=myaxes$z$at,labels=myaxes$z$labels,cex.axis=cex.axis),
	legend.args=list(text="Equilibrium biomass",cex=cex.lab,side=2,line=0.5,las=0),
	bigplot=c(.13,.85,.15,.95),smallplot=c(.91,.95,.15,.95),horizontal=FALSE)
box()
dev.off()

##########
########## 
##### wireframe of the equilibrium biomass as a function of c and h : THRESHOLD
thresh=read.csv("~/Desktop/noMPA_Thresh_add_2013-12-10.csv")

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
lower=min(ebm)
cuts=c(0,seq(lower,max(ebm)+.5,length.out=100))
# s=5
# maxgray=length(cuts)-2+s-1
# vecgray=(s:maxgray)/maxgray
# mycols=c('black',gray(vecgray))
# colorbar.labels=as.character(round(cuts[-1],digits=1))
# mycolorkey=list(col=mycols,at=cuts,height=1,lty=1)
# myalpha=.7
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
z=list(at=seq(0,max(ebm),by=25),labels=as.character(seq(0,max(ebm),by=25))))

quartz()
pdf(file='eqbiomass_thresh.pdf',width=6.83)

# wireframe(toplot_ebm[C:1,H:1], par.settings = list(axis.line = list(col = "transparent")),xlab=list("Rate of environmental shift",rot=10),ylab=list("Threshold",rot=-35),zlab=list('Equilibrium biomass',rot=90),at=cuts,col.regions=mycols,alpha.regions=myalpha,scales=myaxes,screen=list(z=30,x=-80))
par(oma=oma)
image.plot(toplot_ebm[1:C,H:1],bigplot=c(.13,.85,.15,.95),smallplot=c(.91,.95,.15,.95))
image(cvals,(threshvals),toplot_ebm[1:C,H:1],breaks=cuts,col=mycols,
	xlab="Rate of environmental shift",ylab="Threshold",
	cex.lab=cex.lab,cex.axis=cex.axis,axes=F)
axis(1)
axis(2,at=seq(0,1,by=0.2),labels=rev(seq(0,1,by=0.2)))
box()
image.plot(cvals,(threshvals),toplot_ebm,legend.shrink=1,legend.width=.1,zlim=range(cuts),
	axis.args=list(at=myaxes$z$at,labels=myaxes$z$labels,cex.axis=cex.axis),
	legend.args=list(text="Equilibrium biomass",cex=cex.lab,side=2,line=0.5,las=0),breaks=cuts,col=mycols,legend.only=TRUE,bigplot=c(.13,.85,.15,.95),smallplot=c(.91,.95,.15,.95),horizontal=FALSE)

dev.off()

############
#############
#relationship between critical harvesting rate and critical rate of environmental shift 

cvals=matrix(seq(0,1,by=0.01),nrow=1)

nr=3
ns=2
rvals=c(rep(3,ns),rep(7,ns),rep(10,ns))
rcols=c(rep('black',ns),rep('red',ns),rep('blue',ns))
xvals=c(.1,.25)
sigvals=rep((xvals*sqrt(2*pi))^2,nr)
sigltys=rep(c(1,2),nr)
l=length(rvals)

lwd=2
cex.lab=2
cex.axis=1.5
cex.legend=1.5
tck=-.02
mar=c(6,6,2,2)
mgp=c(4,2,0)

quartz()
pdf(file='critical_rates.pdf',width=6.83)

par(mar=mar,mgp=mgp)
plot(cvals,cvals,col='white',ylim=c(0,1),xlab="Rate of environmental shift",ylab="Critical harvesting rate",bty='l',axes=FALSE,xaxs='i',yaxs='i',cex.lab=cex.lab)

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

legend(.63,1.03,col=c(urcols,rep('black',3),'white'),lty=c(rep(1,3),usigltys,1),lwd=lwd,legend=c(bquote(R[0]==.(urvals[1])),bquote(R[0]==.(urvals[2])), bquote(R[0]==.(urvals[3])),expression(symbol("\341")*d*symbol("\361")== .1),expression(symbol("\341")*d*symbol("\361")== .25),expression()),bty='n',cex=cex.legend)

dev.off()

###########
###########
# synergy between harvesting and climate shift
load("/Users/eleanorbrush/Desktop/ebm.rdata")

r=emmaR0
K=emmaK
sig2=1
tol=.00001
diff=.01
cvals=seq(0,1,by=diff)
hvals=seq(0,1,by=diff)

syn=synergy(ebm,2)

a=apply(syn,2,min,na.rm=TRUE)
H=max(which(is.finite(a)))
C=length(cvals)
for(i in 1:H){
	m=which.max(syn[,i])
	if(is.na(syn[C-1,i]))
	syn[,i][(m+1):(C-1)]=NA
}
H=H+2

m=min(syn[!is.na(syn)])
v=c(m-.00002,m-.00001,seq(0,max(syn[!is.na(syn)]),length.out=100))
syn[is.na(syn)]=m-.000015
cols=(sequential_hcl(length(v)-2,h=260,c.=c(0,0),l=c(30,90),power=c(.9)))
cols=c('black',cols)

labs=round(seq(0,max(syn[!is.na(syn)]),by=.001),3)

cex.lab=3
cex.axis=2
line=5
lwd=5
yaxs='i'
xaxs='i'
cex.lab=1.5
cex.axis=1
line=1
oma=c(0,0,0,2)

quartz()
pdf(file='synergy.pdf',width=6.83)

par(oma=oma)
image.plot(cvals,hvals[1:H],syn[,1:H],breaks=v,col=cols,
	xlab="Rate of environmental shift",ylab="Harvesting rate",
	cex.lab=cex.lab,cex.axis=cex.axis, 
	yaxs=yaxs,xaxs=xaxs,axes=TRUE,
	legend.shrink=1,legend.width=.1,zlim=range(v),
	axis.args=list(at=labs,labels=labs,cex.axis=cex.axis),
	legend.args=list(text="Synergy",cex=cex.lab,side=2,line=0.5,las=0),
	bigplot=c(.13,.85,.15,.95),smallplot=c(.91,.95,.15,.95),horizontal=FALSE)
	
	dev.off()
