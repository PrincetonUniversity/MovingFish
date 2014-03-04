library(fields)
library(lattice)
library(colorspace)
library(RCurl)
setwd("/Users/eleanorbrush/Desktop")
source("/Users/eleanorbrush/Desktop/biomass_synergy.R")
source("/Users/eleanorbrush/Desktop/critical_parameters.R")
source("load_data.R")

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
cex.lab=.75
cex.axis=.5
cex.legend=.5
tck=-.02
mar=c(2,2,1,1)
mgp=c(1,.25,0)
oma=c(0,0,0,0)

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

par(mar=mar,mgp=mgp,oma=oma)
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
##### wireframe of the equilibrium biomass as a function of c and h
r=emmaR0
K=emmaK
sig2=1
tol=.00001
diff=.25
cvals=seq(0,1,by=diff)
hvals=seq(0,1,by=diff)


# x=getURL("https://raw.github.com/emfuller/MovingFish/38c6f3bb51dae267fe4056159609de100fb34826/eqbiomass_gaus.csv?token=6224444__eyJzY29wZSI6IlJhd0Jsb2I6ZW1mdWxsZXIvTW92aW5nRmlzaC8zOGM2ZjNiYjUxZGFlMjY3ZmU0MDU2MTU5NjA5ZGUxMDBmYjM0ODI2L2VxYmlvbWFzc19nYXVzLmNzdiIsImV4cGlyZXMiOjEzOTQ0ODQ5OTh9--d95944353679e18e44def40cd49ca41a22502039")
# ebm=read.csv(textConnection(x))

#which params to work with
H=sum(apply(ebm,2,max)>tol)+2
hvals=hvals[1:H]
C=length(cvals)
toplot_ebm=ebm[1:C,1:H]

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


quartz(width=fig.width,height=fig.width)
pdf(file='eqbiomass.pdf')

par(oma=oma)
image.plot(cvals,hvals[1:H],toplot_ebm[,1:H],breaks=cuts,col=mycols,
	xlab="Climate velocity",ylab="Harvesting rate",
	cex.lab=cex.lab,cex.axis=cex.axis, 
	yaxs=yaxs,xaxs=xaxs,axes=TRUE,
	legend.shrink=1,legend.width=.1,zlim=range(cuts),
	axis.args=list(at=myaxes$z$at,labels=myaxes$z$labels,cex.axis=cex.axis),
	legend.args=list(text="Equilibrium biomass",cex=cex.lab,side=2,line=0.5,las=0),
	bigplot=c(.13,.85,.15,.95),smallplot=c(.91,.95,.15,.95),horizontal=FALSE)
box()
graphics.off()


##########
########## 
##### wireframe of the equilibrium biomass as a function of c and h : SIMULATIONS

# github copy
	# x = getURL("https://raw.github.com/emfuller/MovingFish/master/Simluations/Aspatial_fast/Data/MPAnull_2014-03-02.csv?token=3235371__eyJzY29wZSI6IlJhd0Jsb2I6ZW1mdWxsZXIvTW92aW5nRmlzaC9tYXN0ZXIvU2ltbHVhdGlvbnMvQXNwYXRpYWxfZmFzdC9EYXRhL01QQW51bGxfMjAxNC0wMy0wMi5jc3YiLCJleHBpcmVzIjoxMzk0NDA4NjYyfQ%3D%3D--7f380bfb17f1cd58ff4f131134ed434700767def")
	# sim = read.csv(textConnection(x))

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
pdf(file='eqbiomass_sim.pdf',width=fig.width, height=fig.width)

par(oma=oma)
image.plot(cvals,hvals,toplot_ebm,breaks=cuts,col=mycols,
	xlab="Climate velocity",ylab="Harvesting rate",
	cex.lab=cex.lab,cex.axis=cex.axis, 
	yaxs=yaxs,xaxs=xaxs,axes=TRUE,
	legend.shrink=1,legend.width=.1,zlim=range(cuts),
	axis.args=list(at=myaxes$z$at,labels=myaxes$z$labels,cex.axis=cex.axis),
	#legend.args=list(text="Equilibrium biomass",cex=(cex.lab-1),side=2,line=0.5,las=0),
	bigplot=c(.13,.85,.15,.95),smallplot=c(.91,.95,.15,.95),horizontal=FALSE)
box()
dev.off()

##########
##########
##### wireframe of the equilibrium biomass as a function of c and h : fisheries MPA

# github copy
	# x = getURL("https://raw.github.com/emfuller/MovingFish/master/Simluations/Aspatial_fast/Data/MPAfish_2014-03-02.csv?token=3235371__eyJzY29wZSI6IlJhd0Jsb2I6ZW1mdWxsZXIvTW92aW5nRmlzaC9tYXN0ZXIvU2ltbHVhdGlvbnMvQXNwYXRpYWxfZmFzdC9EYXRhL01QQWZpc2hfMjAxNC0wMy0wMi5jc3YiLCJleHBpcmVzIjoxMzk0NDA4NjQxfQ%3D%3D--2dba33c2046114811967e01914fc795e1771f425")
	# mpas = read.csv(textConnection(x))
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
pdf(file='eqbiomass_fishmpa.pdf',width=fig.width,height=fig.width)

par(oma=oma)
image.plot(cvals,hvals,toplot_ebm,breaks=cuts,col=mycols,
	xlab="Climate velocity",ylab="Harvesting rate",
	cex.lab=cex.lab,cex.axis=cex.axis, 
	yaxs=yaxs,xaxs=xaxs,axes=TRUE,
	legend.shrink=1,legend.width=.1,zlim=range(cuts),
	axis.args=list(at=myaxes$z$at,labels=myaxes$z$labels,cex.axis=cex.axis),
	#legend.args=list(text="Equilibrium biomass",cex=cex.lab,side=2,line=0.5,las=0),
	bigplot=c(.13,.85,.15,.95),smallplot=c(.91,.95,.15,.95),horizontal=FALSE)
box()
dev.off()


##### wireframe of the equilibrium biomass as a function of c and h : conservation MPAs 

# github copy
	# x = getURL("https://raw.github.com/emfuller/MovingFish/master/Simluations/Aspatial_fast/Data/MPAcons_2014-03-01.csv?token=3235371__eyJzY29wZSI6IlJhd0Jsb2I6ZW1mdWxsZXIvTW92aW5nRmlzaC9tYXN0ZXIvU2ltbHVhdGlvbnMvQXNwYXRpYWxfZmFzdC9EYXRhL01QQWNvbnNfMjAxNC0wMy0wMS5jc3YiLCJleHBpcmVzIjoxMzk0NDA4NjEzfQ%3D%3D--6957fb9fe54dae43ecf5c361c34446f61a1b0a0f")
	# mpas2 = read.csv(textConnection(x))

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
lower=min(ebm2)
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
pdf(file='eqbiomass_consmpa.pdf',width=fig.width,height=fig.width)


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
dev.off()

##########
########## 
##### wireframe of the equilibrium biomass as a function of c and h : THRESHOLD

# online github copy
	# x = getURL("https://raw.github.com/emfuller/MovingFish/master/Simluations/Aspatial_fast/Data/Thresh_2014-03-02.csv?token=3235371__eyJzY29wZSI6IlJhd0Jsb2I6ZW1mdWxsZXIvTW92aW5nRmlzaC9tYXN0ZXIvU2ltbHVhdGlvbnMvQXNwYXRpYWxfZmFzdC9EYXRhL1RocmVzaF8yMDE0LTAzLTAyLmNzdiIsImV4cGlyZXMiOjEzOTQ0MDg2OTF9--50c2acb25198937b8f95463cd2713c2db8f124b2")
	# thresh = read.csv(textConnection(x))
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

quartz()
pdf(file='eqbiomass_thresh.pdf',width=fig.width, height=fig.width)


par(oma=oma)
image(cvals,(threshvals),toplot_ebm[1:C,H:1],breaks=cuts,col=mycols,
	xlab="Climate velocity",ylab="Threshold",
	cex.lab=cex.lab,cex.axis=cex.axis,axes=F)
axis(1)
axis(2, at=seq(min(threshvals),max(threshvals), length = 6),labels=rev(seq(0,1,by=0.2)))
box()
image.plot(cvals,(threshvals),toplot_ebm,legend.shrink=1,legend.width=.1,zlim=range(cuts),
	axis.args=list(at=myaxes$z$at,labels=myaxes$z$labels,cex.axis=cex.axis),
	#legend.args=list(text="Equilibrium biomass",cex=cex.lab,side=2,line=0.5,las=0),breaks=cuts,col=mycols,legend.only=TRUE,
	bigplot=c(.13,.85,.15,.95),smallplot=c(.91,.95,.15,.95),horizontal=FALSE)
box()
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
pdf(file='synergy.pdf',width=fig.width)

par(oma=oma)
image.plot(cvals,hvals[1:H],syn[,1:H],breaks=v,col=cols,
	xlab="Climate velocity",ylab="Harvesting rate",
	cex.lab=cex.lab,cex.axis=cex.axis, 
	yaxs=yaxs,xaxs=xaxs,axes=TRUE,
	legend.shrink=1,legend.width=.1,zlim=range(v),
	axis.args=list(at=labs,labels=labs,cex.axis=cex.axis),
	legend.args=list(text="Synergy",cex=cex.lab,side=2,line=0.5,las=0),
	bigplot=c(.13,.85,.15,.95),smallplot=c(.91,.95,.15,.95),horizontal=FALSE)
	
	dev.off()
