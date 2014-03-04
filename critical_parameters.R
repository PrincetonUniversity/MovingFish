require(VGAM)
require(fields)
# setwd("/Users/eleanorbrush/Dropbox/fish")

#### test whether r,w,L,c satisfy the necessary conditions for the sinusoidal kernel to make sense
conditions<-function(r,w,L,C=NULL){
	t1<-(pi/(2*w)>L)
	if(!t1){print("L is too big")}
	t2<-(r^2/16*(w^2*L^2-sin(w*L)^2)<1)
	if(!t2){print("r is too big")}
	if(!is.null(C)){
		t3<-(pi/(2*w)-L>C)
		if(!t3){print("c is too big")}
		}
	t4<-(4/(w*L+sin(w*L))<r)
	if(!t4){print("r is too small")}
	if(!is.null(C)){
		test=t1+t2+t3+t4-4
	} else {test=t1+t2+t4-3}
	if(test!=0){return(1)}
	else{return(0)}
	}

#### eigenvalue for sinusoidal kernel
lambdas<-function(c,r,sig2,L=1){
	w=sqrt(pi^2/4-2)/sqrt(sig2)
	x=r*(w*L*cos(w*c)/4+1/4*sqrt(sin(w*L)^2-w^2*L^2*sin(w*c)^2))
	return(x)
}

###### eigenvalue for gaussian kernel
lambdag<-function(c,r,sig2,L=1){
	D=sig2/2 
	x=r*1/(sqrt(2))*exp(-c^2/(8*D))*(pnorm((L-c)/(2*sqrt(D)),mean=0,sd=1)-pnorm((-L-c)/(2*sqrt(D)),mean=0,sd=1))
	return(x)
}


##### 1-lambdas, will be 0 at c=critical speed
fs<-function(c,r,sig2,L=1){
	x=1-lambdas(c,r,sig2,L)
	return(x)
}

#### 1-lambdag, will be 0 at critical speed
fg<-function(c,r,sig2,L=1){
	y=lambdag(c,r,sig2,L)
	x=1-lambdag(c,r,sig2,L)
	return(x)
}


####equates w and D dispersal parameters
wequiv<-function(D){
	w=sqrt(pi^2/4-2)/sqrt(2*D) #in terms of variance
	w=2/L*asin(erf(L/(4*sqrt(D)))) #in terms of offspring dispersing within the patch
	return(w)
}

#### analytical expression for critical speed for sinusoidal kernel
cstar_s<-function(r,sig2,L=1){
	w=sqrt(pi^2/4-2)/sqrt(sig2)
	num<-16+r^2*(w^2*L^2-sin(w*L)^2)
	denom<-8*r*w*L
	if(abs(num)<=abs(denom)){
	x<-1/w*acos(num/denom)} else{
		x=0}
	return(x)
}

#### numerically find critical speed for gaussian kernel
cstar_g<-function(r,sig2,L=1){
	if(lambdag(0,r,sig2,L)<1){ #if eigenvalue is less than 1 for c=0, c=0 is the critical speed
		x=0
	} else{ #if eigenvalue is greater than 1 for c=0, look for c where eigenvalue = 1
	x=uniroot(fg,interval=c(0,10),r=r,sig2=sig2,L=L)$root}
	return(x)
}

#### analytical expression for critical harvesting level for sinusoidal kernel
hstar_s<-function(c,r,sig2,L=1){
	if(is.na(L)){L=1}
	w=sqrt(pi^2/4-2)/sqrt(sig2)
	a=4*w*L
	a=a/r/(w^2*L^2-sin(w*L)^2)
	b=cos(w*c)
	d=sqrt(cos(w*c)^2-1+sin(w*L)^2/w^2/L^2)
	h=1-a*(b-d)
	if(cstar_s(r,sig2,L)<=c){h=0}
	return(h)
}

###### numerically find critical harvesting proportion for gaussian kernel
hstar_g<-function(c,r,sig2,L=1){
	y=cstar_g(r,sig2,L)
	if(y<=c){h=0}
	else{h=1-1/lambdag(c,r,sig2,L)}
	return(h)
}
