library(fields)
library(lattice)

#Beverton-Holt recruitment 

f<-function(n,R0,K){
	next_gen<-R0*n/(1+(R0-1)/K*n)
	return(next_gen)
	}
	
int<-function(v){
	l=length(v)
	step_size/2*(sum(2*v)-v[1]-v[l])
}

finda<-function(c,h,r,K,sig2,L=1,tol=NA){
	if(is.na(tol)){tol=.001}
	a=1
	fish=a*exp(-world^2/(2*sig2))
	fish=(1-h)*fish
	v=exp(-(world-c)^2/(2*sig2))*apply(fish,2,f,R0=r,K=K)
	newa=1/sqrt(2*sig2*pi)*int(v)
	while(abs(a-newa)>tol){
		a=newa
		fish=a*exp(-world^2/(2*sig2))
	fish=(1-h)*fish
	v=exp(-(world-c)^2/(2*sig2))*apply(fish,2,f,R0=r,K=K)
	newa=1/sqrt(2*sig2*pi)*int(v)
	}
	return(newa)
}

biomass<-function(c,h,r,K,sig2,L=1,tol=NA){
	if(is.na(tol)){tol=.001}
	a=finda(c,h,r,K,sig2,L,tol)
	d=a*sqrt(2*sig2*pi)*(pnorm(L/2/sqrt(sig2))-pnorm(-L/2/sqrt(sig2)))
	return(d)
}

eqbiomass<-function(cvals,hvals,r,K,sig2,L=1,tol=NA){
	if(is.na(tol)){tol=.001}
	M=array(,c(length(cvals),length(hvals)))
	for(i in 1:length(cvals)){
		for(j in 1:length(hvals)){
			M[i,j]<-biomass(cvals[i],hvals[j],r,K,sig2,L,tol)
	}
}
	rownames(M)=paste("c",cvals)
	colnames(M)=paste("h",hvals)
	M=round(M,digits=4)
	return(M)
}

synergy<-function(ebm,digs=NA){
	if(is.na(digs)){digs=3}
	cvals=as.numeric(substring(rownames(ebm),2))
	hvals=as.numeric(substring(colnames(ebm),2))
	M=array(,c(length(cvals),length(hvals)))
	M=array(,c(length(cvals),length(hvals)))
	rownames(M)=paste("c",cvals)
	colnames(M)=paste("h",hvals)

for(i in 1:length(cvals)){
	for(j in 1:length(hvals)){
		if(round(ebm[i,j],digits=digs)>0){
		d1=(ebm[1,1]-ebm[i,1])
		d2=(ebm[1,1]-ebm[1,j])
		dboth=ebm[1,1]-ebm[i,j]
		M[i,j]=dboth-(d1+d2)
		}
		else{M[i,j]=NA}
	}
}
M=M[-1,-1]
return(M)
}



