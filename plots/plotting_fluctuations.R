setwd("/Users/efuller/Documents/Projects/Moving_fish/MovingFish/Simluations/Aspatial_fast/Data/fluctuations")

pcNA <- read.csv("popconsnoThreshNA.csv")
pcEf <- read.csv("popconsnoThreshyes.csv")
pfNA <- read.csv("popfishnoThreshNA.csv")
pfEf <- read.csv("popfishnoThreshyes.csv")
pn <- read.csv("popnullnoThreshNA.csv")

# png(file="compare_flucts.png", height=6,width=6,units="in",res=300)
pdf(file='compare_flucts.pdf',height=6,width=6.1)
plot(pcNA[1:200,], type='l',ylim=c(1000,2200),lwd=2,bty="n",xlab="Generation", ylab="Total population size")
lines(pcEf,type='l',col="black",lwd=2,lty=9)

lines(pfNA,lty=1,lwd=2, col ="grey")
lines(pfEf, lty=9,lwd=2, col = "grey")

legend("top",legend=c("Few large reserves", "Many small reserves","Effort removed","Effort constant"),lwd=2, lty=c(1,1,1,9), col=c("black","grey","black","black"),ncol=2,bty="n")
dev.off()


####### plotting on same scale - ignore. 

lines(pn, col="blue")

plot(pn, type='l')

# anomalies 

p1a <- pcNA$x - mean(pcNA$x)
p2a <- pcEf$x - mean(pcEf$x)
plot(p2a, type='l')
lines(p1a,col="red")
p3a <- pfNA$x - mean(pfNA$x)
p4a <- pfEf$x - mean(pfEf$x)
plot(p3a, type='l')
lines(p4a, col="orange")