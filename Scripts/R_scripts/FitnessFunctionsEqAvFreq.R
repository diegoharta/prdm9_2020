setwd("~/Documents/Projects/PZIFE/Data")


a=read.table("FitnessFunctionsEqAvFreqVsErosionRate.dat")

plot(-10,10,xlim=c(.01,20),ylim=c(0,1),xlab="Erosion rate E", ylab="Average equilibrium frequency of used motifs")
count=2
for(i in 2:ncol(a)){
    lines(a[,1],a[,i],col=count)
    points(a[,1],a[,i],col=count)
    count= count +1
  
}

legend("topright", legend=c("Exponential","Linear"), 
       col=c(2,3),lty=1,
       bty="n")