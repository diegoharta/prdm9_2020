label="prueba_1.63a6"



m <- rbind(c(1, 1,1))
layout(m)
par(mar = c(5, 5, 2, 2))

setwd(paste("~/Documents/Projects/PZIFE/C_scripts_and_data/",label,sep=""))

profile=read.table(paste("profile_",label,".dat",sep=""),header=TRUE)
summaryStats=mat.or.vec(2,10)


a=read.table(paste("histogramOfSizeOfZnfArray_",label,".dat",sep=""))
x <- seq(5,20,1)
sumA = apply(a,2,sum)
sumAC = sumA[7:22]
sumAD = sum(sumAC)
plot(x,sumA[7:22]/sumAD)
     
alpha=profile$BirthRate/profile$DeathRate
kmin=5
kmax =20
suma=0
for(k in kmin:kmax){
  suma=suma+alpha^(k-kmin)/k
}
pikmin=1/(kmin*suma)
cte=pikmin*kmin/(alpha^kmin)
pik=mat.or.vec(1,kmax)
for(k in kmin:kmax){
  pik[k]=cte*(alpha^k)/k
}
#plot(0,0,xlim=c(5,20),ylim=c(0,.6))
for(k in 5:20){
  points(k,pik[k],col=2,pch=20)
}
sum(pik)
text(10,.25,paste("bplus = ",profile$BirthRate,sep=""))
text(10,.23,paste("bminus = ",profile$DeathRate,sep=""))






#PRINT PRDM9 Diversity as Latrille et al 2017
c=read.table(paste("prdmDiversity_",label,".dat",sep=""))
nrow(c)
ncol(c)
x=seq(1,ncol(c),1)

divP = apply(c,2,mean)
#colMeans(c)

sdP=apply(c,2,sd)
plot(x,divP,ylab="PRDM9 diversity",ylim=c(0,4),xlim=c(0,profile$Generations-profile$BurnIn),xlab="Generations (post burnin)",
     sub=paste(label," N=",profile$PopulationSize," Theta=",profile$Theta,
                          " C=",profile$GeneConversionRate," E=",profile$ErosionRate," Alpha=",profile$Alpha,
                          " bPlus=",profile$BirthRate," bMinus=",profile$DeathRate,sep=""))
lines(x,divP+sdP,col="green")
lines(x,divP-sdP,col="green")
prom=mean(as.numeric(c[(ncol(c)/2):ncol(c)]))
abline(h=prom,col="red")
relDiversity=(as.numeric(c[(ncol(c)/2):ncol(c)]))
densDiv=density(relDiversity)
#plot(densDiv)
