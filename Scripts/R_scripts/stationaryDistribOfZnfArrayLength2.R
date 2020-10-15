label="prueba_1.721a2-N100-K15-p2-b1"
#label="prueba_1.65a9h"

label="prueba_1.82a-N1000-K20-c0-b1-C1-p2-u3-X3-d6"
label="prueba_1.83b-N1000-K20-s10-c0-b1-C1-p2-u2-X3-d6"
label="prueba_1.85-N100-K20-s1-c0-b1-C15a-p1-u2-X2-d3-long"



m <- rbind(c(1, 1,1))
layout(m)
par(mar = c(5, 5, 2, 2))

setwd(paste("~/Documents/Projects/PZIFE/C_scripts_and_data/",label,sep=""))

profile=read.table(paste("profile_",label,".dat",sep=""),header=TRUE)
summaryStats=mat.or.vec(2,10)

a=read.table(paste("histogramOfSizeOfZnfArray_",label,".dat",sep=""))
novA=a[,3:length(a)]
histot=hist(as.numeric(unlist(novA)),xlim=c(4.5,20.5),ylim=c(0,.2),freq=FALSE,breaks=c(4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5,13.5,14.5,15.5,16.5,17.5,18.5,19.5,20.5))
Prop=profile$SampleSize*2*profile$Runs*(profile$Generations-profile$BurnIn)/20
Prop=1
alpha=profile$BirthRate/profile$DeathRate
kmin=5
kmax =20
suma=0
for(k in kmin:kmax){
  suma=suma+alpha^(k-kmin)/k
}
pikmin=1/(kmin*suma)
cte=Prop*pikmin*kmin/(alpha^kmin)
pik=mat.or.vec(1,kmax)
for(k in kmin:kmax){
  pik[k]=cte*(alpha^k)/k
}
#plot(0,0,xlim=c(5,20),ylim=c(0,.6))
for(k in 5:20){
  points(k,pik[k],col=2,pch=20)
}
sum(pik)
#text(10,.15*Prop,paste("bplus = ",profile$BirthRate,sep=""))
#text(10,.14*Prop,paste("bminus = ",profile$DeathRate,sep=""))
#text(10,.13*Prop,paste("alpha = ",profile$Alpha,sep=""))


text(10,.17*Prop,label)
text(10,.16*Prop,paste("N = ",profile$PopulationSize,sep=""))
text(10,.15*Prop,paste("bplus = ",profile$BirthRate,sep=""))
text(10,.14*Prop,paste("bminus = ",profile$DeathRate,sep=""))
text(10,.13*Prop,paste("alpha = ",profile$Alpha,sep=""))
text(10,.12*Prop,paste("C = ",profile$GeneConversionRate,sep=""))




