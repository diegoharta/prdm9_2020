#This script generates a plot of the variation present within Znfs vs the position within the Znfs to show that the DNA binding positions
# accumulate most of the diversity present within the znfs


label="prueba_1.93-N1000-K20-s1-C1-p1-u3-X5-d1-300.-Z3-j6-b"

setwd(paste("~/Documents/Projects/PZIFE/C_scripts_and_data/",label,sep=""))

m = rbind(c(1,1,1),c(2,2,2))
layout(m,heights=c(2,2))
par(mar = c(3,5,3, 0.5))

#Read files
a=read.table(paste("meanDiversityWithinZnf_",label,".dat",sep=""))
b=read.table(paste("sdDiversityWithinZnf_",label,".dat",sep=""))

profile=read.table(paste("profile_",label,".dat",sep=""),header=TRUE)
Res = profile$RelevantResidues*2+1;

equis = seq(1,Res,1)
plot(-100,-100,xlim=c(0,Res+1),ylim=c(0,1))
for(i in 1:nrow(a)){
  lines(equis,a[i,],col=i)
}

aaDivInPrdm9Binding=mat.or.vec(1,nrow(a))
for(i in 1:nrow(a)){
  sumRelRes=a[i,2]+a[i,4]+a[i,6]

  aaDivInPrdm9Binding[i]=sumRelRes/sum(a[i,])
}
equiss=seq(1,nrow(a),1)
plot(equiss,aaDivInPrdm9Binding,xlim=c(0,nrow(a)),ylim=c(0,1))

