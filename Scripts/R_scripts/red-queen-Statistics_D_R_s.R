#This file intends to show the results from 1 PZIFE simulation
#It shows the relevant statistics that characterize the evolutionary scenario under which the Red-Queen is developing
#It generates one pdf file 


label= "prueba_1.85-N100-K20-s10-c0-b1-C1-p1-u2-X2-d3-long"

setwd(paste("~/Documents/Projects/PZIFE/C_scripts_and_data/",label,sep=""))
profile=read.table(paste("profile_",label,".dat",sep=""),header=TRUE)
m <- rbind(c(1, 1,1), c(2,2,2),c(3,3,3))
layout(m)
par(mar = c(5,5, 0.5, 0.5))

textPos=10
#PRINT PRDM9 Diversity as Latrille et al 2017
c=read.table(paste("prdmDiversity_",label,".dat",sep=""))
x=seq(1,length(c),1)
meanC=rowMeans(c)
plot(x,c[1,],log="y",type="l",ylab="PRDM9 diversity")
prom=mean(as.numeric(c[1,(ncol(c)/2):ncol(c)]))
abline(h=prom,col="red")
text(textPos,max(c[1,]),prom)


#PRINT RECOMBINATION ACTIVITY AS LATRILLE ET AL 2017
d=read.table(paste("recombinationActivity_",label,".dat",sep=""))
meanD=rowMeans(d)
plot(x,d[1,],type="l",ylab="Recombination activity")
prom=mean(as.numeric(d[1,(ncol(d)/2):ncol(d)]))
abline(h=prom,col="red")
text(textPos,max(d[1,]),prom)

#PRINT selection coefficient as Latrille et al 2017
e=read.table(paste("selectionCoefficient_",label,".dat",sep=""),header=TRUE)
x=seq(1,length(e),1)
plot(e$year,e$selecCoeff,ylab="Selection Coefficient",sub=label,xlim=c(profile$BurnIn,profile$Generations))
prom=mean(as.numeric(e[(nrow(e)/2):nrow(e),2]))
abline(h=prom,col="red")
text(profile$BurnIn+1500,max(e$selecCoeff),prom)
selValue=4*profile$PopulationSize*prom
text(profile$BurnIn+1500,max(e$selecCoeff)-max(e$selecCoeff)*.1,paste("4Ns=",selValue,sep=""))



