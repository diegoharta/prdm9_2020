#This plot compares two files: histogramOfSizeOfZnfArray with generalStatistics
#The objective is to compare the average size of the zinc finger array with the effective number of mutations of each type for 
#each run

label="prueba_1.721a2-N100-K15-p2-b1"
#label="prueba_1.65a9h"

label="prueba_1.82a-N1000-K20-c0-b1-C1-p2-u3-X3-d6"
label="prueba_1.83b-N1000-K20-s10-c0-b1-C1-p2-u2-X3-d6"
label="prueba_1.83b-N1000-K20-s10-c0-b1-C2-p2-u3-X3-d6-long"
label="prueba_1.84-N100-K20-s10-c0-b1-C1-p2-u2-X1-d5-long"


arr_color=c("goldenrod2","royalblue","orangered","grey")
m <- rbind(c(1, 1,1))
layout(m)
par(mar = c(5, 5, 2, 2))

setwd(paste("~/Documents/Projects/PZIFE/C_scripts_and_data/",label,sep=""))

profile=read.table(paste("profile_",label,".dat",sep=""),header=TRUE)
summaryStats=mat.or.vec(2,10)

a=read.table(paste("histogramOfSizeOfZnfArray_",label,".dat",sep=""))
bStats=read.table(paste("generalStatistics_",label,".dat",sep=""))

novA=a[,3:length(a)]
novNovA=rowMeans(novA)


numRuns=profile$Runs
intervalo=100
numMeasures=(profile$Generations-profile$BurnIn)/intervalo
promSize=mat.or.vec(numMeasures,1)
avProm=mat.or.vec(numRuns,1)
(equis=seq(1,numMeasures,1))
plot(-1000,-1000,xlim=c(0,numMeasures+100),ylim=c(0,22))
for(i in 1:numRuns){
  for(j in 1:numMeasures){
    posicion=(i-1)*numMeasures+j
    promSize[j]=novNovA[posicion]
  }
  avProm[i]=mean(promSize)
  lines(equis,promSize,col=i)
  text(numMeasures+50,avProm[i],bStats[i,7],col=i)
}

type=c(1,18)
normBstats=mat.or.vec(numRuns,1)
plot(-100,-100,ylim=c(0,1.1),xlim=c(4,21),ylab="Count relative to maximum",xlab="Average size of Znf array in run")
for(kk in 1:4){
  for(k in 1:2){
  j=(kk-1)*2+k
  maxi=0
  for(i in 1:numRuns){
    if(bStats[i,j]>maxi){
      maxi=bStats[i,j]
    }
  }
  for(i in 1:numRuns){
    normBstats[i]=bStats[i,j]/maxi
  }
  points(avProm,normBstats,col=arr_color[kk],cex=2.5,pch=type[k])
  }
}

legend(x="bottom",ncol=4, #x.intersp=c(0,0,0,0,0.4,0.4,0.4), lwd=c(NA,NA,NA,NA,1,1,1),
       #text.width=c(1,1,1,1,1,1),
       c("Znf","Point","GeneConv","All","Effective","Total"),col=c(arr_color[1],arr_color[2],arr_color[3],arr_color[4],1,1),
       lty=c(1,1,1,1,NA,NA),
      pch=c(NA,NA,NA,NA,type[1],type[2]),cex=1.5,bty="n")

text(7,0.2,label)


#Effective over total
plot(-100,-100,ylim=c(0,0.4),xlim=c(4,21),ylab="Effective / total mutation rate",xlab="Average size of Znf array in run")
for(kk in 1:4){
    k=(kk-1)*2+1
    j=(kk-1)*2+2
    for(i in 1:numRuns){
      normBstats[i]=bStats[i,k]/bStats[i,j]
    }
    points(avProm,normBstats,col=arr_color[kk],cex=2.5,pch=16)
  
}

legend(x="topright",ncol=1, #x.intersp=c(0,0,0,0,0.4,0.4,0.4), lwd=c(NA,NA,NA,NA,1,1,1),
       #text.width=c(1,1,1,1,1,1),
       c("Znf","Point","GeneConv","All"),col=c(arr_color[1],arr_color[2],arr_color[3],arr_color[4]),
       pch=16,cex=1.5,bty="n")

text(15,0.4,label)




