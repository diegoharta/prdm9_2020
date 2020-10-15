### this script intends to show the summary statistics related with the size of the array for several simulation runs.
## it plots the histogram and several summary statistics for the size of the array


setwd(paste("~/Documents/Projects/PZIFE/C_scripts_and_data/dataFromCluster/TrialRun_2018_09_14/",sep=""))

label="prueba_pzife_1.97_N100_t400_s10m_p1_X6_D3_C3_U2"



# PRINT PDF
#pdf(paste("sizeOfArrayStats_",label,".pdf",sep=""), width=8, height= 9)

  novNovA=c(1,200)
  a=read.table(paste("histogramOfSizeOfZnfArray_",label,".dat",sep=""),header=FALSE)
  novA=a[,3:length(a)]
  equis=seq(1,200,1)
  plot(equis,novA[1,])
  novNovA=mat.or.vec(10,200)
  
  plot(-100,-100,xlim=c(0,200),ylim=c(0,20))
  for(i in 1:10){
    for(j in 1:200){
     novNovA[i,j]=mean(as.numeric(novA[((i-1)*200+j),]))
    }
    #lines(equis,novNovA[i,],col=i+1)
  }
  novAMean=colMeans(novNovA)
  lines(equis,colMeans(novNovA),col=1,lwd=2)
  
  novNovASD=mat.or.vec(1,200)
  novNovASD=sd(novNovA)
  
  lines(equis,novAMean+novNovASD,col=1,lty=2)
  lines(equis,novAMean-novNovASD,col=1,lty=2)
  
  prom=mean(novAMean)
  vars=mean(novNovASD)^2
  
  (sizeDispersion=vars)
  (sizeDispersion=vars/prom)
  (sizeDispersion=vars/(prom^2))
  
  plot(-100,100,xlim=c(0,200),ylim=c(.9,3))
  hetz=mat.or.vec(10,200)
  for(i in 1:10){
    for(j in 1:200){
      numbers <- unlist(novA[((i-1)*200+j),])
      suma=0
      for(k in 1:length(table(numbers))){
        freq=(table(numbers)[[k]])/200
        suma=suma+freq*freq
      }
      hetz[i,j]=1/suma
     
    }
    lines(hetz[i,],col=i)
  }
 
(mean(hetz))
  
  
  


  