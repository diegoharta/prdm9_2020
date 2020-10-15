m=c(1)
layout(m)

simID="N100_t1000_s1r"
paths="~/Documents/Projects/PZIFE/C_scripts_and_data/dataFromCluster/TrialRun_2018_09_24/s1r"
arrayP=c(2)
arrayE=c(5,3,1)
arrayD=c(4,2,0)
arrayU=c(6,4)

setwd(paths)

par(mar = c(5,5, 0.5, 0.5),xaxs="i")
ymax=20
xmin=-100
xmax=700
count=0
plot(-100,-100,ylim=c(0,ymax),xlim=c(xmin,xmax),ylab="Size or array",xlab="",xaxt="n",las=2)
for(i in 1:length(arrayP)){
  count=0
  # for(j in 1:length(arrayU)){
  for(k in 1:length(arrayD)){
    for(l in 1:length(arrayU)){ 
      countE=0
      for(o in 1:length(arrayE)){
        
        p=arrayP[i]
        X=arrayE[o]
        D=arrayD[k]
        C=D
        U=arrayU[l]
        #U=C+2
        label=paste("prueba_pzife_1.97_",simID,"_p",p,"_X",X,"_D",D,"_C",C,"_U",U,sep="")
        setwd(paste(paths,"/",label,sep=""))
        
        
        if(count == 1 && i==1) {text(1.5,xmax-1,label,cex=1)}
        er=("std_error.dat")
        val=file.info(er)$size
        if(is.na(val) == TRUE){
          fileName <- "std_output.txt"
          conn <- file(fileName,open="r")
          linn <-readLines(conn)
          eco=(linn[length(linn)])
          close(conn)
          substr(eco, 1,5)
          if(substr(eco,1,5)=="alpha"){
            
            
            
            a=read.table(paste("histogramOfSizeOfZnfArray_",label,".dat",sep=""),header=FALSE)
            novA=a[,3:length(a)]
            longA=(profile$Generations-profile$BurnIn)/profile$Interval
            novNovA=mat.or.vec(nrow(sel),longA)
            
            for(m in 1:nrow(sel)){
              for(j in 1:longA){
                novNovA[m,j]=mean(as.numeric(novA[((m-1)*longA+j),]))
              }
              #lines(equis,novNovA[i,],col=i+1)
            }
            if(nrow(sel)>1){
              novAMean=colMeans(novNovA)
            }
            else{
              novAMean=novNovA
            }
            novNovASD=mat.or.vec(1,longA)
            novNovASD=sd(novNovA)
            
            
            prom=mean(novAMean)
            stand=mean(novNovASD)
            vars=mean(novNovASD)^2
            
            #  (sizeDispersion=vars)
            #  (sizeDispersion=vars/prom)
            #  (sizeDispersion=vars/(prom^2))
            
            
            #  prom=mean(colMeans(novA))
            
            #prom=vars/prom
            points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*15),prom,col=arr_color[l],pch=tipo[k],cex=tamany[o])
            points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*15),prom+stand,col=1,pch=1,cex=0.3)
            points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*15),prom-stand,col=1,pch=1,cex=0.3)
          }
        }
        countE=countE+1
        count=count+1
        
      }
      
    }
    
  }
  
}
axis(1, at=c(500,1500,2500,3500), labels=c("0.1","0.01","0.1","1"))
title(xlab="Alpha",sub = label)