### This file is intended to print the new allele potentiality

setwd(paste("~/Documents/Projects/PZIFE/C_scripts_and_data/dataFromCluster/TrialRun_2018_09_29/",sep=""))


label="N1000_t200_s1dd"
# PRINT PDF
paths="~/Documents/Projects/PZIFE/C_scripts_and_data/dataFromCluster/TrialRun_2018_09_29/s1dd"
setwd(paths)
#pdf(paste("newAllelePotentiality_",label,".pdf",sep=""), width=8, height= 9)


arr_color=c("#009E73", "#e79f00", "#0072B2", "#9ad0f3", "#D55E00", 
            "#CC79A7", "#F0E442","#000000")

tamany=c(1.2,1.5,1.8,2)
tamany=c(1,1,1,1,1)

m <- rbind(c(1,1,1,1),c(2,2,3,3), c(4,4,5,5))
layout(m,heights = c(1,4,5))
par(mar = c(0.5,5, 0.5, 0.5))
#,xaxs="i")

tipo=c(22,23,24,25,1)
tipo2=c(15,16,17,18)
tipo3=c(7,8,9,10)

plot.new()
legend("top",c("C=0.004", "C=0.04", "C=0.4", "C=4", "U=0.00004", "U=0.0004", "U=0.004","U=0.04","X=0.000004","X=0.00004","X=0.0004","X=0.004"),ncol = 8, 
       col=c(1,1,1,1,arr_color[1],arr_color[2],arr_color[3],arr_color[4],1,1,1,1),pch=c(tipo[1],tipo[2],tipo[3],tipo[4],NA,NA,NA,NA,tipo[5],tipo[5],tipo[5],tipo[5]),
       lty=c(NA,NA,NA,NA,1,1,1,1,NA,NA,NA,NA),pt.cex=c(1,1,1,1,2,2,2,2,tamany[1],tamany[2],tamany[3],tamany[4]),
       lwd=c(NA,NA,NA,NA,3,3,3,3,NA,NA,NA,NA),bty = "n",x.intersp=0.05)


simID="N1000_t200_s1dd"
paths="~/Documents/Projects/PZIFE/C_scripts_and_data/dataFromCluster/TrialRun_2018_09_29/s1dd"
arrayP=c(1)
arrayE=c(5,4)
arrayD=c(1,0)
arrayU=c(4)


par(mar = c(5,5, 0.5, 0.5),xaxs="i")
ymax=1
count=0

contador=0
for(ii in 1:length(arrayP)){
  count=0
  # for(j in 1:length(arrayU)){
  for(kk in 1:length(arrayD)){
    for(ll in 1:length(arrayU)){ 
      countE=0
      for(oo in 1:length(arrayE)){
        contador=contador+1
        p=arrayP[ii]
        X=arrayE[oo]
        D=arrayD[kk]
        C=D
        U=arrayU[ll]
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
            
            newAllele=read.table(paste("novelAllelePotentiality_",label,".dat",sep=""))
            
            xmin=0.00001
            xmax=max(newAllele)*1.3
            ymax=c(12000,13000,1000,1000)
            
            plot(-100,-100,ylim=c(0,ymax[contador]),xlim=c(xmin,xmax),ylab="Relative effective",xlab="",las=2,log="x",sub=label)
            inicio=100
            final=106
            intervalos=1
            mediciones=(final-inicio+1)/intervalos
           # mediciones=1
            for(xx in 1:mediciones){
              lineainicio=inicio+(xx-1)*intervalos
              lineafinal=inicio+(xx)*intervalos
              #lineainicio=xx
              #lineafinal=xx
              datos=newAllele[lineainicio:lineafinal,]
              lines(density(as.numeric(unlist(datos)),adjust=2),col=xx)
              #hist((as.numeric(unlist(datos))))
             
            }
           
          }
        }
        countE=countE+1
        count=count+1
        
      }
      
    }
    
  }
}
abline(v=230,col=arr_color[6])
abline(v=480,col=arr_color[6])
abline(v=730,col=arr_color[6])





axis(1, at=c(500,1500,2500,3500), labels=c("0.1","0.01","0.1","1"))
title(xlab="Alpha",sub = label)

#dev.off()
