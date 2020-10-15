### This file is intended to print the relationship between potentiality and activity


# PRINT PDF
label="N500_t23c_s1h"
paths="~/Documents/Projects/PZIFE/C_scripts_and_data/dataFromCluster/TrialRun_2018_10_08/s1h"
setwd(paths)
#pdf(paste("newAllelePotentiality_",label,".pdf",sep=""), width=8, height= 9)

simID=paste("prueba_pzife_1.98_",label,sep="")
arrayP=c(1)
arrayE=c(4,2)
arrayD=c(1,1,0)
arrayU=c(2)


inicio=21
final=23

arr_color=c("#009E73", "#e79f00", "#0072B2", "#9ad0f3", "#D55E00", 
            "#CC79A7", "#F0E442","#000000")

tamany=c(1.2,1.5,1.8,2)
tamany=c(1,1,1,1,1)

m <- rbind(c(1,1,1,1),c(2,2,5,5), c(3,3,6,6),c(4,4,7,7))
m <- rbind(c(1,1,1,1),c(2,2,3,3), c(4,4,5,5))
#m <- rbind(c(1,1,2,2),c(3,3,4,4))
m <- rbind(c(1,1,2,2),c(3,3,4,4),c(5,5,6,6))
layout(m,heights = c(1,4,4))
layout(m,heights=c(4,4,4))
par(mar = c(0.5,5, 0.5, 0.5))
#,xaxs="i")

tipo=c(22,23,24,25,1)
tipo2=c(15,16,17,18)
tipo3=c(7,8,9,10)

#plot.new()
#legend("top",c("C=0.004", "C=0.04", "C=0.4", "C=4", "U=0.00004", "U=0.0004", "U=0.004","U=0.04","X=0.000004","X=0.00004","X=0.0004","X=0.004"),ncol = 8, 
#       col=c(1,1,1,1,arr_color[1],arr_color[2],arr_color[3],arr_color[4],1,1,1,1),pch=c(tipo[1],tipo[2],tipo[3],tipo[4],NA,NA,NA,NA,tipo[5],tipo[5],tipo[5],tipo[5]),
#       lty=c(NA,NA,NA,NA,1,1,1,1,NA,NA,NA,NA),pt.cex=c(1,1,1,1,2,2,2,2,tamany[1],tamany[2],tamany[3],tamany[4]),
#      lwd=c(NA,NA,NA,NA,3,3,3,3,NA,NA,NA,NA),bty = "n",x.intersp=0.05)

puntos=c()
varian=c()
listaPot=c()
listaAct=c()
i=1

xmin=c(.6,0,.6,0,0.6,0)
xmax=c(1.02,1.02,1.02,1.02,1.02,1.02)
ymin=c(0,0,0,0,0,0)
ymax=c(0.004,0.004,0.0035,0.0035,0.035,0.035)

par(mar = c(5,5, 0.5, 0.5),xaxs="i")
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
        label=paste(simID,"_p",p,"_X",X,"_D",D,"_C",C,"_U",U,sep="")
        setwd(paste(paths,"/",label,sep=""))
        
        
        #if(count == 1 && i==1) {text(1.5,xmax-1,label,cex=1)}
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
            
            newActivity=read.table(paste("novelAlleleActivity_",label,".dat",sep=""))
            
            
            listaPot=as.numeric(unlist(newAllele[0:400,1:100]))
            listaAct=as.numeric(unlist(newActivity[0:400,1:100]))
            
            promAct=rowMeans(newActivity)
            promPot=rowMeans(newAllele)
            avAct=mean(promAct)
            avPot=mean(promPot)
            
            nrow(newAllele)
            plot(100,100,xlim=c(xmin[contador],xmax[contador]),ylim=c(ymin[contador],ymax[contador]),xlab="Activity",ylab="Potentiality")
            for(jj in 500:1300){
              for(kkj in 1:50){
                
             #   points(newActivity[jj,kkj],newAllele[jj,kkj],col=jj)
                #points(promAct[jj],promPot[jj],col=arr_color[oo])
              
              }
              
            }
            points(listaAct,listaPot,cex=0.2,col=3)
            reg=lm(listaPot ~ listaAct)
            summary(reg)
            abline(reg,col=1)
         #   res <- ye - (fit$coefficients[[2]]*equis+fit$coefficients[[1]])
            #lines(equis,res)
          #  lowess(x=equis,y=ye , f = 2/3, iter = 3, delta = 0.01 * diff(range(equis)))
            abline(v=avAct,col=arr_color[oo])
            abline(h=avPot,col=arr_color[oo])
            title(sub = label)
            
            
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



#dev.off()

