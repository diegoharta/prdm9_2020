### This is the new file changing from effective to real effective muts.

setwd(paste("~/Documents/Projects/PZIFE/C_scripts_and_data/dataFromCluster/TrialRun_2018_09_11/",sep=""))

label="N1000_t400_s10m"
label="N1000_t20_s1aa"
label="N1000_t200_s5ff"
# PRINT PDF
paths="~/Documents/Projects/PZIFE/C_scripts_and_data/dataFromCluster/TrialRun_2018_09_14/s10m"
paths="~/Documents/Projects/PZIFE/C_scripts_and_data/dataFromCluster/TrialRun_2018_09_29/s5ff"
setwd(paths)
pdf(paste("effectiveVsReal_",label,".pdf",sep=""), width=8, height= 9)


arr_color=c("#009E73", "#e79f00", "#0072B2", "#9ad0f3", "#D55E00", 
            "#CC79A7", "#F0E442","#000000")

tamany=c(1.2,1.5,1.8,2)
tamany=c(1,1,1,1,1)

m <- rbind(c(1,1,1,1),c(2,2,3,3), c(4,4,5,5),c(6,6,7,7))
layout(m,heights = c(1,3,3,4))
par(mar = c(0.5,0.5, 0.5, 0.5))
#,xaxs="i")

tipo=c(22,23,24,25,1)
tipo2=c(15,16,17,18)
tipo3=c(7,8,9,10)

plot.new()
#legend("top",c("C=0.004", "C=0.04", "C=0.4", "C=4", "U=0.00004", "U=0.0004", "U=0.004","U=0.04","X=0.000004","X=0.00004","X=0.0004","X=0.004"),ncol = 8, 
 #      col=c(1,1,1,1,arr_color[1],arr_color[2],arr_color[3],arr_color[4],1,1,1,1),pch=c(tipo[1],tipo[2],tipo[3],tipo[4],NA,NA,NA,NA,tipo[5],tipo[5],tipo[5],tipo[5]),
  #     lty=c(NA,NA,NA,NA,1,1,1,1,NA,NA,NA,NA),pt.cex=c(1,1,1,1,2,2,2,2,tamany[1],tamany[2],tamany[3],tamany[4]),
   #    lwd=c(NA,NA,NA,NA,3,3,3,3,NA,NA,NA,NA),bty = "n",x.intersp=0.05)


simID="N1000_t400_s10m"
paths="~/Documents/Projects/PZIFE/C_scripts_and_data/dataFromCluster/TrialRun_2018_09_14/s10m"
arrayP=c(1)
arrayE=c(6,5,4)
arrayD=c(3,2,1,0)
arrayU=c(5,4,3,2)

simID="N1000_t20_s1aa"
paths="~/Documents/Projects/PZIFE/C_scripts_and_data/dataFromCluster/TrialRun_2018_09_29/s1aa3"
arrayP=c(1)
arrayE=c(6,4)
arrayD=c(1,0)
arrayU=c(4,2)

simID="N1000_t200_s1dd"
paths="~/Documents/Projects/PZIFE/C_scripts_and_data/dataFromCluster/TrialRun_2018_09_29/s1dd"
arrayP=c(1)
arrayE=c(6,5,4)
arrayD=c(2,1,0)
arrayU=c(6,4)

totSims=2

simID="N1000_t200_s5ff"
paths="~/Documents/Projects/PZIFE/C_scripts_and_data/dataFromCluster/TrialRun_2018_09_29/s5ff"
arrayP=c(1)
arrayE=c(6,5,4)
arrayD=c(2,1,0)
arrayU=c(4)

totSims=5

xmin=-30
xmax=1005
intervalo1=xmax/length(arrayD)
intervalo2=(intervalo1-10)/length(arrayU)
intervalo3=(intervalo2-10)/length(arrayE)

tamany=c(1.3,1.3,1.3)

valoresGeneConv=c()
valoresZnf=c()
valoresPoint=c()

par(mar = c(0.5,5, 0.5, 0.5),xaxs="i")
ymax=1
count=0
plot(-100,-100,ylim=c(0,ymax),xlim=c(xmin,xmax),ylab="Relative effective",xlab="",xaxt="n",las=2)
for(ii in 1:length(arrayP)){
  count=0
  # for(j in 1:length(arrayU)){
  for(kk in 1:length(arrayD)){
    for(ll in 1:length(arrayU)){ 
      countE=0
      for(oo in 1:length(arrayE)){
        for(uu in 1:totSims){
        p=arrayP[ii]
        X=arrayE[oo]
        D=arrayD[kk]
        C=D
        U=arrayU[ll]
        uu=uu-1
        #U=C+2
        label=paste("prueba_pzife_1.97_",simID,"_p",p,"_X",X,"_D",D,"_C",C,"_U",U,"_s",uu,sep="")
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
            
            gralStats=c()
            gralStatsSD=c()
            mutRate=c()
            epsilonRatio=c()
            bStats=read.table(paste("generalStatistics_",label,".dat",sep=""))
            for(k in 1:14){
              gralStats[k]=mean(as.numeric(unlist(bStats[,k])))
              gralStatsSD[k]=sd(as.numeric(unlist(bStats[,k])))
            }
            valoresGeneConv[uu]=gralStats[5]/gralStats[7]
            valoresZnf[uu]=gralStats[1]/gralStats[7]
            valoresPoint[uu]=Point=gralStats[3]/gralStats[7]
          }}}
        prom=mean(as.numeric(valoresGeneConv))
        stand=sd(as.numeric(valoresGeneConv))
        
        points(((ii-1)*xmax+((kk-1)*intervalo1)+((ll-1)*intervalo2)+(oo-1)*intervalo3),prom,col=arr_color[ll],pch=tipo[kk],cex=tamany[oo])
       # points(((ii-1)*xmax+((kk-1)*intervalo1)+((ll-1)*intervalo2)+(oo-1)*intervalo3),prom+stand,col=1,pch=1,cex=0.3)
      #  points(((ii-1)*xmax+((kk-1)*intervalo1)+((ll-1)*intervalo2)+(oo-1)*intervalo3),prom+stand,col=1,pch=1,cex=0.3)
        
        
        prom=mean(as.numeric(valoresZnf))
        stand=sd(as.numeric(valoresZnf))
        
        points(((ii-1)*xmax+((kk-1)*intervalo1)+((ll-1)*intervalo2)+(oo-1)*intervalo3),prom,col=arr_color[ll],pch=tipo2[kk],cex=tamany[oo])
       # points(((ii-1)*xmax+((kk-1)*intervalo1)+((ll-1)*intervalo2)+(oo-1)*intervalo3),prom+stand,col=1,pch=1,cex=0.3)
      #  points(((ii-1)*xmax+((kk-1)*intervalo1)+((ll-1)*intervalo2)+(oo-1)*intervalo3),prom+stand,col=1,pch=1,cex=0.3)
        
        prom=mean(as.numeric(valoresPoint))
        stand=sd(as.numeric(valoresPoint))
        
        points(((ii-1)*xmax+((kk-1)*intervalo1)+((ll-1)*intervalo2)+(oo-1)*intervalo3),prom,col=arr_color[ll],pch=tipo3[kk],cex=tamany[oo])
       # points(((ii-1)*xmax+((kk-1)*intervalo1)+((ll-1)*intervalo2)+(oo-1)*intervalo3),prom+stand,col=1,pch=1,cex=0.3)
      #  points(((ii-1)*xmax+((kk-1)*intervalo1)+((ll-1)*intervalo2)+(oo-1)*intervalo3),prom+stand,col=1,pch=1,cex=0.3)
        
        
         
        countE=countE+1
        count=count+1
        
      }
      
    }
    
  }
}
abline(v=320,col=arr_color[6])
abline(v=650,col=arr_color[6])
abline(v=999,col=arr_color[6])



par(mar = c(0.5,5, 0.5, 0.5),xaxs="i")
ymax=1
count=0
plot(-100,-100,ylim=c(0,ymax),xlim=c(xmin,xmax),ylab="Relative real",xlab="",xaxt="n",las=2)
for(ii in 1:length(arrayP)){
  count=0
  # for(j in 1:length(arrayU)){
  for(kk in 1:length(arrayD)){
    for(ll in 1:length(arrayU)){ 
      countE=0
      for(oo in 1:length(arrayE)){
        for(uu in 1:totSims){
          p=arrayP[ii]
          X=arrayE[oo]
          D=arrayD[kk]
          C=D
          U=arrayU[ll]
          uu=uu-1
          #U=C+2
          label=paste("prueba_pzife_1.97_",simID,"_p",p,"_X",X,"_D",D,"_C",C,"_U",U,"_s",uu,sep="")
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
            
            gralStats=c()
            gralStatsSD=c()
            mutRate=c()
            epsilonRatio=c()
            bStats=read.table(paste("generalStatistics_",label,".dat",sep=""))
            for(k in 1:12){
              gralStats[k]=mean(as.numeric(unlist(bStats[,k])))
              gralStatsSD[k]=sd(as.numeric(unlist(bStats[,k])))
            }
            
            tot=gralStats[10]+gralStats[11]+gralStats[12]
            valoresGeneConv[uu]=gralStats[12]/tot
            valoresZnf[uu]=gralStats[10]/tot
            valoresPoint[uu]=gralStats[11]/tot
          }}}
        prom=mean(as.numeric(valoresGeneConv))
        stand=sd(as.numeric(valoresGeneConv))
        
        points(((ii-1)*xmax+((kk-1)*intervalo1)+((ll-1)*intervalo2)+(oo-1)*intervalo3),prom,col=arr_color[ll],pch=tipo[kk],cex=tamany[oo])
        # points(((ii-1)*xmax+((kk-1)*intervalo1)+((ll-1)*intervalo2)+(oo-1)*intervalo3),prom+stand,col=1,pch=1,cex=0.3)
        #  points(((ii-1)*xmax+((kk-1)*intervalo1)+((ll-1)*intervalo2)+(oo-1)*intervalo3),prom+stand,col=1,pch=1,cex=0.3)
        
        
        prom=mean(as.numeric(valoresZnf))
        stand=sd(as.numeric(valoresZnf))
        
        points(((ii-1)*xmax+((kk-1)*intervalo1)+((ll-1)*intervalo2)+(oo-1)*intervalo3),prom,col=arr_color[ll],pch=tipo2[kk],cex=tamany[oo])
        # points(((ii-1)*xmax+((kk-1)*intervalo1)+((ll-1)*intervalo2)+(oo-1)*intervalo3),prom+stand,col=1,pch=1,cex=0.3)
        #  points(((ii-1)*xmax+((kk-1)*intervalo1)+((ll-1)*intervalo2)+(oo-1)*intervalo3),prom+stand,col=1,pch=1,cex=0.3)
        
        prom=mean(as.numeric(valoresPoint))
        stand=sd(as.numeric(valoresPoint))
        
        points(((ii-1)*xmax+((kk-1)*intervalo1)+((ll-1)*intervalo2)+(oo-1)*intervalo3),prom,col=arr_color[ll],pch=tipo3[kk],cex=tamany[oo])
        # points(((ii-1)*xmax+((kk-1)*intervalo1)+((ll-1)*intervalo2)+(oo-1)*intervalo3),prom+stand,col=1,pch=1,cex=0.3)
        #  points(((ii-1)*xmax+((kk-1)*intervalo1)+((ll-1)*intervalo2)+(oo-1)*intervalo3),prom+stand,col=1,pch=1,cex=0.3)
        
        
        
        countE=countE+1
        count=count+1
        
      }
      
    }
    
  }
}
abline(v=320,col=arr_color[6])
abline(v=650,col=arr_color[6])
abline(v=999,col=arr_color[6])



ymax=.35
count=0
plot(-100,-100,ylim=c(0,ymax),xlim=c(xmin,xmax),ylab="Effective over all",xlab="",xaxt="n",las=2)
for(ii in 1:length(arrayP)){
  count=0
  # for(j in 1:length(arrayU)){
  for(kk in 1:length(arrayD)){
    for(ll in 1:length(arrayU)){ 
      countE=0
    
      for(oo in 1:length(arrayE)){
      for(uu in 1:totSims){
        p=arrayP[ii]
        X=arrayE[oo]
        D=arrayD[kk]
        C=D
        U=arrayU[ll]
        uu=uu-1
        #U=C+2
        label=paste("prueba_pzife_1.97_",simID,"_p",p,"_X",X,"_D",D,"_C",C,"_U",U,"_s",uu,sep="")
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
            
            gralStats=c()
            gralStatsSD=c()
            mutRate=c()
            epsilonRatio=c()
            bStats=read.table(paste("generalStatistics_",label,".dat",sep=""))
            for(k in 1:14){
              gralStats[k]=mean(as.numeric(unlist(bStats[,k])))
              gralStatsSD[k]=sd(as.numeric(unlist(bStats[,k])))
            }
            valoresGeneConv[uu]=gralStats[5]/gralStats[6]
            valoresZnf[uu]=gralStats[1]/gralStats[2]
            valoresPoint[uu]=gralStats[3]/gralStats[4]
          }}}
        prom=mean(as.numeric(valoresGeneConv))
        stand=sd(as.numeric(valoresGeneConv))
        
        points(((ii-1)*xmax+((kk-1)*intervalo1)+((ll-1)*intervalo2)+(oo-1)*intervalo3),prom,col=arr_color[ll],pch=tipo[kk],cex=tamany[oo])
        # points(((ii-1)*xmax+((kk-1)*intervalo1)+((ll-1)*intervalo2)+(oo-1)*intervalo3),prom+stand,col=1,pch=1,cex=0.3)
        #  points(((ii-1)*xmax+((kk-1)*intervalo1)+((ll-1)*intervalo2)+(oo-1)*intervalo3),prom+stand,col=1,pch=1,cex=0.3)
        
        
        prom=mean(as.numeric(valoresZnf))
        stand=sd(as.numeric(valoresZnf))
        
        points(((ii-1)*xmax+((kk-1)*intervalo1)+((ll-1)*intervalo2)+(oo-1)*intervalo3),prom,col=arr_color[ll],pch=tipo2[kk],cex=tamany[oo])
        # points(((ii-1)*xmax+((kk-1)*intervalo1)+((ll-1)*intervalo2)+(oo-1)*intervalo3),prom+stand,col=1,pch=1,cex=0.3)
        #  points(((ii-1)*xmax+((kk-1)*intervalo1)+((ll-1)*intervalo2)+(oo-1)*intervalo3),prom+stand,col=1,pch=1,cex=0.3)
        
        prom=mean(as.numeric(valoresPoint))
        stand=sd(as.numeric(valoresPoint))
        
        points(((ii-1)*xmax+((kk-1)*intervalo1)+((ll-1)*intervalo2)+(oo-1)*intervalo3),prom,col=arr_color[ll],pch=tipo3[kk],cex=tamany[oo])
        # points(((ii-1)*xmax+((kk-1)*intervalo1)+((ll-1)*intervalo2)+(oo-1)*intervalo3),prom+stand,col=1,pch=1,cex=0.3)
        #  points(((ii-1)*xmax+((kk-1)*intervalo1)+((ll-1)*intervalo2)+(oo-1)*intervalo3),prom+stand,col=1,pch=1,cex=0.3)
        
        
        
        countE=countE+1
        count=count+1
        
      }
    }
    
  }
}

abline(v=320,col=arr_color[6])
abline(v=650,col=arr_color[6])
abline(v=999,col=arr_color[6])





ymax=.2
count=0
plot(-100,100,ylim=c(0.0000001,ymax),xlim=c(xmin,xmax),ylab="Real over all",xlab="",xaxt="n",las=2,log="y")
for(ii in 1:length(arrayP)){
  count=0
  # for(j in 1:length(arrayU)){
  for(kk in 1:length(arrayD)){
    for(ll in 1:length(arrayU)){ 
      countE=0
      
      for(oo in 1:length(arrayE)){
        for(uu in 1:totSims){
          p=arrayP[ii]
          X=arrayE[oo]
          D=arrayD[kk]
          C=D
          U=arrayU[ll]
          uu=uu-1
          #U=C+2
          label=paste("prueba_pzife_1.97_",simID,"_p",p,"_X",X,"_D",D,"_C",C,"_U",U,"_s",uu,sep="")
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
              
              gralStats=c()
              gralStatsSD=c()
              mutRate=c()
              epsilonRatio=c()
              bStats=read.table(paste("generalStatistics_",label,".dat",sep=""))
              for(k in 1:14){
                gralStats[k]=mean(as.numeric(unlist(bStats[,k])))
                gralStatsSD[k]=sd(as.numeric(unlist(bStats[,k])))
              }
              valoresGeneConv[uu]=gralStats[12]/gralStats[6]
              valoresZnf[uu]=gralStats[10]/gralStats[2]
              valoresPoint[uu]=gralStats[11]/gralStats[4]
            }}}
        prom=mean(as.numeric(valoresGeneConv))
        stand=sd(as.numeric(valoresGeneConv))
        
        points(((ii-1)*xmax+((kk-1)*intervalo1)+((ll-1)*intervalo2)+(oo-1)*intervalo3),prom,col=arr_color[ll],pch=tipo[kk],cex=tamany[oo])
        # points(((ii-1)*xmax+((kk-1)*intervalo1)+((ll-1)*intervalo2)+(oo-1)*intervalo3),prom+stand,col=1,pch=1,cex=0.3)
        #  points(((ii-1)*xmax+((kk-1)*intervalo1)+((ll-1)*intervalo2)+(oo-1)*intervalo3),prom+stand,col=1,pch=1,cex=0.3)
        
        
        prom=mean(as.numeric(valoresZnf))
        stand=sd(as.numeric(valoresZnf))
        
        points(((ii-1)*xmax+((kk-1)*intervalo1)+((ll-1)*intervalo2)+(oo-1)*intervalo3),prom,col=arr_color[ll],pch=tipo2[kk],cex=tamany[oo])
        # points(((ii-1)*xmax+((kk-1)*intervalo1)+((ll-1)*intervalo2)+(oo-1)*intervalo3),prom+stand,col=1,pch=1,cex=0.3)
        #  points(((ii-1)*xmax+((kk-1)*intervalo1)+((ll-1)*intervalo2)+(oo-1)*intervalo3),prom+stand,col=1,pch=1,cex=0.3)
        
        prom=mean(as.numeric(valoresPoint))
        stand=sd(as.numeric(valoresPoint))
        
        points(((ii-1)*xmax+((kk-1)*intervalo1)+((ll-1)*intervalo2)+(oo-1)*intervalo3),prom,col=arr_color[ll],pch=tipo3[kk],cex=tamany[oo])
        # points(((ii-1)*xmax+((kk-1)*intervalo1)+((ll-1)*intervalo2)+(oo-1)*intervalo3),prom+stand,col=1,pch=1,cex=0.3)
        #  points(((ii-1)*xmax+((kk-1)*intervalo1)+((ll-1)*intervalo2)+(oo-1)*intervalo3),prom+stand,col=1,pch=1,cex=0.3)
        
        
        
        countE=countE+1
        count=count+1
        
      }
    }
    
  }
}

abline(v=320,col=arr_color[6])
abline(v=650,col=arr_color[6])
abline(v=999,col=arr_color[6])


valoresReal=c()
par(mar = c(5,5, 0.5, 0.5),xaxs="i")
ymax=5e1
ymin=1e-1
count=0
plot(100000,100,ylim=c(ymin,ymax),xlim=c(xmin,xmax),ylab="Total effective mutation rate",xlab="",xaxt="n",las=2,log = "y")
for(ii in 1:length(arrayP)){
  count=0
  # for(j in 1:length(arrayU)){
  for(kk in 1:length(arrayD)){
    for(ll in 1:length(arrayU)){ 
      countE=0
      for(oo in 1:length(arrayE)){
        for(uu in 1:totSims){
          p=arrayP[ii]
          X=arrayE[oo]
          D=arrayD[kk]
          C=D
          U=arrayU[ll]
          uu=uu-1
          #U=C+2
          label=paste("prueba_pzife_1.97_",simID,"_p",p,"_X",X,"_D",D,"_C",C,"_U",U,"_s",uu,sep="")
          setwd(paste(paths,"/",label,sep=""))
          
          
          # if(count == 1 && i==1) {text(1.5,xmax-1,label,cex=1)}
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
              
              #Real Effective mutation rate
              
              gralStats=c()
              mutRate=c()
              mutEffRate=c()
              mutRealRate=c()
              epsilonRatio=c()
              bStats=read.table(paste("generalStatistics_",label,".dat",sep=""))
              profile=read.table(paste("profile_",label,".dat",sep=""),header = TRUE)
              mutRealRate=(bStats[1,7])/((profile$Generations-profile$BurnIn))/(2*profile$PopulationSize)*(4*profile$PopulationSize)
              valoresReal[uu]=mutRealRate
              
              
            }
          }
        }
        prom=mean(as.numeric(valoresReal))
        stand=sd(as.numeric(valoresReal))
        
        points(((ii-1)*xmax+((kk-1)*intervalo1)+((ll-1)*intervalo2)+(oo-1)*intervalo3),prom,col=arr_color[ll],pch=tipo[kk],cex=tamany[oo])
        points(((ii-1)*xmax+((kk-1)*intervalo1)+((ll-1)*intervalo2)+(oo-1)*intervalo3),prom+stand,col=1,pch=1,cex=0.3)
        points(((ii-1)*xmax+((kk-1)*intervalo1)+((ll-1)*intervalo2)+(oo-1)*intervalo3),prom-stand,col=1,pch=1,cex=0.3)
        
        
        countE=countE+1
        count=count+1
        
      }
      
    }
    
  }
}

abline(v=320,col=arr_color[6])
abline(v=650,col=arr_color[6])
abline(v=999,col=arr_color[6])



valoresReal=c()
par(mar = c(5,5, 0.5, 0.5),xaxs="i")
ymax=1e-2
ymin=1e-3
count=0
plot(100000,100,ylim=c(ymin,ymax),xlim=c(xmin,xmax),ylab="Total real mutation rate",xlab="",xaxt="n",las=2,log = "y")
for(ii in 1:length(arrayP)){
  count=0
  # for(j in 1:length(arrayU)){
  for(kk in 1:length(arrayD)){
    for(ll in 1:length(arrayU)){ 
      countE=0
      for(oo in 1:length(arrayE)){
      for(uu in 1:totSims){
        p=arrayP[ii]
        X=arrayE[oo]
        D=arrayD[kk]
        C=D
        U=arrayU[ll]
        uu=uu-1
        #U=C+2
        label=paste("prueba_pzife_1.97_",simID,"_p",p,"_X",X,"_D",D,"_C",C,"_U",U,"_s",uu,sep="")
        setwd(paste(paths,"/",label,sep=""))
        
        
       # if(count == 1 && i==1) {text(1.5,xmax-1,label,cex=1)}
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
            
            #Real Effective mutation rate
            
            gralStats=c()
            mutRate=c()
            mutEffRate=c()
            mutRealRate=c()
            epsilonRatio=c()
            bStats=read.table(paste("generalStatistics_",label,".dat",sep=""))
            profile=read.table(paste("profile_",label,".dat",sep=""),header = TRUE)
            mutRealRate=(bStats[1,13])/((profile$Generations-profile$BurnIn))/(2*profile$PopulationSize)*(4*profile$PopulationSize)
            valoresReal[uu]=mutRealRate
            
            
          }
        }
      }
        prom=mean(as.numeric(valoresReal))
        stand=sd(as.numeric(valoresReal))
        
            points(((ii-1)*xmax+((kk-1)*intervalo1)+((ll-1)*intervalo2)+(oo-1)*intervalo3),prom,col=arr_color[ll],pch=tipo[kk],cex=tamany[oo])
            points(((ii-1)*xmax+((kk-1)*intervalo1)+((ll-1)*intervalo2)+(oo-1)*intervalo3),prom+stand,col=1,pch=1,cex=0.3)
            points(((ii-1)*xmax+((kk-1)*intervalo1)+((ll-1)*intervalo2)+(oo-1)*intervalo3),prom-stand,col=1,pch=1,cex=0.3)
            
          
        countE=countE+1
        count=count+1
        
      }
      
    }
    
  }
}

abline(v=320,col=arr_color[6])
abline(v=650,col=arr_color[6])
abline(v=999,col=arr_color[6])


axis(1, at=c(500,1500,2500,3500), labels=c("0.1","0.01","0.1","1"))
title(xlab="Alpha",sub = label)

dev.off()
