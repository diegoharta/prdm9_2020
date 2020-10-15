
#THIS FILE CREATS A PDF WITH THE SUMMARY RESULTS FROM A SET OF SIMULATIONS
#THEN IT GENERATES AN EXAMPLE (FIRST RUN) OF EACH SET OF SIMULATIONS

#THIS FILE INTENDS TO SHOW THE EVOLUTION OF SEVERAL SUMMARY STATISTICS
#This file intends to show the results from several PZIFE simulations of several runs each 
#For each simulation it will plot the evolution in time of the summary statistics (diversity, recActivity,selCoeff) 
# and will print the average histogram

#It shows the relevant statistics that characterize the evolutionary scenario under which the Red-Queen is developing
#It generates one pdf file 

setwd(paste("~/Documents/Projects/PZIFE/C_scripts_and_data/dataFromCluster/TrialRun_2018_09_11/",sep=""))

label="N1000_t400_s10m"
# PRINT PDF
#pdf(paste("masterPlot_",label,".pdf",sep=""), width=8, height= 9)


arr_color=c("#009E73", "#e79f00", "#0072B2", "#9ad0f3", "#D55E00", 
            "#CC79A7", "#F0E442","#000000")

tamany=c(1.2,1.5,1.8,2)

m <- rbind(c(1,1,1), c(2,2,2),c(3,3,3),c(4,4,4),c(5,5,5),c(6,6,6),c(7,7,7),c(8,8,8),c(9,9,9))
layout(m,heights = c(1,2,2,2,2,2,2,2,3))
par(mar = c(0.5,5, 0.5, 0.5))
#,xaxs="i")

tipo=c(22,23,24,25,1)
arrayP=c(-1,0,1,2,3)
arrayE=c(6,4,2)
arrayD=c(4,3,2,1,0)

arrayP=c(2,1)
arrayE=c(3,4,5)
arrayD=c(2,1,0)
arrayU=c(4,3,2)

#superArrayP=c(2,1,0)
#for(xx in 1:4){
# num=superArrayP[xx]
#arrayP=c(num)



simID="N1000_t130_s1j2"
paths="~/Documents/Projects/PZIFE/C_scripts_and_data/dataFromCluster/TrialRun_2018_09_13"
#arrayP=c(2)
arrayE=c(3,5)
arrayD=c(2,1,0)
arrayU=c(4,3,2)


simID="N1000_t200_s10h"
paths="~/Documents/Projects/PZIFE/C_scripts_and_data/dataFromCluster/TrialRun_2018_09_11"
arrayP=c(3)
arrayE=c(6,5,4,3)
arrayD=c(3,2,1,0)
arrayU=c(5,4,3,2)





simID="N10000_t30_s1l2"
paths="~/Documents/Projects/PZIFE/C_scripts_and_data/dataFromCluster/TrialRun_2018_09_14"
arrayP=c(2)
arrayE=c(5,4,3)
arrayD=c(3,2,1)
arrayU=c(5,4,3)




simID="N1000_t200_s10h"
paths="~/Documents/Projects/PZIFE/C_scripts_and_data/dataFromCluster/TrialRun_2018_09_11"
arrayP=c(2)
arrayE=c(6,5,4,3)
arrayD=c(3,2,1,0)
arrayU=c(5,4,3,2)



simID="N100_t1000_s1r"
paths="~/Documents/Projects/PZIFE/C_scripts_and_data/dataFromCluster/TrialRun_2018_09_24/s1r"
arrayP=c(0)
arrayE=c(6,5)
arrayD=c(4,2,0)
arrayU=c(6,4,2)

simID="N1000_t400_s10m"
paths="~/Documents/Projects/PZIFE/C_scripts_and_data/dataFromCluster/TrialRun_2018_09_14/s10m"
arrayP=c(2)
arrayE=c(6,5,4)
arrayD=c(2,1,0)
arrayU=c(5,4,3,2)

label="N400_t23_s1j"
paths="~/Documents/Projects/PZIFE/C_scripts_and_data/dataFromCluster/TrialRun_2018_10_08/s1j"
setwd(paths)
simID=paste("prueba_pzife_1.98_",label,sep="")
arrayP=c(2)
arrayE=c(5,4,3,2)
arrayD=c(3,2,1,0)
arrayU=c(4,3,2,1)



plot.new()
legend("top",c("C=0.004", "C=0.04", "C=0.4", "C=4", "U=0.00004", "U=0.0004", "U=0.004","U=0.04","X=0.000004","X=0.00004","X=0.0004","X=0.004"),ncol = 8, 
       col=c(1,1,1,1,arr_color[1],arr_color[2],arr_color[3],arr_color[4],1,1,1,1),pch=c(tipo[1],tipo[2],tipo[3],tipo[4],NA,NA,NA,NA,tipo[5],tipo[5],tipo[5],tipo[5]),
       lty=c(NA,NA,NA,NA,1,1,1,1,NA,NA,NA,NA),pt.cex=c(1,1,1,1,2,2,2,2,tamany[1],tamany[2],tamany[3],tamany[4]),
       lwd=c(NA,NA,NA,NA,3,3,3,3,NA,NA,NA,NA),bty = "n",x.intersp=0.05)
#legend("top",c("C=0.004", "C=0.04", "C=0.4" , "U=0.00004", "U=0.0004","U=0.004","X=0.00004","X=0.004","X=0.4"),ncol = 9, 
#col=c(1,1,1,arr_color[1],arr_color[2],arr_color[3],1,1,1),pch=c(tipo[1],tipo[2],tipo[3],NA,NA,NA,tipo[4],tipo[4],tipo[4]),
#lty=c(NA,NA,NA,1,1,1,NA,NA,NA),pt.cex=c(1,1,1,2,2,2,tamany[1],tamany[2],tamany[3]),
#lwd=c(NA,NA,NA,3,3,3,NA,NA,NA),bty = "n",x.intersp=0.05)

valores=mat.or.vec(1,1)

init=0
limX=98
Prop=5
tamanyo=.8
ymax=20
xmin=-10
xmax=1000

count=0
plot(-100,1000,ylim=c(.001,ymax),
     xlim=c(xmin,xmax),ylab="Effective mutation rate",xlab="",xaxt="n",las=2,log="y")
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
        label=paste(simID,"_p",p,"_X",X,"_D",D,"_C",C,"_U",U,sep="")
        setwd(paste(paths,"/",label,sep=""))
        
        #  if(count == 1 && i==1) {text(1.5,1000,label,cex=1)}
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
            
            #Effective mutation rate
            
            gralStats=c()
            mutRate=c()
            epsilonRatio=c()
            bStats=read.table(paste("generalStatistics_",label,".dat",sep=""))
            profile=read.table(paste("profile_",label,".dat",sep=""),header = TRUE)
            
            for(m in 1:nrow(bStats)){
              
              # for(kk in 1:9){
              #  gralStats[kk]=unlist(as.numeric(bStats[kk]))
              #}
              #mutRate[m]=gralStats[9]/((profile$Generations-profile$BurnIn))/(2*profile$PopulationSize)*(4*profile$PopulationSize)
              mutRate=bStats[m,9]/((profile$Generations-profile$BurnIn))/(2*profile$PopulationSize)*(4*profile$PopulationSize)
              
              #text(75*Prop,24,bquote(paste(mu," = ",.(mutRate[ii]),sep="")),cex=tamanyo)
              epsilonRatio=profile$ErosionRate/mutRate
              #text(75*Prop,22.5,bquote(paste(epsilon," = ",.(epsilonRatio[ii]),sep="")),cex=tamanyo)
              #points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*15),mutRate,col=arr_color[l],pch=tipo[k],cex=tamany[o])
              valores[m]=mutRate
            }
            prom=mean(as.numeric(valores))
            stand=sd(as.numeric(valores))
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
#legend("topright",c("C=0.04", "C=0.4", "C=4" , "U=0.0004", "U=0.004", "U=0.04"),ncol = 1, 
#      col=c(1,1,1,arr_color[1],arr_color[2],arr_color[3]),pch=c(tipo[1],tipo[2],tipo[3],NA,NA,NA),
#     lty=c(NA,NA,NA,1,1,1))


init=0
limX=98
Prop=5
tamanyo=.8
ymax=20

count=0
plot(-100,-100,ylim=c(0,ymax),xlim=c(xmin,xmax),ylab="Prdm9 diversity",xlab="",xaxt="n",las=2)
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
        label=paste(simID,"_p",p,"_X",X,"_D",D,"_C",C,"_U",U,sep="")
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
            #PRDM9 DIVERSITY
            divt=read.table(paste("prdmDiversity_",label,".dat",sep=""))
            
            
            for(m in 1:nrow(divt)){
              promDiv=mean(as.numeric(divt[m,(ncol(divt)/4):ncol(divt)]))
              #points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*15),promDiv,col=arr_color[l],pch=tipo[k],cex=tamany[o])
              valores[m]=promDiv
            }
            prom=mean(as.numeric(valores))
            stand=sd(as.numeric(valores))
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
abline(h=10,col=2)


ymax=1
count=0
plot(-100,-100,ylim=c(0,ymax),xlim=c(xmin,xmax),ylab="Recombination Activity",xlab="",xaxt="n",las=2)
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
        label=paste(simID,"_p",p,"_X",X,"_D",D,"_C",C,"_U",U,sep="")
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
            
            #PRINT RECOMBINATION ACTIVITY AS LATRILLE ET AL 2017
            rec=read.table(paste("recombinationActivity_",label,".dat",sep=""))
            
            for(m in 1:nrow(rec)){
              promRec=mean(as.numeric(rec[m,(ncol(rec)/4):ncol(rec)]))
              #points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*15),promRec,col=arr_color[l],pch=tipo[k],cex=tamany[o])
              valores[m]=promRec
            }
            prom=mean(as.numeric(valores))
            stand=sd(as.numeric(valores))
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
abline(h=.6,col=2)

ymax=2000
count=0
plot(-100,1,ylim=c(1,ymax),xlim=c(xmin,xmax),ylab="Selection coefficient",xlab="",xaxt="n",las=2,log="y")
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
        label=paste(simID,"_p",p,"_X",X,"_D",D,"_C",C,"_U",U,sep="")
        setwd(paste(paths,"/",label,sep=""))
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
            #    if(count == 1 && i==1) {text(1.5,xmax-1,label,cex=1)}
            
            #PRINT selection coefficient as Latrille et al 2017
            sel=read.table(paste("selectionCoefficient_",label,".dat",sep=""))
            
            for(m in 1:nrow(sel)){
              promSel=mean(as.numeric(sel[m,(ncol(sel)/4):ncol(sel)]))*4*profile$PopulationSize
             # points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*15),promSel,col=arr_color[l],pch=tipo[k],cex=tamany[o])
              valores[m]=promSel
            }
            prom=mean(as.numeric(valores))
            stand=sd(as.numeric(valores))
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
abline(h=26,col=2)

ymax=1
count=0
plot(-100,-100,ylim=c(0,ymax),xlim=c(xmin,xmax),ylab="AA Div. at Binding sites",xlab="",xaxt="n",las=2)
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
        label=paste(simID,"_p",p,"_X",X,"_D",D,"_C",C,"_U",U,sep="")
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
            
            #PRINTS MEAN AA DIVERSITY AT PRDM9 BINDING SITES
            divW=read.table(paste("meanDiversityWithinZnf_",label,".dat",sep=""))
            if(any(is.na(divW)) == FALSE){
              Res = profile$RelevantResidues*2+1;
              
              aaDivInPrdm9Binding=mat.or.vec(1,nrow(divW))
              for(nn in 1:nrow(divW)){
                sumRelRes=divW[nn,2]+divW[nn,4]+divW[nn,6]
                if(sum(divW[nn,])!=0){
                  aaDivInPrdm9Binding[nn]=sumRelRes/sum(divW[nn,])
                }
                else{
                  aaDivInPrdm9Binding[nn]=0.5
                }
              }
              
              #  for(m in 1:(nrow(divW)/nrow(sel))){
              prom=mean(as.numeric(aaDivInPrdm9Binding[(ncol(aaDivInPrdm9Binding)/4):ncol(aaDivInPrdm9Binding)]))
              points( ((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*15),prom,col=arr_color[l],pch=tipo[k],cex=tamany[o])
              
              # points(((i-1)*1000+((k-1)*333)+((l-1)*111)+(o-1)*30),prom,col=arr_color[l],pch=tipo[k],cex=tamany[o])
              # }
            }
          }
        }
        countE=countE+1
        count=count+1
        
      }
      
    }
    
  }
  
}


ymax=2
count=0
plot(-100,-100,ylim=c(0,ymax),xlim=c(xmin,xmax),ylab="Dispersion in array",xlab="",xaxt="n",las=2)
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
        label=paste(simID,"_p",p,"_X",X,"_D",D,"_C",C,"_U",U,sep="")
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
            #prom=mean(colMeans(novA))
            
            prom=vars/prom
            stand=sd()
            points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*15),prom,col=arr_color[l],pch=tipo[k],cex=tamany[o])
            #points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*15),prom+stand,col=1,pch=1,cex=0.3)
            #points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*15),prom-stand,col=1,pch=1,cex=0.3)
          }
        }
        countE=countE+1
        count=count+1
        
      }
      
    }
    
  }
  
}

ymax=1
count=0
plot(-100,-100,ylim=c(0,ymax),xlim=c(xmin,xmax),ylab="Hetz in array",xlab="",xaxt="n",las=2)
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
        label=paste(simID,"_p",p,"_X",X,"_D",D,"_C",C,"_U",U,sep="")
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
            
            hetz=mat.or.vec(10,longA)
            for(gg in 1:10){
              for(hh in 1:longA){
                numbers <- unlist(novA[((gg-1)*longA+hh),])
                suma=0
                for(jj in 1:length(table(numbers))){
                  freq=(table(numbers)[[jj]])/longA
                  suma=suma+freq*freq
                }
                hetz[gg,hh]=1-suma
                
              }
            #  lines(hetz[gg,],col=gg)
            }
            
            prom=(mean(hetz))
            stand=sd(hetz)
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


par(mar = c(5,5, 0.5, 0.5),xaxs="i")
ymax=20
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
        label=paste(simID,"_p",p,"_X",X,"_D",D,"_C",C,"_U",U,sep="")
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
axis(1, at=c(500,1500,2500,3500), labels=c("0.01","0.01","0.1","1"))
title(xlab="Alpha",sub = label)

#}



#dev.off()


