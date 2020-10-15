
#THIS FILE CREATS A PDF WITH THE SUMMARY RESULTS FROM A SET OF SIMULATIONS
#THEN IT GENERATES AN EXAMPLE (FIRST RUN) OF EACH SET OF SIMULATIONS

#THIS FILE INTENDS TO SHOW THE EVOLUTION OF SEVERAL SUMMARY STATISTICS
#This file intends to show the results from several PZIFE simulations of several runs each 
#For each simulation it will plot the evolution in time of the summary statistics (diversity, recActivity,selCoeff) 
# and will print the average histogram

#It shows the relevant statistics that characterize the evolutionary scenario under which the Red-Queen is developing
#It generates one pdf file 

setwd(paste("~/Documents/Projects/PZIFE/C_scripts_and_data/dataFromCluster/TrialRun_2018_09_11/",sep=""))

label="N1000_t200_s10h"
# PRINT PDF
#pdf(paste("masterPlot_andIndividualSims_",label,".pdf",sep=""), width=8, height= 9)


arr_color=c("#009E73", "#e79f00", "#0072B2", "#9ad0f3", "#D55E00", 
            "#CC79A7", "#F0E442","#000000")

tamany=c(1.2,1.5,1.8,2)

m <- rbind(c(1, 1,1), c(2,2,2),c(3,3,3),c(4,4,4),c(5,5,5),c(6,6,6),c(7,7,7))
layout(m,heights = c(1,2,2,2,2,2,3))
par(mar = c(0.5,5, 0.5, 0.5))
#,xaxs="i")

tipo=c(19,17,15,18,1)
arrayP=c(-1,0,1,2,3)
arrayE=c(6,4,2)
arrayD=c(4,3,2,1,0)


simID="N100_t1000_s1r"
paths="~/Documents/Projects/PZIFE/C_scripts_and_data/dataFromCluster/TrialRun_2018_09_24/s1r"
arrayP=c(3)
arrayE=c(5,1)
arrayD=c(4,0)
arrayU=c(6,4)

simID="prueba_pzife_1.98_N200_t40_s1c"
paths="~/Documents/Projects/PZIFE/C_scripts_and_data/dataFromCluster/TrialRun_2018_10_08/s1c"
arrayP=c(1)
arrayE=c(5,4)
arrayD=c(0)
arrayU=c(4,2)

setwd(paths)
pdf(paste("masterPlot_andIndividualSims_",simID,".pdf",sep=""), width=8, height= 9)

m <- rbind(c(1, 1,1), c(2,2,2),c(3,3,3),c(4,4,4),c(5,5,5),c(6,6,6),c(7,7,7))
layout(m,heights = c(1,2,2,2,2,2,3))
par(mar = c(0.5,5, 0.5, 0.5))

plot.new()
legend("top",c("C=0.004", "C=0.04", "C=0.4", "C=4", "U=0.00004", "U=0.0004", "U=0.004","U=0.04","X=0.000004","X=0.00004","X=0.0004","X=0.004"),ncol = 8, 
       col=c(1,1,1,1,arr_color[1],arr_color[2],arr_color[3],arr_color[4],1,1,1,1),pch=c(tipo[1],tipo[2],tipo[3],tipo[4],NA,NA,NA,NA,tipo[5],tipo[5],tipo[5],tipo[5]),
       lty=c(NA,NA,NA,NA,1,1,1,1,NA,NA,NA,NA),pt.cex=c(1,1,1,1,2,2,2,2,tamany[1],tamany[2],tamany[3],tamany[4]),
       lwd=c(NA,NA,NA,NA,3,3,3,3,NA,NA,NA,NA),bty = "n",x.intersp=0.05)
#legend("top",c("C=0.004", "C=0.04", "C=0.4" , "U=0.00004", "U=0.0004","U=0.004","X=0.00004","X=0.004","X=0.4"),ncol = 9, 
#col=c(1,1,1,arr_color[1],arr_color[2],arr_color[3],1,1,1),pch=c(tipo[1],tipo[2],tipo[3],NA,NA,NA,tipo[4],tipo[4],tipo[4]),
#lty=c(NA,NA,NA,1,1,1,NA,NA,NA),pt.cex=c(1,1,1,2,2,2,tamany[1],tamany[2],tamany[3]),
#lwd=c(NA,NA,NA,3,3,3,NA,NA,NA),bty = "n",x.intersp=0.05)


init=0
limX=98
Prop=5
tamanyo=.8
ymax=20
xmin=-10
xmax=1000

count=0
plot(-100,1000,ylim=c(.01,ymax),
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
              points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*15),mutRate,col=arr_color[l],pch=tipo[k],cex=tamany[o])
            }
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
ymax=10

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
              points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*15),promDiv,col=arr_color[l],pch=tipo[k],cex=tamany[o])
            }
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
              points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*15),promRec,col=arr_color[l],pch=tipo[k],cex=tamany[o])
            }
          }
        }
        countE=countE+1
        count=count+1
        
        
      }
      
    }
    
  }
  
}


ymax=10000
count=0
plot(1000,1000,ylim=c(0.01,ymax),xlim=c(xmin,xmax),ylab="Selection coefficient",xlab="",xaxt="n",las=2,log="y")
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
              points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*15),promSel,col=arr_color[l],pch=tipo[k],cex=tamany[o])
            }
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


intervalos=1000

par(mar = c(5,5, 0.5, 0.5),xaxs="i")
ymax=2
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
            longA=(profile$Generations-profile$BurnIn)/intervalos
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
            
            prom=vars/prom
            
            
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






#dev.off()
#This file intends to show the results from 1 PZIFE simulation
#It shows the relevant statistics that characterize the evolutionary scenario under which the Red-Queen is developing
#It generates one pdf file 


for(zz in 1:length(arrayP)){
  count=0
  # for(j in 1:length(arrayU)){
  for(yy in 1:length(arrayD)){
    for(xx in 1:length(arrayU)){ 
      countE=0
      for(ww in 1:length(arrayE)){
        
        p=arrayP[zz]
        X=arrayE[ww]
        D=arrayD[yy]
        C=D
        U=arrayU[xx]
        #U=C+2
        label=paste(simID,"_p",p,"_X",X,"_D",D,"_C",C,"_U",U,sep="")
        setwd(paste(paths,"/",label,sep=""))
        



m <- rbind(c(1, 1,1), c(2,2,2),c(3,3,3),c(4,4,4),c(5,5,5))
layout(m,heights = c(2,1.5,1.5,1.5,2.5))

par(mar = c(0.5,5, 0.5, 0.5))

#Read files
a=read.table(paste("alleleFreqs_",label,".dat",sep=""))
b=read.table(paste("motifActivity_",label,".dat",sep=""))
c=read.table(paste("alleleZnfArraySize_",label,".dat",sep=""))
d=read.table(paste("referenceOfActiveAlleles_",label,".dat",sep=""))

profile=read.table(paste("profile_",label,".dat",sep=""),header=TRUE)


uq_elem=c()
for(i in 1:ncol(d))
{
  uq_elem=c(unique(d[,i]), uq_elem)
  uq_elem=unique(uq_elem)
}
sortUq=sort(uq_elem)


colMax <- function(X) apply(X, 2, max)
selected=mat.or.vec(ncol(a),1)

# Create newA, newB and newC from reference allele list in d

newA=mat.or.vec(nrow(a),length(sortUq))
newB=mat.or.vec(nrow(a),length(sortUq))
newC=mat.or.vec(nrow(a),length(sortUq))
for(i in 2:length(sortUq)){
  for(k in 1:nrow(a)){
    #for(j in 1:ncol(a)){
    
    #
    j=1
    #repeat{
    while(d[k,j]!=0 && d[k,j]<=sortUq[i] && j<=(length(sortUq)-1)){ 
      if(d[k,j]==sortUq[i]){ 
        newA[k,i-1]=a[k,j]
        newB[k,i-1]=b[k,j]
        newC[k,i-1]=c[k,j]
        
      }
      j=j+1
      #if(d[k,j]==0){break}
      #if(d[k,j]>=sortUq[i]){
      #  break
      #}
    }
    #}
  }
}
newA[,1]

#PRINTS ALLELE FREQUENCIES
maxim = colMax(newA)
init=0
par(mar = c(0.5,5, 0.5, 0.5),xaxs="i")
plot(-100,-100,ylim=c(0,1),xlim=c(init,nrow(a)),ylab="Allele Frequencies",xaxt="n",xlab="")
count=1
for(i in 1:length(maxim)){
  if(maxim[i] > 0.1){
    selected[count]=i #guarda las trayectorias que pasan el threshold
    lines(newA[,i],col=count)
    count=count+1
  }
}

#PRINTS MOTIF ACTIVITY
par(mar = c(0.5,5, 0.5, 0.5))
plot(100,1000,ylim=c(0.0000001,1),xlim=c(init,nrow(b)),ylab="Motif Activity", xaxt="n",xlab="",log="y")
for(i in 1:(count-1)){
  for(j in 1:nrow(newB)){
    points(j,newB[j,selected[i]],col=i,cex=0.4)
    #points(newB[,i],col=i,cex=0.2)
  }
}


#PRDM9 DIVERSITY
divsty=read.table(paste("prdmDiversity_",label,".dat",sep=""))
prom=mean(as.numeric(divsty[1,(ncol(divsty)/2):ncol(divsty)]))
par(mar = c(0.5,5, 0.5, 0.5))
plot(-1000,-1000,ylim=c(0,16),xlim=c(init,ncol(divsty)),ylab="Diversity",xlab = "",xaxt="n")
equis=seq(1,ncol(divsty),1)
lines(equis,divsty[1,],col=1)
abline(h=prom,col=2)
counti=0
for(i in 1:ncol(divsty)){
  if(divsty[1,i]==1){
    counti=counti+1
  }
}
divsty[1,2]
text(10,1,paste("Div=1 counts: ",counti,sep=""))
text(30,1,paste("Av. div = ",prom,sep=""))


#PRINTS MEAN AA DIVERSITY AT PRDM9 BINDING SITES
f=read.table(paste("meanDiversityWithinZnf_",label,".dat",sep=""))

Res = profile$RelevantResidues*2+1;
aaDivInPrdm9Binding=mat.or.vec(1,nrow(f))
for(i in 1:nrow(f)){
  sumRelRes=f[i,2]+f[i,4]+f[i,6]
  aaDivInPrdm9Binding[i]=sumRelRes/sum(f[i,])
}
equiss=seq(1,nrow(f),1)
plot(equiss,aaDivInPrdm9Binding,xlim=c(0,nrow(f)),ylim=c(0,1),xaxt="n",xlab = "",ylab="AA Div. at Binding sites",type = "l")
prom=mean(as.numeric(aaDivInPrdm9Binding[1,(ncol(aaDivInPrdm9Binding)/2):ncol(aaDivInPrdm9Binding)]))
abline(h=prom,col="red")
text(30,0.05,paste("Av. aa div at binding sites = ",prom,sep=""))


#PLOTS SIZE OF ARRAY
par(mar = c(5,5, 0.5, 0.5))
suma=0
plot(-1000,-1000,ylim=c(4,25),xlim=c(init,nrow(c)),ylab="Allele Znf Array Size",xlab = "Generations after burn-in",sub = label)
for(i in 1:(count-1)){
  points(newC[,selected[i]],col=i,cex=0.2)
  #points(b[,i],col=i,cex=0.2)
}

arraySizeH=mat.or.vec(1,nrow(newC))
for(k in 1:nrow(newC)){
  suma=0
  for(i in 1:(count-1)){
    suma=suma+(newC[k,selected[i]])^2
  }
  arraySizeH[k]=1/suma
}
arraySizeHet=mean(arraySizeH)

Prop=nrow(a)/100
tamanyo=1

#text(10*Prop,2.5,label,cex=tamanyo)
text(10*Prop,24,paste("N = ",profile$PopulationSize,sep=""),cex=tamanyo)
text(10*Prop,22,paste("alpha = ",profile$Alpha,sep=""),cex=tamanyo)
text(10*Prop,20,bquote(paste(rho," = ",.(profile$ErosionRate),sep = "")),cex=tamanyo)
text(30*Prop,24,paste("DD = ",profile$BirthRate,sep=""),cex=tamanyo)
#text(10,.14*Prop,paste("bminus = ",profile$DeathRate,sep=""),cex=tamanyo)
text(30*Prop,22,paste("C = ",profile$GeneConversionRate,sep=""),cex=tamanyo)
text(30*Prop,20,paste("U = ",profile$PointMutRate,sep=""),cex=tamanyo)
#text(10,.11*Prop,paste("time = ",profile$Generations,sep=""),cex=tamanyo)
#text(10,.10*Prop,paste("runs = ",profile$Runs,sep=""),cex=tamanyo)


#PRDM9 DIVERSITY
g=read.table(paste("prdmDiversity_",label,".dat",sep=""))
prom=mean(as.numeric(g[1,(ncol(g)/2):ncol(g)]))
text(50*Prop,22,paste("Dvsty = ",prom,sep=""),cex=tamanyo)

#PRINT RECOMBINATION ACTIVITY AS LATRILLE ET AL 2017
d=read.table(paste("recombinationActivity_",label,".dat",sep=""))
prom=mean(as.numeric(d[1,(ncol(d)/2):ncol(d)]))
text(50*Prop,24,paste("Actvty = ",prom,sep=""),cex=tamanyo)

#PRINT selection coefficient as Latrille et al 2017
de=read.table(paste("selectionCoefficient_",label,".dat",sep=""))
prom=mean(as.numeric(de[1,(ncol(de)/2):ncol(de)]))
#text(15,.13*Prop,prom,cex=tamanyo)
selValue=4*profile$PopulationSize*prom
text(50*Prop,20,paste("4Ns = ",selValue,sep=""),cex=tamanyo)

gralStats=c()
mutRate=c()
epsilonRatio=c()
bStats=read.table(paste("generalStatistics_",label,".dat",sep=""))
for(k in 1:9){
  gralStats[k]=unlist(as.numeric(unlist(bStats[1,k])))
}
mutRate[i]=gralStats[9]/((profile$Generations-profile$BurnIn))/(2*profile$PopulationSize)*(4*profile$PopulationSize)
text(75*Prop,24,bquote(paste(mu," = ",.(mutRate[i]),sep="")),cex=tamanyo)
epsilonRatio[i]=profile$ErosionRate/mutRate[i]
text(75*Prop,22.5,bquote(paste(epsilon," = ",.(epsilonRatio[i]),sep="")),cex=tamanyo)

text(75*Prop,20,bquote(paste(kappa," = ",.(arraySizeHet),sep="")),cex=tamanyo)



}}}}

dev.off()


