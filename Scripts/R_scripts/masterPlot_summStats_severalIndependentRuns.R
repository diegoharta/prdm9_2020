
#THIS FILE CREATS THE MASTER PLOT
#IT INCLUDES REAL AND EFFECTIVE MUTATION RATES, SUMMARY STATS AND RELATIVE TO THE SIZE OF ARRAY AND MUTATION TYPE
#IT GETS THE INFO FROM SEVERAL INDEPENDENT RUNS LABELLED BY _s0, _s1, etc

#THIS FILE INTENDS TO SHOW THE EVOLUTION OF SEVERAL SUMMARY STATISTICS
#This file intends to show the results from several PZIFE simulations of several runs each 
#For each simulation it will plot the evolution in time of the summary statistics (diversity, recActivity,selCoeff) 
# and will print the average histogram

#It shows the relevant statistics that characterize the evolutionary scenario under which the Red-Queen is developing
#It generates one pdf file 

setwd(paste("~/Documents/Projects/PZIFE/C_scripts_and_data/dataFromCluster/TrialRun_2018_09_29/s5ff",sep=""))

label="N1000_t200_s5ff"
# PRINT PDF
#pdf(paste("masterPlot_",label,".pdf",sep=""), width=8, height= 9)


arr_color=c("#009E73", "#e79f00", "#0072B2", "#9ad0f3", "#D55E00", 
            "#CC79A7", "#F0E442","#000000")

tamany=c(1.2,1.5,1.8,2)
tamany=c(1,1,1,1,1)

m <- rbind(c(1,1,1,1),c(2,2,2,2), c(3,3,4,4),c(5,5,6,6),c(7,7,8,8),c(9,9,10,10))
m <- rbind(c(1,1,1,1),c(2,2,3,3), c(4,4,5,5),c(6,6,7,7),c(8,8,9,9),c(10,10,11,11))
layout(m,heights = c(1,2,2,2,2,3))

# m <- rbind(c(1,1,1,1),c(2,2,3,3), c(4,4,5,5),c(6,6,7,7),c(8,8,9,9))
# layout(m,heights = c(1,2,2,2,3))
par(mar = c(0.5,5, 0.5, 0.5))
#,xaxs="i")

tipo=c(22,23,24,25,1)
tipo2=c(15,16,17,18)

simID="N1000_t400_s10m"
paths="~/Documents/Projects/PZIFE/C_scripts_and_data/dataFromCluster/TrialRun_2018_09_14/s10m"
arrayP=c(1)
arrayE=c(6,5,4)
arrayD=c(3,2,1,0)
arrayU=c(5,4,3,2)

simID="N1000_t200_s1dd"
paths="~/Documents/Projects/PZIFE/C_scripts_and_data/dataFromCluster/TrialRun_2018_09_29/s1dd"
arrayP=c(1)
arrayE=c(6,5,4)
arrayD=c(2,1,0)
arrayU=c(6,4)

simID="N1000_t200_s5ff"
paths="~/Documents/Projects/PZIFE/C_scripts_and_data/dataFromCluster/TrialRun_2018_09_29/s5ff"
arrayP=c(1)
arrayE=c(6,5,4)
arrayD=c(2,1,0)
arrayU=c(6,4)

intervalo=1000
nsims=5
Propor=1.5
 
plot.new()
legend("top",c("C=0.04", "C=0.4", "C=4", "U=0.000004", "U=0.0004", "X=0.000004","X=0.00004","X=0.0004"),ncol = 8, 
       col=c(1,1,1,arr_color[1],arr_color[2],1,1,1),pch=c(tipo[1],tipo[2],tipo[3],NA,NA,tipo[5],tipo[5],tipo[5]),
       lty=c(NA,NA,NA,1,1,NA,NA,NA),pt.cex=c(1,1,1,2,2,tamany[1],tamany[2],tamany[3]),
       lwd=c(NA,NA,NA,3,3,NA,NA,NA),bty = "n",x.intersp=0.05)
#legend("top",c("C=0.004", "C=0.04", "C=0.4" , "U=0.00004", "U=0.0004","U=0.004","X=0.00004","X=0.004","X=0.4"),ncol = 9, 
#col=c(1,1,1,arr_color[1],arr_color[2],arr_color[3],1,1,1),pch=c(tipo[1],tipo[2],tipo[3],NA,NA,NA,tipo[4],tipo[4],tipo[4]),
#lty=c(NA,NA,NA,1,1,1,NA,NA,NA),pt.cex=c(1,1,1,2,2,2,tamany[1],tamany[2],tamany[3]),
#lwd=c(NA,NA,NA,3,3,3,NA,NA,NA),bty = "n",x.intersp=0.05)

valoresEff=c()
valoresReal=c()
valores=c()
valores2=c()
valores3=c()

init=0
limX=98
Prop=5
tamanyo=.8
ymax=0.01
xmin=-100
xmax=1000

count=0
plot(-100,10000,ylim=c(.00001,ymax),
     xlim=c(xmin,xmax),ylab="Real mutation rate",xlab="",xaxt="n",las=2,log="y")
for(i in 1:length(arrayP)){
  count=0
  for(k in 1:length(arrayD)){
    for(l in 1:length(arrayU)){ 
      countE=0
      for(o in 1:length(arrayE)){
        for(uu in 1:nsims){
        
        p=arrayP[i]
        X=arrayE[o]
        D=arrayD[k]
        C=D
        U=arrayU[l]
        uu=uu-1
        label=paste("prueba_pzife_1.97_",simID,"_p",p,"_X",X,"_D",D,"_C",C,"_U",U,"_s",uu,sep="")
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
            
            #Real Effective mutation rate
            
            gralStats=c()
            mutRate=c()
            mutEffRate=c()
            mutRealRate=c()
            epsilonRatio=c()
            bStats=read.table(paste("generalStatistics_",label,".dat",sep=""))
            profile=read.table(paste("profile_",label,".dat",sep=""),header = TRUE)
            mutRealRate=(bStats[m,13])/((profile$Generations-profile$BurnIn))/(2*profile$PopulationSize)*(4*profile$PopulationSize)
            valoresReal[uu]=mutRealRate
           
            
          }
        }
        }
        prom=mean(as.numeric(valoresReal))
        stand=sd(as.numeric(valoresReal))
        points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*15)*Propor,prom,col=arr_color[l],pch=tipo[k],cex=tamany[o])
        points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*15)*Propor,prom+stand,col=1,pch=1,cex=0.3)
        points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*15)*Propor,prom-stand,col=1,pch=1,cex=0.3)
        
        countE=countE+1
        count=count+1
        
      }
    }
  }
}

init=0
limX=98
Prop=5
tamanyo=.8
ymax=100
xmax=1000
count=0
plot(-100,10000,ylim=c(.05,ymax),
     xlim=c(xmin,xmax),ylab="Effective mutation rate",xlab="",xaxt="n",las=2,log="y")
for(i in 1:length(arrayP)){
  count=0
  for(k in 1:length(arrayD)){
    for(l in 1:length(arrayU)){ 
      countE=0
      for(o in 1:length(arrayE)){
        for(uu in 1:nsims){
          
          p=arrayP[i]
          X=arrayE[o]
          D=arrayD[k]
          C=D
          U=arrayU[l]
          uu=uu-1
          #U=C+2
          label=paste("prueba_pzife_1.97_",simID,"_p",p,"_X",X,"_D",D,"_C",C,"_U",U,"_s",uu,sep="")
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
              mutEffRate=c()
              mutRealRate=c()
              epsilonRatio=c()
              bStats=read.table(paste("generalStatistics_",label,".dat",sep=""))
              profile=read.table(paste("profile_",label,".dat",sep=""),header = TRUE)
              mutEffRate=bStats[m,9]/((profile$Generations-profile$BurnIn))/(2*profile$PopulationSize)*(4*profile$PopulationSize)
              valoresEff[uu]=mutEffRate
            
          }
          }
        }
        promEff=mean(as.numeric(valoresEff))
        standEff=sd(as.numeric(valoresEff))
        
        points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*15)*Propor,promEff,col=arr_color[l],pch=tipo[k],cex=tamany[o])
        points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*15)*Propor,promEff+standEff,col=1,pch=1,cex=0.3)
        points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*15)*Propor,promEff-standEff,col=1,pch=1,cex=0.3)
        countE=countE+1
        count=count+1
      }
    }
    
  }
  
}

init=0
limX=98
Prop=5
tamanyo=.8
ymax=8

count=0
plot(-100,-100,ylim=c(0,ymax),xlim=c(xmin,xmax),ylab="Prdm9 diversity",xlab="",xaxt="n",las=2)
for(i in 1:length(arrayP)){
  count=0
  # for(j in 1:length(arrayU)){
  for(k in 1:length(arrayD)){
    for(l in 1:length(arrayU)){ 
      countE=0
      for(o in 1:length(arrayE)){
        for(uu in 1:nsims){
          
          p=arrayP[i]
          X=arrayE[o]
          D=arrayD[k]
          C=D
          U=arrayU[l]
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
            #PRDM9 DIVERSITY
            divt=read.table(paste("prdmDiversity_",label,".dat",sep=""))
            
              promDiv=mean(as.numeric(divt[1,(ncol(divt)/4):ncol(divt)]))
              #points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*15),promDiv,col=arr_color[l],pch=tipo[k],cex=tamany[o])
              valores[uu]=promDiv
          }
        }
        }
            prom=mean(as.numeric(valores))
            stand=sd(as.numeric(valores))
            points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*15)*Propor,prom,col=arr_color[l],pch=tipo[k],cex=tamany[o])
            points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*15)*Propor,prom+stand,col=1,pch=1,cex=0.3)
            points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*15)*Propor,prom-stand,col=1,pch=1,cex=0.3)
        
        countE=countE+1
        count=count+1
        
      }
      
    }
    
  }
  
}


ymax=1
count=0
plot(-100,-100,ylim=c(0,ymax),xlim=c(xmin,xmax),ylab="Rec. Activity",xlab="",xaxt="n",las=2)
for(i in 1:length(arrayP)){
  count=0
  # for(j in 1:length(arrayU)){
  for(k in 1:length(arrayD)){
    for(l in 1:length(arrayU)){ 
      countE=0
      for(o in 1:length(arrayE)){
        for(uu in 1:nsims){
          
          p=arrayP[i]
          X=arrayE[o]
          D=arrayD[k]
          C=D
          U=arrayU[l]
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
            
            #PRINT RECOMBINATION ACTIVITY AS LATRILLE ET AL 2017
            rec=read.table(paste("recombinationActivity_",label,".dat",sep=""))
      
              promRec=mean(as.numeric(rec[1,(ncol(rec)/4):ncol(rec)]))
              #points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*15),promRec,col=arr_color[l],pch=tipo[k],cex=tamany[o])
              valores[uu]=promRec
            
           
          }
        }
        }
        prom=mean(as.numeric(valores))
        stand=sd(as.numeric(valores))
        points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*15)*Propor,prom,col=arr_color[l],pch=tipo[k],cex=tamany[o])
        points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*15)*Propor,prom+stand,col=1,pch=1,cex=0.3)
        points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*15)*Propor,prom-stand,col=1,pch=1,cex=0.3)
        countE=countE+1
        count=count+1
        
        
      }
      
    }
    
  }
  
}


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
        for(uu in 1:nsims){
          
          p=arrayP[i]
          X=arrayE[o]
          D=arrayD[k]
          C=D
          U=arrayU[l]
          uu=uu-1
          #U=C+2
          label=paste("prueba_pzife_1.97_",simID,"_p",p,"_X",X,"_D",D,"_C",C,"_U",U,"_s",uu,sep="")
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

        
              promSel=mean(as.numeric(sel[1,(ncol(sel)/4):ncol(sel)]))*4*profile$PopulationSize
              # points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*15),promSel,col=arr_color[l],pch=tipo[k],cex=tamany[o])
              valores[uu]=promSel
            
           
          }
        }
        }
        prom=mean(as.numeric(valores))
        stand=sd(as.numeric(valores))
        points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*15)*Propor,prom,col=arr_color[l],pch=tipo[k],cex=tamany[o])
        points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*15)*Propor,prom+stand,col=1,pch=1,cex=0.3)
        points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*15)*Propor,prom-stand,col=1,pch=1,cex=0.3)
        countE=countE+1
        count=count+1
      }
    }
  }
}


ymax=1
count=0
plot(-100,-100,ylim=c(0,ymax),xlim=c(xmin,xmax),ylab="AA Div. at Binding",xlab="",xaxt="n",las=2)
for(i in 1:length(arrayP)){
  count=0
  # for(j in 1:length(arrayU)){
  for(k in 1:length(arrayD)){
    for(l in 1:length(arrayU)){ 
      countE=0
      for(o in 1:length(arrayE)){
        for(uu in 1:nsims){
          
          p=arrayP[i]
          X=arrayE[o]
          D=arrayD[k]
          C=D
          U=arrayU[l]
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
              promAADiv=mean(as.numeric(aaDivInPrdm9Binding[(ncol(aaDivInPrdm9Binding)/4):ncol(aaDivInPrdm9Binding)]))
              valores[uu]=promAADiv
            }
              
            }
          }
        }
        prom=mean(as.numeric(valores))
        stand=sd(as.numeric(valores))
        points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*15)*Propor,prom,col=arr_color[l],pch=tipo[k],cex=tamany[o])
        points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*15)*Propor,prom+stand,col=1,pch=1,cex=0.3)
        points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*15)*Propor,prom-stand,col=1,pch=1,cex=0.3)
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
        for(uu in 1:nsims){
          
          p=arrayP[i]
          X=arrayE[o]
          D=arrayD[k]
          C=D
          U=arrayU[l]
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
            
            a=read.table(paste("histogramOfSizeOfZnfArray_",label,".dat",sep=""),header=FALSE)
            novA=a[,3:length(a)]
           
            avs=mean(as.numeric(unlist(novA)))
            vars=sd(as.numeric(unlist(novA)))^2
            promDisp=vars/avs
            valores[uu]=promDisp
        
        }
        }
      }
      prom=mean(as.numeric(valores))
      stand=sd(as.numeric(valores))
      points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*15)*Propor,prom,col=arr_color[l],pch=tipo[k],cex=tamany[o])
      points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*15)*Propor,prom+stand,col=1,pch=1,cex=0.3)
      points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*15)*Propor,prom-stand,col=1,pch=1,cex=0.3)
      countE=countE+1
      count=count+1
        
      }
      
    }
    
  }
  
}



ymax=1
count=0
plot(-100,-100,ylim=c(-0.5,ymax),xlim=c(xmin,xmax),ylab="Hetz in array",xlab="",xaxt="n",las=2)
for(i in 1:length(arrayP)){
  count=0
  for(k in 1:length(arrayD)){
    for(l in 1:length(arrayU)){
      countE=0
      for(o in 1:length(arrayE)){
        for(uu in 1:nsims){

          p=arrayP[i]
          X=arrayE[o]
          D=arrayD[k]
          C=D
          U=arrayU[l]
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

               a=read.table(paste("histogramOfSizeOfZnfArray_",label,".dat",sep=""),header=FALSE)
               novA=a[,3:length(a)]
               longA=(profile$Generations-profile$BurnIn)/intervalo
               novNovA=mat.or.vec(nrow(sel),longA)

               hetz=mat.or.vec(profile$Runs,longA)
            
               for(gg in 1:profile$Runs){
                 for(hh in 1:longA){
                   numbers <- unlist(novA[((gg-1)*longA+hh),])
                   suma=0
                   for(jj in 1:length(table(numbers))){
                     freq=(table(numbers)[[jj]])/longA
                     suma=suma+freq*freq
                   }
                   hetz[gg,hh]=1-suma
                 }
               }
               
               promHetz=(mean(hetz))
               valores[uu]=promHetz
            }
          }
        }
        prom=mean(as.numeric(valores))
        stand=sd(as.numeric(valores))
        points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*15)*Propor,prom,col=arr_color[l],pch=tipo[k],cex=tamany[o])
        points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*15)*Propor,prom+stand,col=1,pch=1,cex=0.3)
        points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*15)*Propor,prom-stand,col=1,pch=1,cex=0.3)
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
        for(uu in 1:nsims){

          p=arrayP[i]
          X=arrayE[o]
          D=arrayD[k]
          C=D
          U=arrayU[l]
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

            a=read.table(paste("histogramOfSizeOfZnfArray_",label,".dat",sep=""),header=FALSE)
            novA=a[,3:length(a)]
          
            promSize=mean(unlist(as.numeric(unlist(novA))))
            valores[uu]=promSize
          }
         }
        }
        prom=mean(as.numeric(valores))
        stand=sd(as.numeric(valores))
        points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*15)*Propor,prom,col=arr_color[l],pch=tipo[k],cex=tamany[o])
        points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*15)*Propor,prom+stand,col=1,pch=1,cex=0.3)
        points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*15)*Propor,prom-stand,col=1,pch=1,cex=0.3)
        countE=countE+1
        count=count+1

      }

    }

  }
}



par(mar = c(5,5, 0.5, 0.5),xaxs="i")
ymax=1
count=0
plot(-100,-100,ylim=c(0,ymax),xlim=c(xmin,xmax),ylab="Mut. proportions",xlab="",xaxt="n",las=2)
for(ii in 1:length(arrayP)){
  count=0
  # for(j in 1:length(arrayU)){
  for(kk in 1:length(arrayD)){
    for(ll in 1:length(arrayU)){ 
      countE=0
      for(oo in 1:length(arrayE)){
        for(uu in 1:nsims){
          
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
           
            promGeneConv=bStats[12]/bStats[13]
            promZnf=bStats[10]/bStats[13]
            promPoint=bStats[11]/bStats[13]
            
            valores[uu]=promGeneConv
            valores2[uu]=promZnf
            valores3[uu]=promPoint
        
          }
        }
        }
        prom=mean(as.numeric(valores))
        stand=sd(as.numeric(valores))
        prom2=mean(as.numeric(valores2))
        stand2=sd(as.numeric(valores2))
        prom3=mean(as.numeric(valores3))
        stand3=sd(as.numeric(valores3))
        
        points(((ii-1)*1000+((kk-1)*250)+((ll-1)*60)+(oo-1)*15)*Propor,prom,col=arr_color[ll],pch=tipo[kk],cex=tamany[oo])
        points(((ii-1)*1000+((kk-1)*250)+((ll-1)*60)+(oo-1)*15)*Propor,prom+stand,col=1,pch=1,cex=0.3)
        points(((ii-1)*1000+((kk-1)*250)+((ll-1)*60)+(oo-1)*15)*Propor,prom-stand,col=1,pch=1,cex=0.3)
        
        points(((ii-1)*1000+((kk-1)*250)+((ll-1)*60)+(oo-1)*15)*Propor,prom2,col=arr_color[ll],pch=tipo2[kk],cex=tamany[oo])
        points(((ii-1)*1000+((kk-1)*250)+((ll-1)*60)+(oo-1)*15)*Propor,prom2+stand2,col=1,pch=1,cex=0.3)
        points(((ii-1)*1000+((kk-1)*250)+((ll-1)*60)+(oo-1)*15)*Propor,prom2-stand2,col=1,pch=1,cex=0.3)
        
        points(((ii-1)*1000+((kk-1)*250)+((ll-1)*60)+(oo-1)*15)*Propor,prom3,col=arr_color[ll],pch=tipo3[kk],cex=tamany[oo])
        points(((ii-1)*1000+((kk-1)*250)+((ll-1)*60)+(oo-1)*15)*Propor,prom3+stand3,col=1,pch=1,cex=0.3)
        points(((ii-1)*1000+((kk-1)*250)+((ll-1)*60)+(oo-1)*15)*Propor,prom3-stand3,col=1,pch=1,cex=0.3)
        
        countE=countE+1
        count=count+1
           
      }
      
    }
    
  }
}




axis(1, at=c(500,1500,2500,3500), labels=c("0.1","0.01","0.1","1"))
title(xlab="Alpha",sub = label)

#}



#dev.off()


