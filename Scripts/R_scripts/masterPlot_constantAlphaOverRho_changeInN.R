
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

tamany=c(1.2,1.5,1.8,2, 2.1,2.4)
tamany=c(1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5)

m <- rbind(c(1,1,2,2), c(3,3,4,4),c(5,5,6,6),c(7,7,8,8))
#,c(9,9,9))
layout(m,heights = c(2,2,2,3))
par(mar = c(0.5,5, 0.5, 0.5))
#,xaxs="i")

tipo=c(22,23,24,25,1)
arrayP=c(-1,0,1,2,3)
arrayE=c(6,4,2)
arrayD=c(4,3,2,1,0)



simID="N1000_t400_s10m"
paths="~/Documents/Projects/PZIFE/C_scripts_and_data/dataFromCluster/TrialRun_2018_09_14/s10m"
arrayP=c(2)
arrayE=c(6,5,4)
arrayD=c(2,1,0)
arrayU=c(5,4,3,2)

carpeta="s1r"
label=""
paths="~/Documents/Projects/PZIFE/C_scripts_and_data/dataFromCluster/TrialRun_2018_10_08/s1r"
setwd(paths)
simID=paste("prueba_pzife_1.98",label,sep="")
alpha=0.01
#arrayP=c(4, 3, 2.52288, 2.22185,2, 1.52288, 1.22185, 1, 0, -1,-2)
#arrayE=c(4, 3, 2.52288, 2.22185,2, 1.52288, 1.22185, 1, 0, -1,-2)
arrayN=c(100, 200, 400, 800,1600, 3200,4000,8000,10000)
arrayt=c(100, 100,100,100,100, 100,100,100,100)
arrayP=c(1.39794, 1.69897, 2, 2.30103,2.60206,2.90309,3, 3.30103, 3.39794)
arrayE=c(1.39794, 1.69897, 2, 2.30103,2.60206,2.90309,3, 3.30103, 3.39794)
arrayD=c(0)
arrayU=c(2)

arrayYmin=c(0.1,.4,2,-5,.2,-1000,.05,.4)
arrayYmax=c(1,21,20,1,1000,100,.4,.85)

arrayYmin=c(0.1,10,0,0,.3,-100,0,.4)
arrayYmax=c(1,18,12,.8,50,1,1,1)
# 
# arrayP=c(4, 3, 2, 1, 0)
# arrayE=c(5, 4, 3, 2, 1)
# arrayD=c(1)
# arrayU=c(3)
# 
# arrayYmin=c(0.001,.004,0,.45,.1,-1000,.05,.4)
# arrayYmax=c(.5,2,5,.6,10000,1,.3,1)


inter=60
labelsAxis=c()
seqequis=c(0,1*inter,2*inter,3*inter,4*inter,5*inter,6*inter,7*inter,8*inter)
#seqequis=c(0,1*inter,2*inter,3*inter,4*inter,5*inter)

#plot.new()
#legend("top",c("C=0.004", "C=0.04", "C=0.4", "C=4", "U=0.0004", "U=0.004", "U=0.04","U=0.4","X=0.00004","X=0.0004","X=0.004","X=0.04"),ncol = 8, 
#      col=c(1,1,1,1,arr_color[1],arr_color[2],arr_color[3],arr_color[4],1,1,1,1),pch=c(tipo[1],tipo[2],tipo[3],tipo[4],NA,NA,NA,NA,tipo[5],tipo[5],tipo[5],tipo[5]),
#     lty=c(NA,NA,NA,NA,1,1,1,1,NA,NA,NA,NA),pt.cex=c(1,1,1,1,2,2,2,2,tamany[1],tamany[2],tamany[3],tamany[4]),
#    lwd=c(NA,NA,NA,NA,3,3,3,3,NA,NA,NA,NA),bty = "n",x.intersp=0.05)
#legend("top",c("C=0.004", "C=0.04", "C=0.4" , "U=0.00004", "U=0.0004","U=0.004","X=0.00004","X=0.004","X=0.4"),ncol = 9, 
#col=c(1,1,1,arr_color[1],arr_color[2],arr_color[3],1,1,1),pch=c(tipo[1],tipo[2],tipo[3],NA,NA,NA,tipo[4],tipo[4],tipo[4]),
#lty=c(NA,NA,NA,1,1,1,NA,NA,NA),pt.cex=c(1,1,1,2,2,2,tamany[1],tamany[2],tamany[3]),
#lwd=c(NA,NA,NA,3,3,3,NA,NA,NA),bty = "n",x.intersp=0.05)

valores=mat.or.vec(1,1)
realEffMuts=c()
effMuts=c()

init=0
limX=98
Prop=5
tamanyo=.8
ymax=1
ymin=.1
xmin=-50
xmax=520
o=1
countPlot=1
ymin=arrayYmin[countPlot]
ymax=arrayYmax[countPlot]

plot(-100,1000,ylim=c(ymin,ymax),
     xlim=c(xmin,xmax),ylab="Real Eff mutRate",xlab="",xaxt="n",las=2,log="y")
for(o in 1:length(arrayP)){
  count=0
  # for(j in 1:length(arrayU)){
  for(k in 1:length(arrayD)){
    for(l in 1:length(arrayU)){ 
      countE=0
      # for(o in 1:length(arrayE)){
      i=1
      p=arrayP[o]
      X=arrayE[o]
      t=arrayt[o]
      N=arrayN[o]
      D=arrayD[k]
      C=D
      U=arrayU[l]
      #U=C+2
      label=paste(simID,"_N",N,"_t",t,"_",carpeta,"_p",p,"_X",X,"_D",D,"_C",C,"_U",U,sep="")
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
            mutRate=bStats[m,13]/((profile$Generations-profile$BurnIn))/(2*profile$PopulationSize)*(4*profile$PopulationSize)
            
            #text(75*Prop,24,bquote(paste(mu," = ",.(mutRate[ii]),sep="")),cex=tamanyo)
            epsilonRatio=profile$ErosionRate/mutRate
            #text(75*Prop,22.5,bquote(paste(epsilon," = ",.(epsilonRatio[ii]),sep="")),cex=tamanyo)
            #points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),mutRate,col=arr_color[l],pch=tipo[k],cex=tamany[o])
            valores[m]=mutRate
            
          }
          realEffMuts[o]=mutRate
          prom=mean(as.numeric(valores))
          stand=sd(as.numeric(valores))
          points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),prom,col=arr_color[l+1],pch=tipo[k+1],cex=tamany[o])
          points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),prom+stand,col=1,pch=1,cex=0.3)
          points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),prom-stand,col=1,pch=1,cex=0.3)
          
          # for(m in 1:nrow(bStats)){
          #   
          #   # for(kk in 1:9){
          #   #  gralStats[kk]=unlist(as.numeric(bStats[kk]))
          #   #}
          #   #mutRate[m]=gralStats[9]/((profile$Generations-profile$BurnIn))/(2*profile$PopulationSize)*(4*profile$PopulationSize)
          #   mutRate=bStats[m,9]/((profile$Generations-profile$BurnIn))/(2*profile$PopulationSize)*(4*profile$PopulationSize)
          #   
          #   #text(75*Prop,24,bquote(paste(mu," = ",.(mutRate[ii]),sep="")),cex=tamanyo)
          #   epsilonRatio=profile$ErosionRate/mutRate
          #   #text(75*Prop,22.5,bquote(paste(epsilon," = ",.(epsilonRatio[ii]),sep="")),cex=tamanyo)
          #   #points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),mutRate,col=arr_color[l],pch=tipo[k],cex=tamany[o])
          #   valores[m]=mutRate
          # }
          # prom=mean(as.numeric(valores))
          # stand=sd(as.numeric(valores))
          # points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),prom,col=arr_color[l],pch=tipo[k+1],cex=tamany[o])
          # points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),prom+stand,col=1,pch=1,cex=0.3)
          # points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),prom-stand,col=1,pch=1,cex=0.3)
          
        }
      }
      countE=countE+1
      count=count+1
      
      #}
      
    }
    
  }
  
}
#legend("topright",c("C=0.04", "C=0.4", "C=4" , "U=0.0004", "U=0.004", "U=0.04"),ncol = 1, 
#      col=c(1,1,1,arr_color[1],arr_color[2],arr_color[3]),pch=c(tipo[1],tipo[2],tipo[3],NA,NA,NA),
#     lty=c(NA,NA,NA,1,1,1))

abline(h=0.4, col=2)

init=0
limX=98
Prop=5
tamanyo=.8
countPlot=countPlot+1
ymin=arrayYmin[countPlot]
ymax=arrayYmax[countPlot]

count=0
plot(-100,1000,ylim=c(ymin,ymax),
     xlim=c(xmin,xmax),ylab="Eff mutRate",xlab="",xaxt="n",las=2)
for(o in 1:length(arrayP)){
  count=0
  # for(j in 1:length(arrayU)){
  for(k in 1:length(arrayD)){
    for(l in 1:length(arrayU)){ 
      countE=0
      #for(o in 1:length(arrayE)){
      
      p=arrayP[o]
       X=arrayE[o]       
       t=arrayt[o]      
       N=arrayN[o]
      D=arrayD[k]
      C=D
      U=arrayU[l]
      #U=C+2
      label=paste(simID,"_N",N,"_t",t,"_",carpeta,"_p",p,"_X",X,"_D",D,"_C",C,"_U",U,sep="")
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
            #  mutRate=(bStats[m,10]+bStats[m,12])/bStats[m,13]
            
            mutRate=bStats[m,7]/((profile$Generations-profile$BurnIn))/(2*profile$PopulationSize)*(4*profile$PopulationSize)
            
            #text(75*Prop,24,bquote(paste(mu," = ",.(mutRate[ii]),sep="")),cex=tamanyo)
            epsilonRatio=profile$ErosionRate/mutRate
            #text(75*Prop,22.5,bquote(paste(epsilon," = ",.(epsilonRatio[ii]),sep="")),cex=tamanyo)
            #points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),mutRate,col=arr_color[l],pch=tipo[k],cex=tamany[o])
            valores[m]=mutRate
          }
          
          effMuts[o]=mutRate
          prom=mean(as.numeric(valores))
          stand=sd(as.numeric(valores))
          points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),prom,col=arr_color[l+2],pch=tipo[k+2],cex=tamany[o])
          points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),prom+stand,col=1,pch=1,cex=0.3)
          points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),prom-stand,col=1,pch=1,cex=0.3)
          
        }
      }
      countE=countE+1
      count=count+1
      
      # }
      
    }
    
  }
  
}
abline(h=100,col=2)
#legend("topright",c("C=0.04", "C=0.4", "C=4" , "U=0.0004", "U=0.004", "U=0.04"),ncol = 1, 
#      col=c(1,1,1,arr_color[1],arr_color[2],arr_color[3]),pch=c(tipo[1],tipo[2],tipo[3],NA,NA,NA),
#     lty=c(NA,NA,NA,1,1,1))


init=0
limX=98
Prop=5
tamanyo=.8
ymax=18
ymin=2


countPlot=countPlot+1
ymin=arrayYmin[countPlot]
ymax=arrayYmax[countPlot]

count=0
plot(-100,-100,ylim=c(ymin,ymax),xlim=c(xmin,xmax),ylab="Prdm9 div",xlab="",xaxt="n",las=2)
for(o in 1:length(arrayP)){
  count=0
  # for(j in 1:length(arrayU)){
  for(k in 1:length(arrayD)){
    for(l in 1:length(arrayU)){ 
      countE=0
      #for(o in 1:length(arrayE)){
      
      p=arrayP[o]
       X=arrayE[o]     
       t=arrayt[o]     
       N=arrayN[o]
      D=arrayD[k]
      C=D
      U=arrayU[l]
      #U=C+2
      label=paste(simID,"_N",N,"_t",t,"_",carpeta,"_p",p,"_X",X,"_D",D,"_C",C,"_U",U,sep="")
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
          profile=read.table(paste("profile_",label,".dat",sep=""),header = TRUE)
          
          
          for(m in 1:nrow(divt)){
            promDiv=mean(as.numeric(divt[m,(ncol(divt)/4):ncol(divt)]))
            #points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),promDiv,col=arr_color[l],pch=tipo[k],cex=tamany[o])
            valores[m]=promDiv
          }
          prom=mean(as.numeric(valores))
          stand=sd(as.numeric(valores))
          points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),prom,col=arr_color[l],pch=tipo[k],cex=tamany[o])
          points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),prom+stand,col=1,pch=1,cex=0.3)
          points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),prom-stand,col=1,pch=1,cex=0.3)
          
          propOverMutRate=realEffMuts[o]*6
          points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),propOverMutRate,col=arr_color[l+1],pch=tipo[k+1],cex=tamany[o])
          
          propOverMutRate=effMuts[o]*6
          points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),propOverMutRate,col=arr_color[l+2],pch=tipo[k+2],cex=tamany[o])
          
        }
      }
      countE=countE+1
      count=count+1
      
      # }
      
    }
    
  }
  
}
abline(h=10,col=2)



ymax=0.6
count=0.4

countPlot=countPlot+1
ymin=arrayYmin[countPlot]
ymax=arrayYmax[countPlot]

plot(-100,-100,ylim=c(ymin,ymax),xlim=c(xmin,xmax),ylab="Rec Act",xlab="",xaxt="n",las=2)
for(o in 1:length(arrayP)){
  count=0
  # for(j in 1:length(arrayU)){
  for(k in 1:length(arrayD)){
    for(l in 1:length(arrayU)){ 
      countE=0
      #for(o in 1:length(arrayE)){
      
      p=arrayP[o]
       X=arrayE[o]    
       t=arrayt[o]      
       N=arrayN[o]
      D=arrayD[k]
      C=D
      U=arrayU[l]
      #U=C+2
      label=paste(simID,"_N",N,"_t",t,"_",carpeta,"_p",p,"_X",X,"_D",D,"_C",C,"_U",U,sep="")
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
          profile=read.table(paste("profile_",label,".dat",sep=""),header = TRUE)
          
          for(m in 1:nrow(rec)){
            promRec=mean(as.numeric(rec[m,(ncol(rec)/4):ncol(rec)]))
            #points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),promRec,col=arr_color[l],pch=tipo[k],cex=tamany[o])
            valores[m]=promRec
          }
          prom=mean(as.numeric(valores))
          stand=sd(as.numeric(valores))
          points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),prom,col=arr_color[l],pch=tipo[k],cex=tamany[o])
          points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),prom+stand,col=1,pch=1,cex=0.3)
          points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),prom-stand,col=1,pch=1,cex=0.3)
          
          
          theorExpR=1-sqrt(profile$ErosionRate/profile$Alpha/realEffMuts[o])
          points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),theorExpR,col=arr_color[l+1],pch=tipo[k+1],cex=tamany[o])
          theorExpR2=1-sqrt(profile$ErosionRate/profile$Alpha/effMuts[o])
          points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),theorExpR2,col=arr_color[l+2],pch=tipo[k+2],cex=tamany[o])
        }
      }
      countE=countE+1
      count=count+1
      
      
      #}
      
    }
    
  }
  
}
abline(h=.6,col=2)
abline(h=.3,col=2)


ymax=200
count=0

countPlot=countPlot+1
ymin=arrayYmin[countPlot]
ymax=arrayYmax[countPlot]

plot(-100,1,ylim=c(ymin,ymax),xlim=c(xmin,xmax),ylab="Sel coeff",xlab="",xaxt="n",las=2,log="y")
for(o in 1:length(arrayP)){
  count=0
  # for(j in 1:length(arrayU)){
  for(k in 1:length(arrayD)){
    for(l in 1:length(arrayU)){
      countE=0
      #for(o in 1:length(arrayE)){
      
      p=arrayP[o]
       X=arrayE[o]     
       t=arrayt[o]      
       N=arrayN[o]
      D=arrayD[k]
      C=D
      U=arrayU[l]
      #U=C+2
      label=paste(simID,"_N",N,"_t",t,"_",carpeta,"_p",p,"_X",X,"_D",D,"_C",C,"_U",U,sep="")
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
          profile=read.table(paste("profile_",label,".dat",sep=""),header = TRUE)
          
          for(m in 1:nrow(sel)){
            promSel=mean(as.numeric(sel[m,(ncol(sel)/4):ncol(sel)]))*4*profile$PopulationSize
            # points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),promSel,col=arr_color[l],pch=tipo[k],cex=tamany[o])
            valores[m]=promSel
          }
          prom=mean(as.numeric(valores))
          stand=sd(as.numeric(valores))
          points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),prom,col=arr_color[l],pch=tipo[k],cex=tamany[o])
          points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),prom+stand,col=1,pch=1,cex=0.3)
          points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),prom-stand,col=1,pch=1,cex=0.3)
          
          
          propOverMutRate=.5*sqrt(profile$Alpha*profile$ErosionRate/realEffMuts[o])*4*profile$PopulationSize
          points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),propOverMutRate,col=arr_color[l+1],pch=tipo[k+1],cex=tamany[o])
          
          propOverMutRate=.5*sqrt(profile$Alpha*profile$ErosionRate/effMuts[o])*4*profile$PopulationSize
          points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),propOverMutRate,col=arr_color[l+2],pch=tipo[k+2],cex=tamany[o])
        }
      }
      countE=countE+1
      count=count+1
      #}
    }
  }
}
abline(h=26,col=2)


countPlot=countPlot+1
ymin=arrayYmin[countPlot]
ymax=arrayYmax[countPlot]

plot(-100,1,ylim=c(ymin,ymax),xlim=c(xmin,xmax),ylab="RealSelCoeff",xlab="",xaxt="n",las=2)
for(o in 1:length(arrayP)){
  count=0
  # for(j in 1:length(arrayU)){
  for(k in 1:length(arrayD)){
    for(l in 1:length(arrayU)){
      countE=0
      #       for(o in 1:length(arrayE)){
      #
      p=arrayP[o]
       X=arrayE[o]    
       t=arrayt[o]      
       N=arrayN[o]
      D=arrayD[k]
      #         C=D
      U=arrayU[l]
      #         #U=C+2
      label=paste(simID,"_N",N,"_t",t,"_",carpeta,"_p",p,"_X",X,"_D",D,"_C",C,"_U",U,sep="")
      setwd(paste(paths,"/",label,sep=""))
      er=("std_error.dat")
      val=file.info(er)$size
      if(is.na(val) == TRUE){
        #
        fileName <- "std_output.txt"
        conn <- file(fileName,open="r")
        linn <-readLines(conn)
        eco=(linn[length(linn)])
        close(conn)
        substr(eco, 1,5)
        if(substr(eco,1,5)=="alpha"){
          #    if(count == 1 && i==1) {text(1.5,xmax-1,label,cex=1)}
          
          #PRINT selection coefficient as Latrille et al 2017
          sel=read.table(paste("selectionCoefficientWithYear_",label,".dat",sep=""),skip=1,header=TRUE)
          profile=read.table(paste("profile_",label,".dat",sep=""),header = TRUE)
          
          
          promSelReal=mean(as.numeric(sel[(nrow(sel)/4):nrow(sel),3]))*4*profile$PopulationSize
          
          prom=mean(as.numeric(promSelReal))
          stand=sd(as.numeric(promSelReal))
          points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),prom,col=arr_color[l],pch=tipo[k],cex=tamany[o])
          points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),prom+stand,col=1,pch=1,cex=0.3)
          points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),prom-stand,col=1,pch=1,cex=0.3)
          
          propOverMutRate=.5*sqrt(profile$Alpha*profile$ErosionRate/realEffMuts[o])
          points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),propOverMutRate,col=arr_color[l+1],pch=tipo[k+1],cex=tamany[o])
          
          propOverMutRate=.5*sqrt(profile$Alpha*profile$ErosionRate/effMuts[o])
          points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),propOverMutRate,col=arr_color[l+2],pch=tipo[k+2],cex=tamany[o])
          #
          #
        }
      }
      countE=countE+1
      count=count+1
    }
    #     }
  }
}
abline(h=26,col=2)


countPlot=countPlot+1
ymin=arrayYmin[countPlot]
ymax=arrayYmax[countPlot]
par(mar = c(5,5, 0.5, 0.5),xaxs="i")
plot(-100,1,ylim=c(ymin,ymax),xlim=c(xmin,xmax),ylab="Div Rel Znf",xlab="",xaxt="n",las=2)
for(o in 1:length(arrayP)){
  count=0
  # for(j in 1:length(arrayU)){
  for(k in 1:length(arrayD)){
    for(l in 1:length(arrayU)){
      countE=0
      # for(o in 1:length(arrayE)){
      
      p=arrayP[o]
       X=arrayE[o]  
       t=arrayt[o]       
       N=arrayN[o]
      D=arrayD[k]
      C=D
      U=arrayU[l]
      #U=C+2
      label=paste(simID,"_N",N,"_t",t,"_",carpeta,"_p",p,"_X",X,"_D",D,"_C",C,"_U",U,sep="")
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
          
          divRelZnf=read.table(paste("avDivergenceRelZnf_",label,".dat",sep=""))
          
          for(m in 1:nrow(divRelZnf)){
            promDivRel=mean(as.numeric(unlist(divRelZnf)))
            # points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),promSel,col=arr_color[l],pch=tipo[k],cex=tamany[o])
            valores[m]=promDivRel
          }
          prom=mean(as.numeric(valores))
          stand=sd(as.numeric(valores))
          points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),prom,col=arr_color[l],pch=tipo[k],cex=tamany[o])
          points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),prom+stand,col=1,pch=1,cex=0.3)
          points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),prom-stand,col=1,pch=1,cex=0.3)
          
        
          divRelZnf=read.table(paste("avRelevantDivergenceRelZnf_",label,".dat",sep=""))
          
          for(m in 1:nrow(divRelZnf)){
            promDivRel=mean(as.numeric(unlist(divRelZnf)))
            # points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),promSel,col=arr_color[l],pch=tipo[k],cex=tamany[o])
            valores[m]=promDivRel
          }
          prom=mean(as.numeric(valores))
          stand=sd(as.numeric(valores))
          points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),prom,col=arr_color[l+1],pch=tipo[k+1],cex=tamany[o])
          points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),prom+stand,col=1,pch=1,cex=0.3)
          points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),prom-stand,col=1,pch=1,cex=0.3)
          
          
          
        }
      }
      countE=countE+1
      count=count+1
      #}
    }
  }
}
abline(h=26,col=2)

axis(1, at=seqequis, labels=round(10^-arrayP,digits=4))
title(xlab="Alpha",sub = label)




# 
# ymax=2000
# count=0
# plot(-100,1,ylim=c(1,ymax),xlim=c(xmin,xmax),ylab="SelecCoeffOne",xlab="",xaxt="n",las=2,log="y")
# for(i in 1:length(arrayP)){
#   count=0
#   # for(j in 1:length(arrayU)){
#   for(k in 1:length(arrayD)){
#     for(l in 1:length(arrayU)){ 
#       countE=0
#       for(o in 1:length(arrayE)){
#         
#         p=arrayP[i]
#          X=arrayE[o]       t=arrayt[o]       N=arrayN[o]
#         D=arrayD[k]
#         C=D
#         U=arrayU[l]
#         #U=C+2
#         label=paste(simID,"_N",N,"_t",t,"_",carpeta,"_p",p,"_X",X,"_D",D,"_C",C,"_U",U,sep="")
#         setwd(paste(paths,"/",label,sep=""))
#         er=("std_error.dat")
#         val=file.info(er)$size
#         if(is.na(val) == TRUE){
#           
#           fileName <- "std_output.txt"
#           conn <- file(fileName,open="r")
#           linn <-readLines(conn)
#           eco=(linn[length(linn)])
#           close(conn)
#           substr(eco, 1,5)
#           if(substr(eco,1,5)=="alpha"){
#             #    if(count == 1 && i==1) {text(1.5,xmax-1,label,cex=1)}
#             
#             #PRINT selection coefficient as Latrille et al 2017
#             sel=read.table(paste("selectionCoefficientWithYear_",label,".dat",sep=""),skip=1,header=TRUE)
#             SelReal=(as.numeric(sel[(nrow(sel)/4):nrow(sel),3]))*4*profile$PopulationSize
#             SelReal=SelReal[SelReal>0]
#             prom=mean(as.numeric(SelReal))
#             stand=sd(as.numeric(SelReal))
#             points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),prom,col=arr_color[l],pch=tipo[k],cex=tamany[o])
#             points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),prom+stand,col=1,pch=1,cex=0.3)
#             points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),prom-stand,col=1,pch=1,cex=0.3)
#             
#           }
#         }
#         countE=countE+1
#         count=count+1
#       }
#     }
#   }
# }
# abline(h=26,col=2)

# 
# 
par(mar = c(5,5, 0.5, 0.5),xaxs="i")

countPlot=countPlot+1
ymin=arrayYmin[countPlot]
ymax=arrayYmax[countPlot]

plot(-100,-100,ylim=c(ymin,ymax),xlim=c(xmin,xmax),ylab="AA Div Bind sit",xlab="",xaxt="n",las=2)
for(o in 1:length(arrayP)){
  count=0
  for(k in 1:length(arrayD)){
    for(l in 1:length(arrayU)){ 
      countE=0
      #       for(o in 1:length(arrayE)){
      #         
      p=arrayP[o]
       X=arrayE[o]      
       t=arrayt[o]      
       N=arrayN[o]
      D=arrayD[k]
      C=D
      U=arrayU[l]
      #         #U=C+2
      label=paste(simID,"_N",N,"_t",t,"_",carpeta,"_p",p,"_X",X,"_D",D,"_C",C,"_U",U,sep="")
      setwd(paste(paths,"/",label,sep=""))
      #         
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
          #             
          #             #PRINTS MEAN AA DIVERSITY AT PRDM9 BINDING SITES
          divW=read.table(paste("meanDiversityWithinZnf_",label,".dat",sep=""))
          if(any(is.na(divW)) == FALSE){
            Res = profile$RelevantResidues*2+1;
            #               
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
            #               
            #  for(m in 1:(nrow(divW)/nrow(sel))){
            prom=mean(as.numeric(aaDivInPrdm9Binding[(ncol(aaDivInPrdm9Binding)/4):ncol(aaDivInPrdm9Binding)]))
            points( ((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),prom,col=arr_color[l],pch=tipo[k],cex=tamany[o])
            #               
            #               # points(((i-1)*1000+((k-1)*333)+((l-1)*111)+(o-1)*30),prom,col=arr_color[l],pch=tipo[k],cex=tamany[o])
            #               # }
            #             }
          }
        }
        countE=countE+1
        count=count+1
        #         
      }
      #       
    }
    #     
  }
  #   
}

# 
# ymax=2
# count=0
# plot(-100,-100,ylim=c(0,ymax),xlim=c(xmin,xmax),ylab="Dispersion in array",xlab="",xaxt="n",las=2)
# for(i in 1:length(arrayP)){
#   count=0
#   # for(j in 1:length(arrayU)){
#   for(k in 1:length(arrayD)){
#     for(l in 1:length(arrayU)){ 
#       countE=0
#       for(o in 1:length(arrayE)){
#         
#         p=arrayP[i]
#          X=arrayE[o]       t=arrayt[o]       N=arrayN[o]
#         D=arrayD[k]
#         C=D
#         U=arrayU[l]
#         #U=C+2
#         label=paste(simID,"_N",N,"_t",t,"_",carpeta,"_p",p,"_X",X,"_D",D,"_C",C,"_U",U,sep="")
#         setwd(paste(paths,"/",label,sep=""))
#         
#         
#         if(count == 1 && i==1) {text(1.5,xmax-1,label,cex=1)}
#         er=("std_error.dat")
#         val=file.info(er)$size
#         if(is.na(val) == TRUE){
#           fileName <- "std_output.txt"
#           conn <- file(fileName,open="r")
#           linn <-readLines(conn)
#           eco=(linn[length(linn)])
#           close(conn)
#           substr(eco, 1,5)
#           if(substr(eco,1,5)=="alpha"){
#             
#             a=read.table(paste("histogramOfSizeOfZnfArray_",label,".dat",sep=""),header=FALSE)
#             novA=a[,3:length(a)]
#             longA=(profile$Generations-profile$BurnIn)/profile$Interval
#             novNovA=mat.or.vec(nrow(sel),longA)
#             
#             for(m in 1:nrow(sel)){
#               for(j in 1:longA){
#                 novNovA[m,j]=mean(as.numeric(novA[((m-1)*longA+j),]))
#               }
#               #lines(equis,novNovA[i,],col=i+1)
#             }
#             if(nrow(sel)>1){
#               novAMean=colMeans(novNovA)
#             }
#             else{
#               novAMean=novNovA
#             }
#             novNovASD=mat.or.vec(1,longA)
#             novNovASD=sd(novNovA)
#             
#             prom=mean(novAMean)
#             stand=mean(novNovASD)
#             vars=mean(novNovASD)^2
#             
#             #  (sizeDispersion=vars)
#             #  (sizeDispersion=vars/prom)
#             #  (sizeDispersion=vars/(prom^2))
#             #prom=mean(colMeans(novA))
#             
#             prom=vars/prom
#             stand=sd()
#             points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),prom,col=arr_color[l],pch=tipo[k],cex=tamany[o])
#             #points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),prom+stand,col=1,pch=1,cex=0.3)
#             #points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),prom-stand,col=1,pch=1,cex=0.3)
#           }
#         }
#         countE=countE+1
#         count=count+1
#         
#       }
#       
#     }
#     
#   }
#   
# }
# 
# ymax=1
# count=0
# plot(-100,-100,ylim=c(0,ymax),xlim=c(xmin,xmax),ylab="Hetz in array",xlab="",xaxt="n",las=2)
# for(i in 1:length(arrayP)){
#   count=0
#   # for(j in 1:length(arrayU)){
#   for(k in 1:length(arrayD)){
#     for(l in 1:length(arrayU)){ 
#       countE=0
#       for(o in 1:length(arrayE)){
#         
#         p=arrayP[i]
#          X=arrayE[o]       t=arrayt[o]       N=arrayN[o]
#         D=arrayD[k]
#         C=D
#         U=arrayU[l]
#         #U=C+2
#         label=paste(simID,"_N",N,"_t",t,"_",carpeta,"_p",p,"_X",X,"_D",D,"_C",C,"_U",U,sep="")
#         setwd(paste(paths,"/",label,sep=""))
#         
#         
#         if(count == 1 && i==1) {text(1.5,xmax-1,label,cex=1)}
#         er=("std_error.dat")
#         val=file.info(er)$size
#         if(is.na(val) == TRUE){
#           fileName <- "std_output.txt"
#           conn <- file(fileName,open="r")
#           linn <-readLines(conn)
#           eco=(linn[length(linn)])
#           close(conn)
#           substr(eco, 1,5)
#           if(substr(eco,1,5)=="alpha"){
#             
#             a=read.table(paste("histogramOfSizeOfZnfArray_",label,".dat",sep=""),header=FALSE)
#             novA=a[,3:length(a)]
#             longA=(profile$Generations-profile$BurnIn)/profile$Interval
#             novNovA=mat.or.vec(nrow(sel),longA)
#             
#             hetz=mat.or.vec(10,longA)
#             for(gg in 1:10){
#               for(hh in 1:longA){
#                 numbers <- unlist(novA[((gg-1)*longA+hh),])
#                 suma=0
#                 for(jj in 1:length(table(numbers))){
#                   freq=(table(numbers)[[jj]])/longA
#                   suma=suma+freq*freq
#                 }
#                 hetz[gg,hh]=1-suma
#                 
#               }
#               #  lines(hetz[gg,],col=gg)
#             }
#             
#             prom=(mean(hetz))
#             stand=sd(hetz)
#             points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),prom,col=arr_color[l],pch=tipo[k],cex=tamany[o])
#             points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),prom+stand,col=1,pch=1,cex=0.3)
#             points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),prom-stand,col=1,pch=1,cex=0.3)
#           }
#         }
#         countE=countE+1
#         count=count+1
#         
#       }
#       
#     }
#     
#   }
#   
# }
# 
# 
# par(mar = c(5,5, 0.5, 0.5),xaxs="i")
# ymax=20
# count=0
# plot(-100,-100,ylim=c(0,ymax),xlim=c(xmin,xmax),ylab="Size or array",xlab="",xaxt="n",las=2)
# for(i in 1:length(arrayP)){
#   count=0
#   # for(j in 1:length(arrayU)){
#   for(k in 1:length(arrayD)){
#     for(l in 1:length(arrayU)){ 
#       countE=0
#       for(o in 1:length(arrayE)){
#         
#         p=arrayP[i]
#          X=arrayE[o]       t=arrayt[o]       N=arrayN[o]
#         D=arrayD[k]
#         C=D
#         U=arrayU[l]
#         #U=C+2
#         label=paste(simID,"_N",N,"_t",t,"_",carpeta,"_p",p,"_X",X,"_D",D,"_C",C,"_U",U,sep="")
#         setwd(paste(paths,"/",label,sep=""))
#         
#         
#         if(count == 1 && i==1) {text(1.5,xmax-1,label,cex=1)}
#         er=("std_error.dat")
#         val=file.info(er)$size
#         if(is.na(val) == TRUE){
#           fileName <- "std_output.txt"
#           conn <- file(fileName,open="r")
#           linn <-readLines(conn)
#           eco=(linn[length(linn)])
#           close(conn)
#           substr(eco, 1,5)
#           if(substr(eco,1,5)=="alpha"){
#             
#             
#             
#             a=read.table(paste("histogramOfSizeOfZnfArray_",label,".dat",sep=""),header=FALSE)
#             novA=a[,3:length(a)]
#             longA=(profile$Generations-profile$BurnIn)/profile$Interval
#             novNovA=mat.or.vec(nrow(sel),longA)
#             
#             for(m in 1:nrow(sel)){
#               for(j in 1:longA){
#                 novNovA[m,j]=mean(as.numeric(novA[((m-1)*longA+j),]))
#               }
#               #lines(equis,novNovA[i,],col=i+1)
#             }
#             if(nrow(sel)>1){
#               novAMean=colMeans(novNovA)
#             }
#             else{
#               novAMean=novNovA
#             }
#             novNovASD=mat.or.vec(1,longA)
#             novNovASD=sd(novNovA)
#             
#             
#             prom=mean(novAMean)
#             stand=mean(novNovASD)
#             vars=mean(novNovASD)^2
#             
#             #  (sizeDispersion=vars)
#             #  (sizeDispersion=vars/prom)
#             #  (sizeDispersion=vars/(prom^2))
#             
#             
#             #  prom=mean(colMeans(novA))
#             
#             #prom=vars/prom
#             points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),prom,col=arr_color[l],pch=tipo[k],cex=tamany[o])
#             points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),prom+stand,col=1,pch=1,cex=0.3)
#             points(((i-1)*1000+((k-1)*250)+((l-1)*60)+(o-1)*60),prom-stand,col=1,pch=1,cex=0.3)
#           }
#         }
#         countE=countE+1
#         count=count+1
#         
#       }
#       
#     }
#     
#   }
#   
# }

axis(1, at=seqequis, labels=round(10^-arrayP,digits=4))
title(xlab="Alpha",sub = label)

#}



#dev.off()


