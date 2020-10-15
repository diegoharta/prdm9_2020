
#m <- rbind(c(1,2,3,4),c(5,6,7,8),c(9,10,11,12),c(13,14,15,16),c(17,17,17,17))
m <- rbind(c(1,1,2,2,3),c(4,5,6,7,8),c(9,10,11,12,13),c(14,15,16,17,18),c(19,20,21,22,23),c(24,24,24,24,24))
layout(m,heights=c(2,2,2,2,2,1.5))

par(mar = c(2,2, 0.5, 0.5))


arr_color=c("#000000", "#009E73", "#e79f00", "#9ad0f3", "#0072B2", "#D55E00", 
            "#CC79A7", "#F0E442")


labelArray=c(
  "prueba_1.90-N100-K20-s1-c0-b1-C10a-p1-u2-X2-d2-longD-Z4-j6",
  
  
"prueba_1.90-N100-K20-s1-c0-b1-C1a-p1-u3-X3-d2-longD-Z4-j6",
  
  "prueba_1.90-N100-K20-s1-c0-b1-C0.1a-p1-u4-X4-d2-longD-Z4-j6",
  
"prueba_1.90-N100-K20-s1-c0-b1-C0.01a-p1-u5-X5-d2-longD-Z4-j6",
"prueba_1.90-N100-K20-s1-c0-b1-C0.001a-p1-u6-X6-d2-longD-Z4-j6",
"prueba_1.92-N1000-K20-s1-C1a-p1-u2-X3-d1a-100.-Z3-j6-a"
#"prueba_1.90-N100-K20-s1-c0-b1-C10a-p1-u0-X2-d3-longD-Z4-j6"
)


for(i in 1:length(labelArray)){
  
  label=labelArray[i]
  setwd(paste("~/Documents/Projects/PZIFE/C_scripts_and_data/",label,sep=""))
  
  profile=read.table(paste("profile_",label,".dat",sep=""),header=TRUE)
  
  
 
  kmin=6
  kmax=20
  
  summaryStats=mat.or.vec(2,10)
  
  a=read.table(paste("histogramOfSizeOfZnfArray_",label,".dat",sep=""))
  novA=a[,3:length(a)]
  kmin=6
  kmax =20


  histot=hist(as.numeric(unlist(novA)),xlim=c(kmin-.5,kmax+0.5),ylim=c(0,.22),freq=FALSE,
              breaks=c(4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5,13.5,14.5,15.5,16.5,17.5,18.5,19.5,20.5), main="")

  Prop=profile$SampleSize*2*profile$Runs*(profile$Generations-profile$BurnIn)/20
  Prop=1
  alpha=(profile$BirthRate)/profile$DeathRate
  
  suma=0
  for(k in kmin:kmax){
    suma=suma+alpha^(k-kmin)/k
    
  }
  pikmin=1/(kmin*suma)
  cte=Prop*pikmin*kmin/(alpha^kmin)
  pik=mat.or.vec(1,kmax)
  for(k in kmin:kmax){
    pik[k]=cte*(alpha^k)/k
  }
  
  #plot(0,0,xlim=c(5,20),ylim=c(0,.6))
  for(k in kmin:kmax){
    points(k,pik[k],col=2,pch=20)
  }
  sum(pik)
 
 
  plot.new()
  tamanyo=0.7
  text(0.5,0,label,cex=tamanyo)
  
  tamanyo=1
  text(0.25,.9,paste("dd = ",profile$BirthRate,sep = ""),cex=tamanyo)
  text(0.25,0.75,paste("U = ",profile$Theta,sep = ""),cex=tamanyo)
  text(0.25,.6,paste("C = ",round(profile$GeneConversionRate,digits=4),sep = ""),cex=tamanyo)
  text(0.25,0.45,bquote(paste(rho," = ",.(profile$ErosionRate),sep = "")),cex=tamanyo)

  #PRDM9 DIVERSITY
  c=read.table(paste("prdmDiversity_",label,".dat",sep=""))
  prom=mean(as.numeric(c[1,(ncol(c)/2):ncol(c)]))
  text(0.75,.9,paste("Dvsty = ",round(prom,digits=4),sep=""),cex=tamanyo)
  
  #PRINT RECOMBINATION ACTIVITY AS LATRILLE ET AL 2017
  d=read.table(paste("recombinationActivity_",label,".dat",sep=""))
  prom=mean(as.numeric(d[1,(ncol(d)/2):ncol(d)]))
  text(0.75,.75,paste("Actvty = ",round(prom,digits=4),sep=""),cex=tamanyo)
  
  #PRINT selection coefficient as Latrille et al 2017
  e=read.table(paste("selectionCoefficient_",label,".dat",sep=""),header=TRUE)
  prom=mean(as.numeric(e[(nrow(e)/2):nrow(e),2]))
  #text(15,.13*Prop,prom,cex=tamanyo)
  selValue=4*profile$PopulationSize*prom
  text(0.75,.6,paste("4Ns = ",round(selValue,digits=4),sep=""),cex=tamanyo)
  

  bStats=read.table(paste("generalStatistics_",label,".dat",sep=""))
  for(k in 1:8){
    gralStats[i,k]=unlist(as.numeric(bStats[k]))
  }
  mutRate[i]=gralStats[i,7]/((profile$Generations-profile$BurnIn)*4*profile$PopulationSize)
  mux=round(mutRate[i]*4*profile$PopulationSize,digits=4)
  text(0.75,.45,bquote(paste(mu," = ",.(mux),sep="")),cex=tamanyo)
  epsilonRatio[i]=profile$ErosionRate/(mutRate[i]*4*profile$PopulationSize)
  eps=round(epsilonRatio[i],digits=6)
  text(0.75,.3,bquote(paste(epsilon," = ",.(eps),sep="")),cex=tamanyo)
  
  
  a=read.table(paste("arraySizeChange_",label,".dat",sep=""))
  equis=seq(kmin,kmax-1,1)
  sube=a[1,]
  baja=a[2,]
  genconv=a[3,]
  point=a[4,]
  subeeff=a[5,]
  bajaeff=a[6,]
  genconveff=a[7,]
  pointeff=a[8,]
  
  
  length(sube)
  equis=seq(kmin,kmax-1,1)
  
  #propi=20
  relSube=mat.or.vec(kmax-kmin,1)
  relBaja=mat.or.vec(kmax-kmin,1)
  relSube2=mat.or.vec(kmax-kmin,1)
  relBaja2=mat.or.vec(kmax-kmin,1)
  relSube3=mat.or.vec(kmax-kmin,1)
  relBaja3=mat.or.vec(kmax-kmin,1)
  
  relGeneConv=mat.or.vec(kmax-kmin,1)
  relGeneConv2=mat.or.vec(kmax-kmin,1)
  relGeneConv3=mat.or.vec(kmax-kmin,1)
  relPoint=mat.or.vec(kmax-kmin,1)
  relPoint2=mat.or.vec(kmax-kmin,1)
  relPoint3=mat.or.vec(kmax-kmin,1)
  
 
  for(j in 1:(kmax-kmin)){
    relSube[j]=subeeff[j]/sube[j]
    relBaja[j]=bajaeff[j]/baja[j]
    
    relSube2[j]=sube[j]/histot$counts[j+1]
    relBaja2[j]=baja[j]/histot$counts[j+1]
   
     relSube3[j]=subeeff[j]/histot$counts[j+1]
    relBaja3[j]=bajaeff[j]/histot$counts[j+1]
    
    relGeneConv[j]=genconveff[j]/genconv[j]
    relGeneConv2[j]=genconv[j]/histot$counts[j+1]
    relGeneConv3[j]=genconveff[j]/histot$counts[j+1]
   
     relPoint[j]=pointeff[j]/point[j]
    relPoint2[j]=point[j]/histot$counts[j+1]
    relPoint3[j]=pointeff[j]/histot$counts[j+1]
  }

  diffs=mat.or.vec(kmax-kmin-1,1)
  for(j in 1:(kmax-kmin-1)){
    restaeffs=bajaeff[j]-subeeff[j+1]
    sumaeffs=bajaeff[j]+subeeff[j+1]
    
    diffs[j]=restaeffs/sumaeffs
 
    
  }

  
  equis2=seq(kmin,kmax-2,1)
  par(mar = c(2,2, 0.5, 0.5))
  plot(equis2, diffs,col=arr_color[5],pch=13)

  par(mar = c(2,2, 0.5, 0.5))
  plot(equis, sube,col=arr_color[1],pch=19,xaxt="n")
  plot(equis, subeeff,col=arr_color[1],pch=15,xaxt="n")

  plot(equis,relSube2,col=arr_color[1],pch=17,xaxt="n")
  plot(equis,relSube3,col=arr_color[1],pch=18,xaxt="n")
  plot(equis,relSube,col=arr_color[1],pch=14,xaxt="n")
  
  par(mar = c(2,2, 0.5, 0.5))
  
  plot(equis, baja,col=arr_color[2],pch=17,xaxt="n")
  plot(equis, bajaeff,col=arr_color[2],pch=15,xaxt="n")
 
  plot(equis,relBaja2,col=arr_color[2],pch=17,xaxt="n")
  plot(equis,relBaja3,col=arr_color[2],pch=18,xaxt="n")
  plot(equis,relBaja,col=arr_color[2],pch=14,xaxt="n")
  
  par(mar = c(2,2, 0.5, 0.5))

  plot(equis,genconv,col=arr_color[3],pch=19,xaxt="n")
  plot(equis,genconveff,col=arr_color[3],pch=15,xaxt="n")

  plot(equis,relGeneConv2,col=arr_color[3],pch=17,xaxt="n")
  plot(equis,relGeneConv3,col=arr_color[3],pch=18,xaxt="n")
  plot(equis,relGeneConv,col=arr_color[3],pch=14,xaxt="n")
  
  par(mar = c(2,2, 0.5, 0.5))

  plot(equis,point,col=arr_color[4],pch=19)
  plot(equis,pointeff,col=arr_color[4],pch=15)
  
  plot(equis,relPoint2,col=arr_color[4],pch=17)
  plot(equis,relPoint3,col=arr_color[4],pch=18)
  plot(equis,relPoint,col=arr_color[4],pch=14)
  
  
  diffs1=mat.or.vec(kmax-kmin,1)
  
  for(j in 1:(kmax-kmin-1)){
    restaeffs=bajaeff[j]-subeeff[j+1]
    sumaeffs=bajaeff[j]+subeeff[j+1]
    
    diffs1[j]=restaeffs/sumaeffs
    
    
  }
  diffs[kmax-kmin]=0
  
  
  minimo=min(min(sube),min(baja))
  maximo=.7
  
 # plot(equis,relSube,col=arr_color[1],pch=19,cex=tamanyo,ylim=c(0,maximo))
#  points(equis,relBaja,col=arr_color[2],pch=19,cex=tamanyo)
 # points(equis,relGeneConv,col=arr_color[3],pch=19)
  #points(equis,relPoint,col=arr_color[4],pch=19)
  #oints(equis,diffs1,col=arr_color[5],pch=17)
  
  
  

  plot.new()
  
  
  #  length(equis)
  legend("bottom",c("duplication","deletion","geneConversion","point","total","effective","total/histogram","effective/histgram","effective/total","deletion bias"),
          ncol = 5, col=c(arr_color[1],arr_color[2],arr_color[3],arr_color[4],1,1,1,1,1,arr_color[5]),
         pch=c(NA,NA,NA,NA,19,15,17,18,14,13),lty=c(1,1,1,1,NA,NA,NA,NA,NA,NA),lwd=c(2,2,2,2,NA,NA,NA,NA,NA,NA))
  
  
}
