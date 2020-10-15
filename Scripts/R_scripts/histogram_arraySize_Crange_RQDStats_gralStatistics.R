
# This file is to generate a pfd with several showing the outcome in znf array size for sevaral (9 or 12) different simulations
setwd(paste("~/Documents/Projects/PZIFE/Plots/",sep=""))

Granlabel="prueba1_Crange_p1_X2"
png(paste("histogramZnfArraySize_",Granlabel,".png",sep=""),width     = 8,height    = 4.5,units     = "in",  res       = 400,   pointsize = 7  )
#pdf("histogramZnfArraySize_severalSimulations.pdf", width=8, height= 4.5)

#layout(matrix(c(1,2,3,4), ncol=4, byrow=TRUE), heights=c(1.5))

m <- rbind(c(1, 2,3), c(4,5,6)
           ,c(7,8,9)
           ,c(10,11,12)
           )
layout(m)
par(mar = c(2,2, 0.5, 0.5))


labelArray=c(
  "prueba_1.85-N100-K20-s1-c0-b1-C0.01a-p1-u2-X1-d3-longC",
  #"prueba_1.85-N100-K20-s1-c0-b1-C0.025a-p1-u2-X1-d3-longC",
  "prueba_1.85-N100-K20-s1-c0-b1-C0.05a-p1-u2-X1-d3-longC", 
  "prueba_1.85-N100-K20-s1-c0-b1-C0.1a-p1-u2-X1-d3-longC", 
  #"prueba_1.85-N100-K20-s1-c0-b1-C0.25a-p1-u2-X1-d3-longC", 
  "prueba_1.85-N100-K20-s1-c0-b1-C0.5a-p1-u2-X1-d3-longC", 
  "prueba_1.85-N100-K20-s1-c0-b1-C1a-p1-u2-X1-d3-longC", 
  "prueba_1.85-N100-K20-s1-c0-b1-C5a-p1-u2-X1-d3-longC", 
  "prueba_1.85-N100-K20-s1-c0-b1-C10a-p1-u2-X1-d3-longC",
  "prueba_1.85-N100-K20-s1-c0-b1-C50a-p1-u2-X1-d3-longC",
  "prueba_1.85-N100-K20-s1-c0-b1-C100a-p1-u2-X1-d3-longC"
  
)




labelArray=c(
 # "prueba_1.85-N100-K20-s1-c0-b1-C0a-p1-u2-X2-d3-longC", 
  "prueba_1.85-N100-K20-s1-c0-b1-C.01a-p1-u2-X2-d3-longC",
  "prueba_1.85-N100-K20-s1-c0-b1-C0.025a-p1-u2-X2-d3-longC",
  "prueba_1.85-N100-K20-s1-c0-b1-C.05a-p1-u2-X2-d3-longC", 
  "prueba_1.85-N100-K20-s1-c0-b1-C0.1a-p1-u2-X2-d3-longD", 
  "prueba_1.85-N100-K20-s1-c0-b1-C0.25a-p1-u2-X2-d3-longC", 
  "prueba_1.85-N100-K20-s1-c0-b1-C.5a-p1-u2-X2-d3-longC", 
  "prueba_1.85-N100-K20-s1-c0-b1-C1a-p1-u2-X2-d3-longC", 
  "prueba_1.85-N100-K20-s1-c0-b1-C2.5a-p1-u2-X2-d3-longC", 
  "prueba_1.85-N100-K20-s1-c0-b1-C5a-p1-u2-X2-d3-longC", 
  "prueba_1.85-N100-K20-s1-c0-b1-C10a-p1-u2-X2-d3-longC", 
  #"prueba_1.85-N100-K20-s1-c0-b1-C15a-p1-u2-X2-d3-longC",
  "prueba_1.85-N100-K20-s1-c0-b1-C1a-p25a-u2-X2-d3-longC", 
  "prueba_1.85-N100-K20-s1-c0-b1-C50a-p1-u2-X2-d3-longC"
  
  )

#labelArray=c("prueba_1.92-N1000-K20-s1-C1a-p1-u2-X3-d1a-100.-Z3-j6-a")


promDiv=mat.or.vec(length(labelArray),1)
promAct=mat.or.vec(length(labelArray),1)
promSel=mat.or.vec(length(labelArray),1)
promArraySize=mat.or.vec(length(labelArray),1)
mutRate=mat.or.vec(length(labelArray),1)
epsilonRatio=mat.or.vec(length(labelArray),1)
gralStats=mat.or.vec(length(labelArray),8)
gralStats[3,4]=5

for(i in 1:length(labelArray)){
  
  label=labelArray[i]
  
  setwd(paste("~/Documents/Projects/PZIFE/C_scripts_and_data/",label,sep=""))
  
  
  profile=read.table(paste("profile_",label,".dat",sep=""),header=TRUE)
  summaryStats=mat.or.vec(2,10)
  maxy=0.4
  
  a=read.table(paste("histogramOfSizeOfZnfArray_",label,".dat",sep=""))
  novA=a[,3:length(a)]
  histot=hist(as.numeric(unlist(novA)),xlim=c(4.5,20.5),ylim=c(0,maxy),freq=FALSE,breaks=c(4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5,13.5,14.5,15.5,16.5,17.5,18.5,19.5,20.5), main="")
  Prop=profile$SampleSize*2*profile$Runs*(profile$Generations-profile$BurnIn)/20
  Prop=1
  alpha=(profile$BirthRate*4/5)/profile$DeathRate
  kmin=5
  kmax =20
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
  for(k in 5:20){
    points(k,pik[k],col=2,pch=20)
  }
  sum(pik)
  #text(10,.15*Prop,paste("bplus = ",profile$BirthRate,sep=""))
  #text(10,.14*Prop,paste("bminus = ",profile$DeathRate,sep=""))
  #text(10,.13*Prop,paste("alpha = ",profile$Alpha,sep=""))
  tamanyo=0.7
  
  text(13.5,(maxy-.03)*Prop,label,cex=tamanyo)
  text(15,(maxy-.04)*Prop,paste("N = ",profile$PopulationSize,sep=""),cex=tamanyo)
  #text(10,.15*Prop,paste("bplus = ",profile$BirthRate,sep=""),cex=tamanyo)
  #text(10,.14*Prop,paste("bminus = ",profile$DeathRate,sep=""),cex=tamanyo)
  text(15,(maxy-.05)*Prop,paste("alpha = ",profile$Alpha,sep=""),cex=tamanyo)
  text(15,(maxy-.06)*Prop,paste("C = ",profile$GeneConversionRate,sep=""),cex=tamanyo)
  #text(10,.11*Prop,paste("time = ",profile$Generations,sep=""),cex=tamanyo)
  #text(10,.10*Prop,paste("runs = ",profile$Runs,sep=""),cex=tamanyo)
  
  promArraySize[i]=median(colMeans(novA))
  abline(v=promArraySize[i],col=2)
  
  #PRDM9 DIVERSITY
  c=read.table(paste("prdmDiversity_",label,".dat",sep=""))
  promDiv[i]=mean(as.numeric(c[1,(ncol(c)/2):ncol(c)]))
  text(15,(maxy-.09)*Prop,paste("Dvsty = ",promDiv[i],sep=""),cex=tamanyo)
  
  #PRINT RECOMBINATION ACTIVITY AS LATRILLE ET AL 2017
  d=read.table(paste("recombinationActivity_",label,".dat",sep=""))
  promAct[i]=mean(as.numeric(d[1,(ncol(d)/2):ncol(d)]))
  text(15,(maxy-.1)*Prop,paste("Actvty = ",promAct[i],sep=""),cex=tamanyo)
  
  #PRINT selection coefficient as Latrille et al 2017
  e=read.table(paste("selectionCoefficient_",label,".dat",sep=""),header=TRUE)
  prom=mean(as.numeric(e[(nrow(e)/2):nrow(e),2]))
  #text(15,.13*Prop,prom,cex=tamanyo)
  selValue=4*profile$PopulationSize*prom
  promSel[i]=selValue
  text(15,(maxy-.08)*Prop,paste("4Ns = ",selValue,sep=""),cex=tamanyo)
    
    
    bStats=read.table(paste("generalStatistics_",label,".dat",sep=""))
    for(k in 1:8){
      gralStats[i,k]=unlist(as.numeric(bStats[k]))
    }
    mutRate[i]=gralStats[i,7]/((profile$Generations-profile$BurnIn))
    text(15,(maxy-.11)*Prop,bquote(paste(u," = ",.(mutRate[i]),sep="")),cex=tamanyo)
    epsilonRatio[i]=profile$ErosionRate/mutRate[i]
    text(15,(maxy-.12)*Prop,bquote(paste(epsilon," = ",.(epsilonRatio[i]),sep="")),cex=tamanyo)
}
dev.off()



#PLOTS DIVERSITY, ACTIVITY, 4Ns AND MEAN ARRAY LENGTH AS A FUNCTION OF C
png(paste("redQueenStats_Crange_",Granlabel,".png",sep=""),width     = 8,height    = 4.5,units     = "in",  res       = 400,   pointsize = 7  )

m <- rbind(c(1, 1), c(2,3),c(4,5),c(6,7))
layout(m,heights=c(0.5,2,2,2))
par(mar = c(0,0,0,0))
arr_color=c("goldenrod4","royalblue","orangered","grey50","forestgreen","darkorchid3")

plot.new()
text(0.5,0.5,paste("N = ",profile$PopulationSize,"  ","bplus = ",profile$BirthRate,"  ",
                   "bminus = ",profile$DeathRate,"  ","alpha = ",profile$Alpha,"  ",
                   "time = ",profile$Generations,"  ","vg = ",profile$ErosionRate,"  ",
                   "u = ",profile$Theta,sep=""),cex=1.2,font=2)

par(mar = c(5,5, 0.5, 0.5))
equis=c(0.01,0.025,0.05,0.1,.25,.5,1,2.5,5,10,25,50)
#equis=c(0.01,0.05,0.1,.5,1,5,10,50,100)

plot(equis,promDiv,col=arr_color[1],log="x",ylab="PRDM9 Diversity",xlab="",pch=16)
#points(equis,promDiv,col=1)
plot(equis,promAct,col=arr_color[2],log="x",ylab="Recombination activity",xlab="",pch=16)
plot(equis,promSel,col=arr_color[3],log="x",ylab="4Ns",xlab="C",pch=16)
plot(equis,promArraySize,col=arr_color[4],log="x",ylab="Mean array length",xlab="C",pch=16)

plot(equis,mutRate,col=arr_color[5],log="x",ylab="Effective mutation rate",xlab="C",pch=16)
plot(equis,epsilonRatio,col=arr_color[6],log="x",ylab=bquote(epsilon),xlab="C",pch=16)

dev.off()


#PLOTS EFFECTIVE AND TOTAL MUTATIONS AS A FUNCTION OF THE AVERAGE ARRAY SIZE ASSOCIATED TO EACH SIMULATION

m <- rbind(c(1,1),c(2,3,4,5),c(6,7,8,9))
layout(m,heights=c(1,3,3))

par(mar = c(0,0,0,0))
plot.new()
text(0.5,0.5,paste("N = ",profile$PopulationSize,"  ","bplus = ",profile$BirthRate,"  ",
                   "bminus = ",profile$DeathRate,"  ","alpha = ",profile$Alpha,"  ",
                   "time = ",profile$Generations,"  ","vg = ",profile$ErosionRate,"  ",
                   "u = ",profile$Theta,sep=""),cex=1.2,font=2)

ylabArr=c("Effective Znf muts","Total Znf muts", "Effective Point muts", "Total Point muts",
          "Effective GeneConv muts", "Total GeneConv muts", "Total effective muts", "Total muts")
par(mar = c(5,5, 0.5, 0.5))
for(kk in 1:4){
  # plot(-100,-100,ylim=c(0,1.1),xlim=c(.001,100),log="x",ylab="Count relative to maximum",xlab="Average size of Znf array in run")
  for(k in 1:2){
    j=(kk-1)*2+k
    plot(promArraySize,gralStats[,j],col=arr_color[kk],cex=2.5,pch=type[k],ylab=ylabArr[j],xlab="Average Znf array size")
  }
}



#Effective over total
normBstats=mat.or.vec(length(promArraySize),1)

m <- rbind(c(1),c(2))
layout(m,heights=c(1,6))

par(mar = c(0,0,0,0))
plot.new()
text(0.5,0.5,paste("N = ",profile$PopulationSize,"  ","bplus = ",profile$BirthRate,"  ",
                   "bminus = ",profile$DeathRate,"  ","alpha = ",profile$Alpha,"  ",
                   "time = ",profile$Generations,"  ","vg = ",profile$ErosionRate,"  ",
                   "u = ",profile$Theta,sep=""),cex=1.2,font=2)


par(mar = c(5,5, 0.5, 0.5))
plot(-100,-100,ylim=c(0,0.4),xlim=c(4,21),ylab="Effective / total mutation rate",xlab="Average size of Znf array in run")
for(kk in 1:4){
  k=(kk-1)*2+1
  j=(kk-1)*2+2
  for(i in 1:length(promArraySize)){
    normBstats[i]=gralStats[i,k]/gralStats[i,j]
  }
  points(promArraySize,normBstats,col=arr_color[kk],cex=1.5,pch=16)
  
}


legend(x="topright",ncol=1, #x.intersp=c(0,0,0,0,0.4,0.4,0.4), lwd=c(NA,NA,NA,NA,1,1,1),
       #text.width=c(1,1,1,1,1,1),
       c("Znf","Point","GeneConv","All"),col=c(arr_color[1],arr_color[2],arr_color[3],arr_color[4]),
       pch=16,cex=1.5,bty="n")

text(10,0.4,label)








