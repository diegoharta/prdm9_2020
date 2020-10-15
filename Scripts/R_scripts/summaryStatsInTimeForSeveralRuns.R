
#This file intends to show the results from several PZIFE simulations of several runs each 
#For each simulation it will plot the evolution in time of the summary statistics (diversity, recActivity,selCoeff) 
# and will print the average histogram

#It shows the relevant statistics that characterize the evolutionary scenario under which the Red-Queen is developing
#It generates one pdf file 


labelArray=c(
  #"prueba_1.95-N1000-K20-s1-C1-p1-u3-X3-d1-100.-Z3-j6-o2-a"
  #"prueba_1.94-N1000-K20-s2-C1a-p2-u2-X4-d1a-30.-Z3-j6-o2-a"
  "prueba_1.95-N1000-K20-s3-C1-p2-u3-X5-d1-30.-Z3-j6-o2-a"
)

Prop=1
for(k in 1:length(labelArray)){
  
  label=labelArray[k]
  setwd(paste("~/Documents/Projects/PZIFE/C_scripts_and_data/",label,sep=""))
  
  
  # PRINT PDF
  #pdf(paste("prdm9Diversity_recActivity_selecCoeff_Histogram_severalRuns_",label,".pdf",sep=""), width=8, height= 9)
  m <- rbind(c(1, 1,1), c(2,2,2),c(3,3,3),c(4,4,4),c(5,5,5),c(6,6,6))
  layout(m,heights = c(2,2,2,2,2.5,2))
  par(mar = c(0.5,5, 0.5, 0.5),xaxs="i")
  
  
  #PRDM9 DIVERSITY
  divt=read.table(paste("prdmDiversity_",label,".dat",sep=""))
  promDiv=mean(as.numeric(divt[1,(ncol(divt)/2):ncol(divt)]))
  # text(50*Prop,22,paste("Dvsty = ",prom,sep=""),cex=tamanyo)
  init=0
  par(mar = c(0.5,5, 0.5, 0.5),xaxs="i")
  plot(-100,-100,ylim=c(0,10),xlim=c(init, ncol(divt)),ylab="Prdm9 diversity",xaxt="n",xlab="")
  equis=seq(1,ncol(divt),1)
  for(i in 1:nrow(divt)){
      lines(equis,divt[i,],col=i)
  }
  text(50*Prop,1,paste("Dvsty = ",promDiv,sep=""),cex=tamanyo)
  
  #PRINT RECOMBINATION ACTIVITY AS LATRILLE ET AL 2017
  rec=read.table(paste("recombinationActivity_",label,".dat",sep=""))
  promRec=mean(as.numeric(rec[1,(ncol(rec)/2):ncol(rec)]))
  # text(50*Prop,24,paste("Actvty = ",prom,sep=""),cex=tamanyo)
  plot(-100,-100,ylim=c(0,1),xlim=c(init, ncol(rec)),ylab="Recombination activity",xaxt="n",xlab="")
  equis=seq(1,ncol(rec),1)
  for(i in 1:nrow(rec)){
    lines(equis,rec[i,],col=i)
  }
  text(50*Prop,.1,paste("Actvty = ",promRec,sep=""),cex=tamanyo)
  
  
  #PRINT selection coefficient as Latrille et al 2017
  sel=read.table(paste("selectionCoefficient_",label,".dat",sep=""))
  prom=mean(as.numeric(sel[1,(ncol(sel)/2):ncol(sel)]))
  #text(15,.13*Prop,prom,cex=tamanyo)
  selValue=4*profile$PopulationSize*prom
  plot(-100,-100,ylim=c(0,.12),xlim=c(init, ncol(sel)),ylab="Selection coefficient",xaxt="n",xlab="")
  equis=seq(1,ncol(sel),1)
  for(i in 1:nrow(sel)){
    lines(equis,sel[i,],col=i)
  }
  text(50*Prop,.1,paste("4Ns = ",selValue,sep=""),cex=tamanyo)
  
  
  #PRINTS MEAN AA DIVERSITY AT PRDM9 BINDING SITES
  par(mar = c(2,5, 0.5, 0.5),xaxs="i")
  divW=read.table(paste("meanDiversityWithinZnf_",label,".dat",sep=""))
  Res = profile$RelevantResidues*2+1;
  aaDivInPrdm9Binding=mat.or.vec(1,nrow(divW))
  for(i in 1:nrow(divW)){
    sumRelRes=divW[i,2]+divW[i,4]+divW[i,6]
    aaDivInPrdm9Binding[i]=sumRelRes/sum(divW[i,])
  }
  equiss=seq(1,nrow(divW),1)
  plot(equiss,aaDivInPrdm9Binding,xlim=c(0,nrow(divW)),ylim=c(0,1),ylab="AA Div. at Binding sites",type = "l")
  prom=mean(as.numeric(aaDivInPrdm9Binding[1,(ncol(aaDivInPrdm9Binding)/2):ncol(aaDivInPrdm9Binding)]))
  abline(h=prom,col="red")
  text(50*Prop,0.1,paste("Av. aa div at binding sites = ",prom,sep=""))
  
  #PRINTS HISTOGRAM OF ALLELE SIZE
  par(mar = c(3,5, 0.5, 0.5),xaxs="i")
  kmin=6
  kmax=20
  summaryStats=mat.or.vec(2,10)
  his=read.table(paste("histogramOfSizeOfZnfArray_",label,".dat",sep=""))
  novA=his[,3:length(his)]
  kmin=6
  kmax =20
  
  histot=hist(as.numeric(unlist(novA)),xlim=c(kmin-.5,kmax+0.5),ylim=c(0,.3),freq=FALSE,breaks=c(4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5,13.5,14.5,15.5,16.5,17.5,18.5,19.5,20.5), main="")
  Prop=profile$SampleSize*2*profile$Runs*(profile$Generations-profile$BurnIn)/20
  Prop=1.1
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
 
  for(k in kmin:kmax){
    points(k,pik[k],col=2,pch=20)
  }
  sum(pik)
  tamanyo=0.7
  
  
 
 
plot(-100,-100,ylim=c(15,25),xlim=c(0,Prop*100))

profile=read.table(paste("profile_",label,".dat",sep=""),header=TRUE)
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

text(75*Prop,16,label,cex=tamanyo)



gralStats=c()
mutRate=c()
epsilonRatio=c()
bStats=read.table(paste("generalStatistics_",label,".dat",sep=""))
for(k in 1:9){
  gralStats[k]=unlist(as.numeric(bStats[k]))
}
mutRate[i]=gralStats[9]/((profile$Generations-profile$BurnIn))/(2*profile$PopulationSize)*(4*profile$PopulationSize)
text(50*Prop,24,bquote(paste(mu," = ",.(mutRate[i]),sep="")),cex=tamanyo)
epsilonRatio[i]=profile$ErosionRate/mutRate[i]
text(50*Prop,22.5,bquote(paste(epsilon," = ",.(epsilonRatio[i]),sep="")),cex=tamanyo)

text(75*Prop,24,paste("Dvsty = ",promDiv,sep=""),cex=tamanyo)
text(75*Prop,22,paste("Actvty = ",promRec,sep=""),cex=tamanyo)
text(75*Prop,20,paste("4Ns = ",selValue,sep=""),cex=tamanyo)

# dev.off()

}

