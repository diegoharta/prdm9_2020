
library(RColorBrewer)
n <- 10000
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vect = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
col_vector = sample(col_vect, n,replace = TRUE)

label="prueba29_pzife_2.011_N200_p2_X3_DNull_C2_U3"
label="prueba32_pzife_2.011_N200_p2_X3_DNull_C0_U3"

#setwd(paste("~/Documents/Projects/PZIFE/C_scripts_and_data/",label,sep=""))
setwd(paste("~/Documents/Projects/PZIFE/C_scripts_and_data/data2FromCluster/TR_2019_02_04/",label,sep=""))


m <- rbind(c(1,1,1), c(2,2,2),c(3,3,3),c(4,4,4),c(5,5,5),c(6,6,6))
layout(m,heights = c(2,1.5,1,1.5,1,3))

par(mar = c(0.5,5, 0.5, 0.5))


a=read.table(paste("alleleFreqs_",label,".dat",sep=""))
b=read.table(paste("motifActivity_",label,".dat",sep=""))
c=read.table(paste("alleleZnfArraySize_",label,".dat",sep=""))
d=read.table(paste("referenceOfActiveAlleles_",label,".dat",sep=""))
e=read.table(paste("parentalAlleles_",label,".dat",sep=""))

znfStats=read.table(paste("znfCountStatistics_",label,".dat",sep=""))
lifetime=read.table(paste("allelicClassLifetime_",label,".dat",sep=""),header=TRUE)
allClassDivsty=read.table(paste("allelicClassDiversity_",label,".dat",sep=""))
meanDiversities=colMeans(allClassDivsty)

profile=read.table(paste("profile_",label,".dat",sep=""),header=TRUE)


sum(lifetime$SqFreqs)/63*200


uq_elem=c()
for(i in 1:ncol(d))
{
  uq_elem=c(unique(d[,i]), uq_elem)
  uq_elem=unique(uq_elem)
}
sortUq=sort(uq_elem)

colMax <- function(X) apply(X, 2, max)
selected=mat.or.vec(ncol(a),1)
selected2=mat.or.vec(1000,1)

# Create newA, newB and newC from reference allele list in d

newA=mat.or.vec(nrow(a),length(sortUq))
newB=mat.or.vec(nrow(a),length(sortUq))
newC=mat.or.vec(nrow(a),length(sortUq))
newE=mat.or.vec(nrow(a),length(sortUq))
newF=mat.or.vec(nrow(a),length(sortUq))
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
        newE[k,i-1]=e[k,j]
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


#PRINTS ALLELE FREQUENCIES
maxim = colMax(newA)
init=0
par(mar = c(0.5,5, 0.5, 0.5),xaxs="i")
plot(-100,-100,ylim=c(0,1),xlim=c(init,nrow(a)),ylab="Allele Frequencies",xaxt="n",xlab="")
count=1
for(i in 1:length(maxim)){
  if(maxim[i] > 0.01){
    selected[count]=i #guarda las trayectorias que pasan el threshold
    lines(newA[,i],col=col_vector[count])
    count=count+1
  }
}





#PRINTS ALLELIC CLASS NUMBER AND POINTS AT TIME OF OCCURRENCE
lifetime=read.table(paste("allelicClassLifetime_",label,".dat",sep=""),header=TRUE)
lifetimeZnf=read.table(paste("znfLifetime_",label,".dat",sep=""),header=TRUE)
# for(xx in 1:(count-1)){
#   nums=sortUq[selected[xx]]
#   for(jj in 1:nrow(lifetime)){
#     if(lifetime[jj,]$BindMotifNumber==nums){
#       texto=lifetime[jj,]$BindingMotif
#       lugar=(lifetime[jj,]$Moment-profile$BurnIn)/profile$Interval
#       text(lugar,.1,texto,cex=1,col=col_vector[xx])
#       text(lugar,.2,"*")
#     }
#   }
# }
newColorOrder=c()
for(kk in 1:(count-1)){
  for(jj in 1:nrow(lifetime)){
    if(lifetime[jj,]$BindMotifNumber == sortUq[selected[kk]+1]){
      newColorOrder[jj]=kk
    }
  }
}

divsty=read.table(paste("prdmDiversity_",label,".dat",sep=""))
promDivsty=mean(as.numeric(divsty[1,(ncol(divsty)/10):ncol(divsty)]))
umbral=1/promDivsty
umbral=0
countInvasions=1
classInvasionArray=c()
for(jj in 1:nrow(lifetime)){
  if(lifetime[jj,]$Freqs>umbral){
    textoA=lifetime[jj,]$BindingMotif
    if(nchar(textoA)<9){
      textoA <- paste("0", textoA,sep="")
    }  
    textoB=paste(substr(textoA,1,3), "-", substr(textoA,4,6), "-", substr(textoA,7,9), sep="")
    lugar=(lifetime[jj,]$Moment-profile$BurnIn)/profile$Interval
    text(lugar,1-.1*(lugar%%5),textoB,cex=.8,col=col_vector[newColorOrder[jj]])
    text(lugar,umbral,"*")
    if(lugar != 0){
      classInvasionArray[countInvasions]=lugar
      countInvasions=countInvasions+1
    }
  }
}
orden=sort(classInvasionArray)
timeDiffsBetweenInvasions=c()
for(yy in 1:length(orden)-1){
  timeDiffsBetweenInvasions[yy]=orden[yy+1]-orden[yy]
}
avTimeBetweenInvasions=mean(timeDiffsBetweenInvasions)
#avTimeBetweenInvasions=avTimeBetweenInvasions*promDivsty
#Ttau=avTimeBetweenInvasions*meanDiversities[4]
Ttau=avTimeBetweenInvasions*profile$Interval#*promDivsty

sum(lifetime$Freqs)
sum(lifetimeZnf$Freqs)

avArea=mean(lifetime$Freqs)
tau = avArea*profile$Interval#*profile$Interval/(profile$Generations-profile$BurnIn)
tauDiv=tau*meanDiversities[4]

avSqArea=mean(lifetime$SqFreqs)
tau2=avSqArea/avArea*profile$Interval#*profile$Interval/(profile$Generations-profile$BurnIn)
tau2Div=tau2*meanDiversities[4]





#PRINTS MOTIF ACTIVITY
par(mar = c(0.5,5, 0.5, 0.5))
plot(100,1000,ylim=c(0,1),xlim=c(init,nrow(b)),ylab="Motif Activity", xaxt="n",xlab="")
for(i in 1:(count-1)){
  for(j in 1:nrow(newB)){
    if(newB[j,selected[i]]!=0){
      points(j,newB[j,selected[i]],col=col_vector[i],cex=0.8,pch=20)
      #points(newB[,i],col=i,cex=0.2)
    }
  }
}


#PRDM9 DIVERSITY
#divsty=read.table(paste("prdmDiversity_",label,".dat",sep=""))
#promDivsty=mean(as.numeric(divsty[1,(ncol(divsty)/10):ncol(divsty)]))
par(mar = c(0.5,5, 0.5, 0.5))
plot(-1000,-1000,ylim=c(0,16),xlim=c(init,ncol(divsty)),ylab="Diversity",xlab = "",xaxt="n")
equis=seq(1,ncol(divsty),1)
lines(equis,divsty[1,],col=1)
abline(h=promDivsty,col=2)
counti=0
for(i in 1:ncol(divsty)){
  if(divsty[1,i]==1){
    counti=counti+1
  }
}
text(30,1,paste("Av. div = ",promDivsty,sep=""))

#PRINTS Znf ALLELE FREQUENCIES
maxim = colMax(znfStats)
summa = apply(znfStats,2,sum)
init=0
par(mar = c(0.5,5, 0.5, 0.5),xaxs="i")
plot(-100,-100,ylim=c(0,1),xlim=c(init,nrow(znfStats)),ylab="Allele Frequencies",xaxt="n",xlab="")
countZ=1
for(i in 1:length(maxim)){
  if(maxim[i] > 0){
    selected2[countZ]=i #guarda las trayectorias que pasan el threshold
    lines(znfStats[,i],col=col_vector[countZ])
    countZ=countZ+1
  }
}


v <-colSums(znfStats)
suma=0
counter=0
for(j in 1:length(v)){
  if(v[j]>0){
    suma=suma+as.numeric(v[j])
    counter=counter+1
  }
}
avAreaZnf=suma/counter
znfDv=mean(unlist((allClassDivsty[1])))
tauZnf=avAreaZnf*znfDv
print(tau)
print(tauZnf)

avAreaZnf=mean(lifetimeZnf$Freqs)
znfDv=mean(unlist((allClassDivsty[1])))
tauZnf2=avAreaZnf*znfDv
print(tauZnf2)

sum(lifetime$Freqs)/nrow(lifetime)
mean(lifetime$Freqs)

avAreaZnf=mean(lifetimeZnf$Freqs)
tauZnf = avAreaZnf*profile$Interval
tauDivZnf=tauZnf*meanDiversities[1]

avSqAreaZnf=mean(lifetimeZnf$SqFreqs)
tau2Znf=avSqAreaZnf/avAreaZnf*profile$Interval
tau2DivZnf=tau2Znf*meanDiversities[1]


umbral=0
countZnfInvasions=1
znfInvasionArray=c()
for(jj in 1:nrow(lifetimeZnf)){
  if(lifetimeZnf[jj,]$Freqs>umbral){
    textoA=lifetimeZnf[jj,]$ZnfNumber
    if(nchar(textoA)<3){
      textoA <- paste("0", textoA,sep="")
    }  
    #textoB=paste(substr(textoA,1,3), "-", substr(textoA,4,6), "-", substr(textoA,7,9), sep="")
    lugar=(lifetimeZnf[jj,]$Moment-profile$BurnIn)/profile$Interval
    text(lugar,1-.1*(lugar%%5),textoA,cex=.8,col=1)#col_vector[newColorOrder[jj]])
    text(lugar,umbral/10,"*")
    znfInvasionArray[countZnfInvasions]=lugar
    countZnfInvasions=countZnfInvasions+1
  }
}
ordenZnf=sort(znfInvasionArray)
timeDiffsBetweenZnfInvasions=c()
for(yy in 1:length(ordenZnf)-1){
  timeDiffsBetweenZnfInvasions[yy]=ordenZnf[yy+1]-ordenZnf[yy]
}
avTimeBetweenZnfInvasions=mean(timeDiffsBetweenZnfInvasions)
TtauZnf=avTimeBetweenZnfInvasions*meanDiversities[2]


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
  points(newC[,selected[i]],col=col_vector[i],cex=0.2)
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
prom=mean(as.numeric(g[1,]))#(ncol(g)/10):ncol(g)]))
text(50*Prop,22,paste("Dvsty = ",prom,sep=""),cex=tamanyo)

#PRINT RECOMBINATION ACTIVITY AS LATRILLE ET AL 2017
d=read.table(paste("recombinationActivity_",label,".dat",sep=""))
prom=mean(as.numeric(d[1,(ncol(d)/6):ncol(d)]))
promAct=mean(as.numeric(d[1,]))
text(50*Prop,24,paste("Actvty = ",prom,sep=""),cex=tamanyo)

#PRINT selection coefficient as Latrille et al 2017
de=read.table(paste("selectionCoefficient_",label,".dat",sep=""))
prom=mean(as.numeric(de[1,(ncol(de)/6):ncol(de)]))
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
text(50*Prop,18,bquote(paste(mu," = ",.(mutRate[i]),sep="")),cex=tamanyo)
epsilonRatio[i]=profile$ErosionRate/mutRate[i]
text(50*Prop,16,bquote(paste(epsilon," = ",.(epsilonRatio[i]),sep="")),cex=tamanyo)
text(50*Prop,14,bquote(paste(kappa," = ",.(arraySizeHet),sep="")),cex=tamanyo)

text(70*Prop,24,bquote(paste(tau,"_AC = ",.(tau),sep="")),cex=tamanyo)
text(70*Prop,22,bquote(paste(tau,"d_AC = ",.(tauDiv),sep="")),cex=tamanyo)
text(70*Prop,20,bquote(paste(tau,"2_AC = ",.(tau2),sep="")),cex=tamanyo)
text(70*Prop,18,bquote(paste(tau,"2d_AC = ",.(tau2Div),sep="")),cex=tamanyo)
text(70*Prop,16,bquote(paste(Div,"_AC = ",.(meanDiversities[4]),sep="")),cex=tamanyo)


text(85*Prop,24,bquote(paste(tau,"_znf = ",.(tauZnf),sep="")),cex=tamanyo)
text(85*Prop,22,bquote(paste(tau,"d_znf = ",.(tauDivZnf),sep="")),cex=tamanyo)
text(85*Prop,20,bquote(paste(tau,"2_znf = ",.(tau2Znf),sep="")),cex=tamanyo)
text(85*Prop,18,bquote(paste(tau,"2d_znf = ",.(tau2DivZnf),sep="")),cex=tamanyo)
text(85*Prop,16,bquote(paste(Div,"_znf = ",.(meanDiversities[1]),sep="")),cex=tamanyo)

print(paste(profile$PointMutRate,profile$GeneConversionRate,profile$ErosionRate,profile$Alpha,promAct,meanDiversities[2],
            meanDiversities[4],meanDiversities[1],tau,tauDiv,tau2,tau2Div,collapse ="    "))




#newA=rbind(c(.1,0,0,.8),c(.1,.2,.2,.5),c(.1,.2,.2,.5),c(.1,.2,.2,.5),c(0,0,1,0))

#newA=rbind(c(.5,.5),c(.5,.5),c(.5,.5))

crosshomo=mat.or.vec(1,nrow(newA)-1)


for(lag in 1:(nrow(newA))){
  startRow=0
  countComparisons=0
  crosshomo[lag]=0
  crosshomoTot[lag]=0
  crosshomoCount[lag]=0
  repeat{
    startRow=startRow+1
    endRow=startRow+lag-1
    if(endRow>nrow(newA)){
      break
    }else{
      for(j in 1:ncol(newA)){
        homozyg=newA[startRow,j]*newA[endRow,j]
        crosshomo[lag]=crosshomo[lag]+homozyg
      }
      countComparisons=countComparisons+1
    }
  }
  crosshomo[lag]=crosshomo[lag]/countComparisons
}
(crosshomo)
equis=seq(1,length(crosshomo))
#
#plot(equis,crosshomo[equis])
crosshomo[1]

x=0
mitad=crosshomo[1]/2
repeat{
  x=x+1
  if(crosshomo[x]<mitad){
    break
  }
}
decorrelationTime=x

text(70*Prop,14,bquote(paste(Decorr," = ",.(decorrelationTime),sep="")),cex=tamanyo)

#newA=znfStats
newZnf=znfStats[,-(which(colSums(znfStats) == 0))] 
crosshomoZnf=mat.or.vec(1,nrow(newZnf)-1)


for(lag in 1:(nrow(newZnf))){
  startRow=0
  countComparisons=0
  crosshomo[lag]=0
  repeat{
    startRow=startRow+1
    endRow=startRow+lag-1
    if(endRow>nrow(newZnf)){
      break
    }else{
      for(j in 1:ncol(newZnf)){
        homozyg=newZnf[startRow,j]*newZnf[endRow,j]
        crosshomo[lag]=crosshomo[lag]+homozyg
      }
      countComparisons=countComparisons+1
    }
  }
  crosshomo[lag]=crosshomo[lag]/countComparisons
}
(crosshomo)
equis=seq(1,length(crosshomo))
#
#plot(equis,crosshomo[equis])
crosshomo[1]

x=0
mitad=crosshomo[1]/2
repeat{
  x=x+1
  if(crosshomo[x]<mitad){
    break
  }
}
decorrelationTime=x

text(85*Prop,14,bquote(paste(Decorr," = ",.(decorrelationTime),sep="")),cex=tamanyo)