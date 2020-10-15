#This file intends to show the results from 1 PZIFE simulation
#It shows the relevant statistics that characterize the evolutionary scenario under which the Red-Queen is developing
#It generates one pdf file 

library(RColorBrewer)
n <- 10000
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vect = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
col_vector = sample(col_vect, n,replace = TRUE)
#pie(rep(1,n), col=sample(col_vector, n))


label="prueba_pzife_1.98_N1600_t30_s1q_p2.60206_X2.60206_D0_C0_U2"


label="prueba_pzife_1.98_N100_t100_s1r_p1.39794_X1.39794_D0_C0_U2"
label="prueba_pzife_1.98_N400_t100_s1u_p0_X0_DNull_CNull_U2"
label="prueba_pzife_1.99_N1000_t100_s1y_p4_X4.52288_DNull_CNull_U0.60206"
label="prueba_pzife_1.999_prueba4_U1"
label="prueba_pzife_1.999_prueba4_D0C0U3"
label= "prueba5_DNullC1U3_p01X3"

label="prueba5_D0C0U3_p2X3_b"

label="prueba5_DNullC0U2_p01X2_b"
label="prueba5_DNullCNullU1b_p01X2"
label="prueba5_DNullCNullU3_p01X3_c"
label="prueba5_D0bC0bU2_p01X2_b"
label="prueba5_DNullC0U2_p0X1_b"
label="prueba6_1.999_N200_DNullC0.04aU3_p2X3k110c"

setwd(paste("~/Documents/Projects/PZIFE/C_scripts_and_data/",label,sep=""))
#setwd(paste("~/Documents/Projects/PZIFE/C_scripts_and_data/dataFromCluster/TrialRun_2018_10_08/s1y/",label,sep=""))


m <- rbind(c(1, 1,1), c(2,2,2),c(3,3,3),c(4,4,4),c(5,5,5),c(6,6,6),c(7,7,7))
layout(m,heights = c(2,1.5,1.5,1.5,1.5,1.5,2.5))

par(mar = c(0.5,5, 0.5, 0.5))

#Read files
a=read.table(paste("alleleFreqs_",label,".dat",sep=""))
b=read.table(paste("motifActivity_",label,".dat",sep=""))
c=read.table(paste("alleleZnfArraySize_",label,".dat",sep=""))
d=read.table(paste("referenceOfActiveAlleles_",label,".dat",sep=""))
e=read.table(paste("parentalAlleles_",label,".dat",sep=""))
g=read.table(paste("allelicClassDiversity_",label,".dat",sep=""))

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
newE=mat.or.vec(nrow(a),length(sortUq))
newF=mat.or.vec(nrow(a),length(sortUq))
newG=mat.or.vec(nrow(a),length(sortUq))
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
        newG[k,i-1]=g[k,j]
        
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

for(j in 1:nrow(newE)){
  for(k in 1:ncol(newE)){
    for(l in 2:length(sortUq)){
      if(sortUq[l]==newE[j,k]){
        newF[j,k]=l
      }
    }
  }
}


#PRINTS ALLELE FREQUENCIES
maxim = colMax(newA)
init=0
par(mar = c(0.5,5, 0.5, 0.5),xaxs="i")
plot(-100,-100,ylim=c(0,1),xlim=c(init,nrow(a)),ylab="Allele Frequencies",xaxt="n",xlab="")
count=1
for(i in 1:length(maxim)){
  if(maxim[i] > 0.1){
    selected[count]=i #guarda las trayectorias que pasan el threshold
    lines(newA[,i],col=col_vector[count])
    count=count+1
  }
}

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
newB[,2]


#PRINTS MOTIF ACTIVITY with parental colors
par(mar = c(0.5,5, 0.5, 0.5))
plot(100,1000,ylim=c(0,1),xlim=c(init,nrow(b)),ylab="Motif Activity", xaxt="n",xlab="")
for(i in 1:(count-1)){
  for(j in 1:nrow(newB)){
    points(j,newB[j,selected[i]],col=col_vector[newF[j,selected[i]]],cex=0.8,pch=20)
    #points(newB[,i],col=i,cex=0.2)
  }
}
#newB[,2]

#PRDM9 DIVERSITY
divsty=read.table(paste("prdmDiversity_",label,".dat",sep=""))
promDiv=mean(as.numeric(divsty[1,(ncol(divsty)/10):ncol(divsty)]))
par(mar = c(0.5,5, 0.5, 0.5))
plot(-1000,-1000,ylim=c(0,20),xlim=c(init,ncol(divsty)),ylab="Diversity",xlab = "",xaxt="n")
equis=seq(1,ncol(divsty),1)
lines(equis,divsty[1,],col=1)
abline(h=promDiv,col=2)
counti=0
for(i in 1:ncol(divsty)){
  if(divsty[1,i]==1){
    counti=counti+1
  }
}
divsty[1,2]
#text(10,1,paste("Div=1 counts: ",counti,sep=""))
text(30,1,paste("Av. div = ",promDiv,sep=""))




#PRINTS ALLELIC CLASS DIVERSITY
par(mar = c(0.5,5, 0.5, 0.5))
plot(100,1000,ylim=c(0,10),xlim=c(init,nrow(g)),ylab="AC Diversity", xaxt="n",xlab="")
for(i in 1:(count-1)){
  for(j in 1:nrow(newG)){
    if(newG[j,selected[i]]!=0){
      points(j,newG[j,selected[i]],col=col_vector[i],cex=0.8,pch=20)
      #points(newB[,i],col=i,cex=0.2)
    }
  }
}
#newB[,2]

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

#h=read.table(paste("prdmDiversity_",label,".dat",sep=""))
#prom=mean(as.numeric(h[1,(ncol(h)/2):ncol(h)]))
text(50*Prop,22,paste("Dvsty = ",promDiv,sep=""),cex=tamanyo)

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
for(k in 1:18){
  gralStats[k]=unlist(as.numeric(unlist(bStats[1,k])))
}
mutRate[i]=gralStats[9]/((profile$Generations-profile$BurnIn))/(2*profile$PopulationSize)*(4*profile$PopulationSize)
text(75*Prop,24,bquote(paste(mu,"_away = ",.(mutRate[i]),sep="")),cex=tamanyo)
mutRate[i]=gralStats[13]/((profile$Generations-profile$BurnIn))/(2*profile$PopulationSize)*(4*profile$PopulationSize)
text(75*Prop,22,bquote(paste(mu,"_birth = ",.(mutRate[i]),sep="")),cex=tamanyo)
epsilonRatio[i]=profile$ErosionRate/mutRate[i]
text(75*Prop,20,bquote(paste(epsilon," = ",.(epsilonRatio[i]),sep="")),cex=tamanyo)




# PRINT PDF
pdf(paste("alleleFreqs_motifActivity_alleleArraySize_FromRef_",label,"_ZOOM2.pdf",sep=""), width=8, height= 9)
m <- rbind(c(1, 1,1), c(2,2,2),c(3,3,3),c(4,4,4),c(5,5,5),c(6,6,6),c(7,7,7))
layout(m,heights = c(1.5,1,1,1,1,1,2.5))

par(mar = c(0.5,5, 0.5, 0.5),xaxs="i")

#PRINTS ALLELE FREQUENCIES
maxim = colMax(newA)
init=0
par(mar = c(0.5,5, 0.5, 0.5),xaxs="i")
plot(-100,-100,ylim=c(0,1),xlim=c(init,nrow(a)),ylab="Allele Frequencies",xaxt="n",xlab="")
count=1
for(i in 1:length(maxim)){
  if(maxim[i] > 0.1){
    selected[count]=i #guarda las trayectorias que pasan el threshold
    lines(newA[,i],col=col_vector[count])
    count=count+1
  }
}

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
newB[,2]


#PRINTS MOTIF ACTIVITY with parental colors
par(mar = c(0.5,5, 0.5, 0.5))
plot(100,1000,ylim=c(0,1),xlim=c(init,nrow(b)),ylab="Motif Activity", xaxt="n",xlab="")
for(i in 1:(count-1)){
  for(j in 1:nrow(newB)){
    points(j,newB[j,selected[i]],col=col_vector[newF[j,selected[i]]],cex=0.8,pch=20)
    #points(newB[,i],col=i,cex=0.2)
  }
}
#newB[,2]

#PRDM9 DIVERSITY
divsty=read.table(paste("prdmDiversity_",label,".dat",sep=""))
prom=mean(as.numeric(divsty[1,(ncol(divsty)/10):ncol(divsty)]))
par(mar = c(0.5,5, 0.5, 0.5))
plot(-1000,-1000,ylim=c(0,12),xlim=c(init,ncol(divsty)),ylab="Diversity",xlab = "",xaxt="n")
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
#text(10,1,paste("Div=1 counts: ",counti,sep=""))
text(30,1,paste("Av. div = ",prom,sep=""))




#PRINTS ALLELIC CLASS DIVERSITY
par(mar = c(0.5,5, 0.5, 0.5))
plot(100,1000,ylim=c(0,10),xlim=c(init,nrow(g)),ylab="AC Diversity", xaxt="n",xlab="")
for(i in 1:(count-1)){
  for(j in 1:nrow(newG)){
    if(newG[j,selected[i]]!=0){
      points(j,newG[j,selected[i]],col=col_vector[i],cex=0.8,pch=20)
      #points(newB[,i],col=i,cex=0.2)
    }
  }
}
#newB[,2]

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
#h=read.table(paste("prdmDiversity_",label,".dat",sep=""))
#prom=mean(as.numeric(h[1,(ncol(h)/2):ncol(h)]))
#text(50*Prop,22,paste("Dvsty = ",prom,sep=""),cex=tamanyo)
text(50*Prop,22,paste("Dvsty = ",promDiv,sep=""),cex=tamanyo)

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
for(k in 1:18){
  gralStats[k]=unlist(as.numeric(unlist(bStats[1,k])))
}
mutRate[i]=gralStats[9]/((profile$Generations-profile$BurnIn))/(2*profile$PopulationSize)*(4*profile$PopulationSize)
text(75*Prop,24,bquote(paste(mu,"_away = ",.(mutRate[i]),sep="")),cex=tamanyo)
mutRate[i]=gralStats[13]/((profile$Generations-profile$BurnIn))/(2*profile$PopulationSize)*(4*profile$PopulationSize)
text(75*Prop,22,bquote(paste(mu,"_birth = ",.(mutRate[i]),sep="")),cex=tamanyo)
epsilonRatio[i]=profile$ErosionRate/mutRate[i]
text(75*Prop,20,bquote(paste(epsilon," = ",.(epsilonRatio[i]),sep="")),cex=tamanyo)

#text(75*Prop,20,bquote(paste(kappa," = ",.(arraySizeHet),sep="")),cex=tamanyo)

# 
# 
# #PRINTS ALLELE FREQUENCIES
# maxim = colMax(newA)
# init=0
# par(mar = c(0.5,5, 0.5, 0.5),xaxs="i")
# plot(-100,-100,ylim=c(0,1),xlim=c(init,nrow(a)),ylab="Allele Frequencies",xaxt="n",xlab="")
# count=1
# for(i in 1:length(maxim)){
#   if(maxim[i] > 0.1){
#     selected[count]=i #guarda las trayectorias que pasan el threshold
#     lines(newA[,i],col=count)
#     count=count+1
#   }
# }
# 
# #PRINTS MOTIF ACTIVITY
# par(mar = c(0.5,5, 0.5, 0.5))
# plot(-100,-1000,ylim=c(0,1),xlim=c(init,nrow(b)),ylab="Motif Activity", xaxt="n",xlab="")
# for(i in 1:(count-1)){
#   for(j in 1:nrow(newB)){
#     points(j,newB[j,selected[i]],col=i,cex=0.4)
#     #points(newB[,i],col=i,cex=0.2)
#   }
# }
# #newB[,2]
# 
# 
# #PRDM9 DIVERSITY
# divsty=read.table(paste("prdmDiversity_",label,".dat",sep=""))
# prom=mean(as.numeric(divsty[1,(ncol(divsty)/2):ncol(divsty)]))
# par(mar = c(0.5,5, 0.5, 0.5))
# plot(-1000,-1000,ylim=c(0,16),xlim=c(init,ncol(divsty)),ylab="Diversity",xlab = "",xaxt="n")
# equis=seq(1,ncol(divsty),1)
# lines(equis,divsty[1,],col=1)
# abline(h=prom,col=2)
# counti=0
# for(i in 1:ncol(divsty)){
#   if(divsty[1,i]==1){
#     counti=counti+1
#   }
# }
# divsty[1,2]
# text(10,1,paste("Div=1 counts: ",counti,sep=""))
# text(30,1,paste("Av. div = ",prom,sep=""))
# 
# 
# 
# #PRINTS MEAN AA DIVERSITY AT PRDM9 BINDING SITES
# f=read.table(paste("meanDiversityWithinZnf_",label,".dat",sep=""))
# 
# Res = profile$RelevantResidues*2+1;
# aaDivInPrdm9Binding=mat.or.vec(1,nrow(f))
# for(i in 1:nrow(f)){
#   sumRelRes=f[i,2]+f[i,4]+f[i,6]
#   aaDivInPrdm9Binding[i]=sumRelRes/sum(f[i,])
# }
# equiss=seq(1,nrow(f),1)
# plot(equiss,aaDivInPrdm9Binding,xlim=c(0,nrow(f)),ylim=c(0,1),xaxt="n",xlab = "",ylab="AA Div. at Binding sites",type = "l")
# prom=mean(as.numeric(aaDivInPrdm9Binding[1,(ncol(aaDivInPrdm9Binding)/2):ncol(aaDivInPrdm9Binding)]))
# abline(h=prom,col="red")
# text(30,0.05,paste("Av. aa div at binding sites = ",prom,sep=""))
# 
# 
# 
# #PLOTS SIZE OF ARRAY
# par(mar = c(5,5, 0.5, 0.5))
# plot(-1000,-1000,ylim=c(4,25),xlim=c(init,nrow(c)),ylab="Allele Znf Array Size",xlab = "Generations after burn-in",sub = label)
# for(i in 1:(count-1)){
#   points(newC[,selected[i]],col=i,cex=0.2)
#   #points(b[,i],col=i,cex=0.2)
#   
# }
# 
# profile=read.table(paste("profile_",label,".dat",sep=""),header=TRUE)
# Prop=nrow(a)/100
# tamanyo=1
# 
# #text(10*Prop,2.5,label,cex=tamanyo)
# text(10*Prop,24,paste("N = ",profile$PopulationSize,sep=""),cex=tamanyo)
# text(10*Prop,22,paste("alpha = ",profile$Alpha,sep=""),cex=tamanyo)
# text(10*Prop,20,bquote(paste(rho," = ",.(profile$ErosionRate),sep = "")),cex=tamanyo)
# 
# text(30*Prop,24,paste("DD = ",profile$BirthRate,sep=""),cex=tamanyo)
# #text(10,.14*Prop,paste("bminus = ",profile$DeathRate,sep=""),cex=tamanyo)
# 
# text(30*Prop,22,paste("C = ",profile$GeneConversionRate,sep=""),cex=tamanyo)
# text(30*Prop,20,paste("U = ",profile$PointMutRate,sep=""),cex=tamanyo)
# #text(10,.11*Prop,paste("time = ",profile$Generations,sep=""),cex=tamanyo)
# #text(10,.10*Prop,paste("runs = ",profile$Runs,sep=""),cex=tamanyo)
# 
# 
# 
# 
# #PRDM9 DIVERSITY
# g=read.table(paste("prdmDiversity_",label,".dat",sep=""))
# prom=mean(as.numeric(g[1,(ncol(g)/2):ncol(g)]))
# text(50*Prop,22,paste("Dvsty = ",prom,sep=""),cex=tamanyo)
# 
# #PRINT RECOMBINATION ACTIVITY AS LATRILLE ET AL 2017
# d=read.table(paste("recombinationActivity_",label,".dat",sep=""))
# prom=mean(as.numeric(d[1,(ncol(d)/2):ncol(d)]))
# text(50*Prop,24,paste("Actvty = ",prom,sep=""),cex=tamanyo)
# 
# #PRINT selection coefficient as Latrille et al 2017
# de=read.table(paste("selectionCoefficient_",label,".dat",sep=""))
# prom=mean(as.numeric(de[1,(ncol(de)/2):ncol(de)]))
# #text(15,.13*Prop,prom,cex=tamanyo)
# selValue=4*profile$PopulationSize*prom
# text(50*Prop,20,paste("4Ns = ",selValue,sep=""),cex=tamanyo)
# 
# gralStats=c()
# mutRate=c()
# epsilonRatio=c()
# bStats=read.table(paste("generalStatistics_",label,".dat",sep=""))
# for(k in 1:9){
#   gralStats[k]=unlist(as.numeric(unlist(bStats[1,k])))
# }
# mutRate[i]=gralStats[9]/((profile$Generations-profile$BurnIn))/(2*profile$PopulationSize)*(4*profile$PopulationSize)
# text(75*Prop,24,bquote(paste(mu," = ",.(mutRate[i]),sep="")),cex=tamanyo)
# epsilonRatio[i]=profile$ErosionRate/mutRate[i]
# text(75*Prop,22.5,bquote(paste(epsilon," = ",.(epsilonRatio[i]),sep="")),cex=tamanyo)



dev.off()
