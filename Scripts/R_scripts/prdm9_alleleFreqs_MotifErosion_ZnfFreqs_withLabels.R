
# library(RColorBrewer)
# n <- 1000
# qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
# col_vect = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
# col_vector = sample(col_vect, n,replace = TRUE)

#col_vector = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#b15928')

#col_vector = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf','#377eb8','#4daf4a','#984ea3','#e41a1c','#ff7f00','#f781bf','#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf','#e41a1c',
#                '#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf','#377eb8','#4daf4a','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf')

# col_vector = c('#000000','#1f78b4','#e31a1c','#ff7f00', '#33a02c','#6a3d9a')
# n <- 50
# colfunc <- colorRampPalette(col_vector)
# colors <- colfunc(n)
# resortedcolors <- colors[c(seq(1,n,10), seq(2,n,10), seq(3,n,10), seq(4,n,10), seq(5,n,10), seq(6,n,10), seq(7,n,10), seq(8,n,10), seq(9,n,10), seq(10,n,10))]
# col_vector <- resortedcolors

col_vector_mbv = c('red3', 'darkorange2','gold', 'forestgreen', 'seagreen2', 'royalblue3', 'mediumblue', 'darkmagenta')
n <- 50
colfunc <- colorRampPalette(col_vector_mbv)
colors_mbv <- colfunc(n)
resortedcolors_mbv <- colors_mbv[c(seq(1,n,10), seq(6,n,10), seq(2,n,10), seq(7,n,10), seq(3,n,10), seq(8,n,10), seq(4,n,10), seq(9,n,10), seq(5,n,10), seq(10,n,10))]
col_vector<- resortedcolors_mbv

#col_vector = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#b15928')


sim=2
p=2
X=3
C=1
U=3
starTime=201
endTime=900

label=paste("prueba1_cluster_prdm9_N1000_t1M_r",sim,"_p2_X3_C",C,"_U3.",sep="")
label2=paste("prueba1_cluster_prdm9_N1000_t1M_r",sim,"_N1000_p",p,"_X",X,"_DNull_C",C,"_U",U,".",sep="")

setwd(paste("~/Documents/Projects/PZIFE/Prdm9Nicolas/dataFromCluster/TR_2019_02_19_b/TR_2019_02_19_b_trace/",sep=""))

m <- rbind(c(1,1,1), c(2,2,2),c(3,3,3))
layout(m,heights = c(3,3,4))

par(mar = c(0.5,5, 0.5, 0.5))

nrow(a)
a=read.table(paste(label2,"allelic_class_freq_history_new",sep=""))
a=a[starTime:endTime,]
d=read.table(paste(label2,"allelic_class_history_new",sep=""))
d=d[starTime:endTime,]
act=read.table(paste(label2,"alelic_class_act_history_new",sep=""))
act=act[starTime:endTime,]


#profile=read.table(paste("std_output_",label2,"txt",sep=""),header=TRUE)

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
newB=mat.or.vec(nrow(act),length(sortUq))
for(i in 2:length(sortUq)){
  for(k in 1:nrow(a)){
    j=1
    while(d[k,j]!=0 && d[k,j]<=sortUq[i] && j<=(length(sortUq)-1)){ 
    #while(d[k,j]<=sortUq[i] && j<=(length(sortUq)-1)){ 
        
        if(d[k,j]==sortUq[i]){ 
        newA[k,i-1]=a[k,j]
        newB[k,i-1]=act[k,j]
      }
      j=j+1
    }
  }
}


umbral=0
#PRINTS ALLELIC CLASS FREQUENCIES
maxim = colMax(newA)
init=0
par(mar = c(0.5,5, 0.5, 0.5),xaxs="i")
plot(-100,-100,ylim=c(0,2),xlim=c(init,nrow(a)),ylab="Allelic Class Frequencies",xaxt="n",xlab="",cex.lab=1.4)
count=1
abline(h=1,col=1)
for(i in 1:length(maxim)){
  if(maxim[i] > umbral){
    selected[count]=i #guarda las trayectorias que pasan el threshold
    lines(newA[,i],col=col_vector[count])
    count=count+1
  }
}


#PRINTS ALLELIC CLASS NUMBER AT TIME OF OCCURRENCE

#To determine where to place the label corresponding to each allelic class
times=mat.or.vec(1,ncol(newA)-1)
counts=mat.or.vec(1,ncol(newA)-1)
moment=mat.or.vec(1,ncol(newA)-1)
sumaCols=mat.or.vec(1,ncol(newA)-1)

for(i in 1:(ncol(newA)-1)){
  for(j in 1:nrow(newA)){
    if(newA[j,i]!=0){
      times[i]=times[i]+j
      moment[i]=moment[i]+j*newA[j,i]
      counts[i]=counts[i]+1
      sumaCols[i]=sumaCols[i]+newA[j,i]
    }
  }
  times[i]=times[i]/counts[i]
  moment[i]=moment[i]/sumaCols[i]
}
#prints allelic class numbers on top
countInvasions=1
countPosition=0
classInvasionArray=c()
for(jj in 1:(length(sortUq)-1)){
  if(maxim[jj]>umbral){
    countPosition=countPosition+1
    textoA=sortUq[jj+1]
    while(nchar(textoA)<9){
      textoA <- paste("0", textoA,sep="")
    }  
    textoB=paste(substr(textoA,1,3), "-", substr(textoA,4,6), "-", substr(textoA,7,9), sep="")
    lugar=moment[jj]
    text(lugar,2-.1*((countPosition)%%9),textoB,cex=1.2,col=col_vector[countPosition])
    # text(lugar,1.5,textoB,cex=1.2,col=1)
    
    if(lugar != 0){
      classInvasionArray[countInvasions]=lugar
      countInvasions=countInvasions+1
    }
  }
}



#PRINTS MOTIF ACTIVITY
par(mar = c(0.5,5, 0.5, 0.5))
plot(100,1000,ylim=c(0,1),xlim=c(init,nrow(act)),ylab="Recombination Activity", xaxt="n",xlab="",cex.lab=1.4)
for(i in 1:(count-1)){
  for(j in 1:nrow(newB)){
    if(newB[j,selected[i]]!=0){
      points(j,newB[j,selected[i]],col=col_vector[i],cex=0.8,pch=20)
      #points(newB[,i],col=i,cex=0.2)
    }
  }
}


#PRINTS ZINC FINGER FREQUENCIES

a=read.table(paste(label2,"zf_freq_history_new",sep=""))
a=a[starTime:endTime,]
d=read.table(paste(label2,"zf_history_new",sep=""))
d=d[starTime:endTime,]

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

newA_Znf=mat.or.vec(nrow(a),length(sortUq))
for(i in 2:length(sortUq)){
  for(k in 1:nrow(a)){
    j=1
   # while(d[k,j]!=0 && d[k,j]<=sortUq[i] && j<=(length(sortUq)-1)){ 
    while(d[k,j]<=sortUq[i] && j<=(length(sortUq)-1)){ 
      
     if(d[k,j]==sortUq[i]){ 
        newA_Znf[k,i-1]=a[k,j]
      }
      j=j+1
    }
  }
}

umbral=0.001
#PRINTS ZNF FREQUENCIES
maxim = colMax(newA_Znf)
init=0
par(mar = c(0.5,5, 0.5, 0.5),xaxs="i")
plot(-100,-100,ylim=c(0,2),xlim=c(init,nrow(a)),ylab="Allelic Class Frequencies",xlab="",cex.lab=1.4)
count=1
abline(h=1,col=1)
for(i in 1:length(maxim)){
  if(maxim[i] > umbral){
    selected[count]=i #guarda las trayectorias que pasan el threshold
    lines(newA_Znf[,i],col=col_vector[count])
    count=count+1
  }
}


#PRINTS Znf NUMBER AT TIME OF OCCURRENCE

#To determine where to place the label corresponding to each allelic class
times=mat.or.vec(1,ncol(newA_Znf)-1)
counts=mat.or.vec(1,ncol(newA_Znf)-1)
moment=mat.or.vec(1,ncol(newA_Znf)-1)
sumaCols=mat.or.vec(1,ncol(newA_Znf)-1)


for(i in 1:(ncol(newA_Znf)-1)){
  for(j in 1:nrow(newA_Znf)){
    if(newA_Znf[j,i]!=0){
      times[i]=times[i]+j
      moment[i]=moment[i]+j*newA_Znf[j,i]
      counts[i]=counts[i]+1
      sumaCols[i]=sumaCols[i]+newA_Znf[j,i]
    }
  }
  times[i]=times[i]/counts[i]
  moment[i]=moment[i]/sumaCols[i]
}


countInvasions=1
countPosition=0
classInvasionArray=c()
for(jj in 1:(length(sortUq)-1)){
  if(maxim[jj]>umbral){
    countPosition=countPosition+1
    textoA=sortUq[jj+1]
    while(nchar(textoA)<3){
      textoA <- paste("0", textoA,sep="")
    }  
    #  textoB=paste(substr(textoA,1,3), "-", substr(textoA,4,6), "-", substr(textoA,7,9), sep="")
    lugar=times[jj]
    text(lugar,2-.1*((countPosition)%%9),textoA,cex=1.2,col=col_vector[countPosition])
    # text(lugar,1.5,textoB,cex=1.2,col=1)
    
    if(lugar != 0){
      classInvasionArray[countInvasions]=lugar
      countInvasions=countInvasions+1
    }
  }
}


# CALCULATES TAU FROM AUTOCORRELATIONS

#plot.new()
crosshomo=mat.or.vec(1,nrow(newA)-1)
crosshomoTot=mat.or.vec(1,nrow(newA)-1)
crosshomoCount=mat.or.vec(1,nrow(newA)-1)


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
#plot(crosshomo)

x=0
mitad=crosshomo[1]/2
repeat{
  x=x+1
  if(crosshomo[x]<mitad){
    break
  }
}
decorrelationTime=x
#abline(v=decorrelationTime,col=2)

#Calculates autocorrelation time for znfingers.
crosshomoZnf=mat.or.vec(1,nrow(newA_Znf)-1)

for(lag in 1:(nrow(newA_Znf))){
  startRow=0
  countComparisons=0
  crosshomo[lag]=0
  repeat{
    startRow=startRow+1
    endRow=startRow+lag-1
    if(endRow>nrow(newA_Znf)){
      break
    }else{
      for(j in 1:ncol(newA_Znf)){
        homozyg=newA_Znf[startRow,j]*newA_Znf[endRow,j]
        crosshomoZnf[lag]=crosshomoZnf[lag]+homozyg
      }
      countComparisons=countComparisons+1
    }
  }
  crosshomoZnf[lag]=crosshomoZnf[lag]/countComparisons
}
equis=seq(1,length(crosshomo))
#plot(equis,crosshomoZnf[equis])

x=0
mitad=crosshomoZnf[1]/2
repeat{
  x=x+1
  if(crosshomoZnf[x]<mitad){
    break
  }
  if(x>(length(crosshomoZnf)-1)){
    x=0
    break
  }
}
decorrelationTimeZnf=x
#abline(v=decorrelationTimeZnf,col=2)

Prop =1.1
Prop2=1/30

#plot(-100,-100,xlim=c(0,100),ylim=c(0,1))
#text(10*Prop,2.5,label,cex=tamanyo)
#text(10*Prop,24*Prop2,paste("N = ",profile$PopulationSize,sep=""),cex=tamanyo)
text(10*Prop,22*Prop2,paste("alpha = ",profile$alpha,sep=""),cex=tamanyo)
text(10*Prop,20*Prop2,bquote(paste(rho," = ",.(profile$rho),sep = "")),cex=tamanyo)

#text(30*Prop,24*Prop2,paste("DD = ",profile$BirthRate,sep=""),cex=tamanyo)
#text(10,.14*Prop,paste("bminus = ",profile$DeathRate,sep=""),cex=tamanyo)

text(30*Prop,22*Prop2,paste("C = ",profile$C,sep=""),cex=tamanyo)
text(30*Prop,20*Prop2,paste("U = ",profile$U,sep=""),cex=tamanyo)
#text(10,.11*Prop,paste("time = ",profile$Generations,sep=""),cex=tamanyo)
#text(10,.10*Prop,paste("runs = ",profile$Runs,sep=""),cex=tamanyo)

text(50*Prop,22*Prop2,paste("AlleleDvsty = ",profile$allele_div,sep=""),cex=tamanyo)
text(50*Prop,24*Prop2,paste("Actvty = ",profile$meanrec,sep=""),cex=tamanyo)
#text(50*Prop,20*Prop2,paste("4Ns = ",selValue,sep=""),cex=tamanyo)
text(50*Prop,20*Prop2,paste("Znf_array_div = ",profile$zf_divarray,sep=""),cex=tamanyo)
text(50*Prop,18*Prop2,paste("Anf_pop_div = ",profile$zf_divpop,sep=""),cex=tamanyo)


text(70*Prop,24*Prop2,bquote(paste(tau,"_AC = ",.(profile$class_tau),sep="")),cex=tamanyo)
text(70*Prop,22*Prop2,bquote(paste("DecorrTime_AC = ",.(decorrelationTime),sep="")),cex=tamanyo)
scaledDecor=decorrelationTime/2/1000*100
text(70*Prop,20*Prop2,bquote(paste("DecorrTime_AC_scaled = ",.(scaledDecor),sep="")),cex=tamanyo)
text(70*Prop,16*Prop2,bquote(paste(tau,"_Znf = ",.(profile$zf_tau),sep="")),cex=tamanyo)
text(70*Prop,14*Prop2,bquote(paste("DecorrTime_Znf = ",.(decorrelationTimeZnf),sep="")),cex=tamanyo)
scaledDecorZnf=decorrelationTimeZnf/2/1000*100
text(70*Prop,12*Prop2,bquote(paste("DecorrTime_Znf_corr = ",.(scaledDecorZnf),sep="")),cex=tamanyo)


# gralStats=c()
# mutRate=c()
# epsilonRatio=c()
# bStats=read.table(paste("generalStatistics_",label,".dat",sep=""))
# for(k in 1:9){
#   gralStats[k]=unlist(as.numeric(unlist(bStats[1,k])))
# }
# mutRate[i]=gralStats[9]/((profile$Generations-profile$BurnIn))/(2*profile$PopulationSize)*(4*profile$PopulationSize)
# text(50*Prop,18*Prop2,bquote(paste(mu," = ",.(mutRate[i]),sep="")),cex=tamanyo)
# epsilonRatio[i]=profile$ErosionRate/mutRate[i]
# text(50*Prop,16*Prop2,bquote(paste(epsilon," = ",.(epsilonRatio[i]),sep="")),cex=tamanyo)
# text(50*Prop,14*Prop2,bquote(paste(kappa," = ",.(arraySizeHet),sep="")),cex=tamanyo)
# 
# text(70*Prop,24*Prop2,bquote(paste(tau,"_AC = ",.(tau),sep="")),cex=tamanyo)
# text(70*Prop,22*Prop2,bquote(paste(tau,"d_AC = ",.(tauDiv),sep="")),cex=tamanyo)
# text(70*Prop,20*Prop2,bquote(paste(tau,"2_AC = ",.(tau2),sep="")),cex=tamanyo)
# text(70*Prop,18*Prop2,bquote(paste(tau,"2d_AC = ",.(tau2Div),sep="")),cex=tamanyo)
# text(70*Prop,16*Prop2,bquote(paste(Div,"_AC = ",.(meanDiversities[4]),sep="")),cex=tamanyo)
# 
# 
# text(85*Prop,24*Prop2,bquote(paste(tau,"_znf = ",.(tauZnf),sep="")),cex=tamanyo)
# text(85*Prop,22*Prop2,bquote(paste(tau,"d_znf = ",.(tauDivZnf),sep="")),cex=tamanyo)
# text(85*Prop,20*Prop2,bquote(paste(tau,"2_znf = ",.(tau2Znf),sep="")),cex=tamanyo)
# text(85*Prop,18*Prop2,bquote(paste(tau,"2d_znf = ",.(tau2DivZnf),sep="")),cex=tamanyo)
# text(85*Prop,16*Prop2,bquote(paste(Div,"_znf = ",.(meanDiversities[1]),sep="")),cex=tamanyo)
# 
# print(paste(profile$PointMutRate,profile$GeneConversionRate,profile$ErosionRate,profile$Alpha,promAct,meanDiversities[2],
#             meanDiversities[4],meanDiversities[1],tau,tauDiv,tau2,tau2Div,collapse ="    "))
# 
# 

# 
# 
# orden=sort(classInvasionArray)
# timeDiffsBetweenInvasions=c()
# for(yy in 1:length(orden)-1){
#   timeDiffsBetweenInvasions[yy]=orden[yy+1]-orden[yy]
# }
# avTimeBetweenInvasions=mean(timeDiffsBetweenInvasions)
# #avTimeBetweenInvasions=avTimeBetweenInvasions*promDivsty
# #Ttau=avTimeBetweenInvasions*meanDiversities[4]
# Ttau=avTimeBetweenInvasions*profile$Interval#*promDivsty
# 
# avArea=mean(lifetime$Freqs)
# tau = avArea*profile$Interval*profile$Interval/(profile$Generations-profile$BurnIn)
# tauDiv=tau*meanDiversities[4]
# 
# avSqArea=mean(lifetime$SqFreqs)
# tau2=avSqArea/avArea*profile$Interval#*profile$Interval/(profile$Generations-profile$BurnIn)
# tau2Div=tau2*meanDiversities[4]
# 
# 
# 
# 
# 
# #PRINTS MOTIF ACTIVITY
# par(mar = c(0.5,5, 0.5, 0.5))
# plot(100,1000,ylim=c(0,1),xlim=c(init,nrow(b)),ylab="Recombination Activity", xaxt="n",xlab="",cex.lab=1.4)
# for(i in 1:(count-1)){
#   for(j in 1:nrow(newB)){
#     if(newB[j,selected[i]]!=0){
#       points(j,newB[j,selected[i]],col=col_vector[i],cex=0.8,pch=20)
#       #points(newB[,i],col=i,cex=0.2)
#     }
#   }
# }


#PRDM9 DIVERSITY
#divsty=read.table(paste("prdmDiversity_",label,".dat",sep=""))
#promDivsty=mean(as.numeric(divsty[1,(ncol(divsty)/10):ncol(divsty)]))
# par(mar = c(0.5,5, 0.5, 0.5))
# plot(-1000,-1000,ylim=c(0,16),xlim=c(init,ncol(divsty)),ylab="Diversity",xlab = "",xaxt="n")
# equis=seq(1,ncol(divsty),1)
# lines(equis,divsty[1,],col=1)
# abline(h=promDivsty,col=2)
# counti=0
# for(i in 1:ncol(divsty)){
#   if(divsty[1,i]==1){
#     counti=counti+1
#   }
# }
# text(30,1,paste("Av. div = ",promDivsty,sep=""))
# subtitulo=bquote(paste("N = ",.(profile$PopulationSize),"  ",alpha," = ",.(profile$Alpha),"  ",rho," = ",.(profile$ErosionRate),
#                        "  D = ",.(profile$BirthRate),"  ","C = ",.(profile$GeneConversionRate),"  ","U = ",.(profile$PointMutRate),"  ",
#                        sep =""))


#PRINTS Znf ALLELE FREQUENCIES
# par(mar = c(6,5, 0.5, 0.5))
# colorZnf=c()
# maxim = colMax(znfStats)
# summa = apply(znfStats,2,sum)
# init=0
# par(mar = c(5,5, 0.5, 0.5))
# plot(-100,-100,ylim=c(-.1,1),xlim=c(init,nrow(znfStats)),ylab="Zinc Finger Frequencies",xlab = "Generations after burn-in",cex.lab=1.4, cex.sub=1, sub = subtitulo)
# countZ=1
# for(i in 1:length(maxim)){
#   if(maxim[i] > .1){
#     selected2[countZ]=i #guarda las trayectorias que pasan el threshold
#     #selected2Color[countZ]=
#     x1 <- runif(1, 1, 9)
#     lines(znfStats[,i],col=col_vector[countZ])
#     colorZnf[i-1]=countZ
#     countZ=countZ+1
#   }
# }
# 
# 
# newColorOrderZ=c()
# for(kk in 1:(countZ-1)){
#   for(jj in 1:nrow(lifetimeZnf)){
#     if(lifetimeZnf[jj,]$ZnfNumber == sortUq[selected2[kk]+1]){
#       newColorOrderZ[jj]=kk
#     }
#   }
# }

# v <-colSums(znfStats)
# suma=0
# counter=0
# for(j in 1:length(v)){
#   if(v[j]>0){
#     suma=suma+as.numeric(v[j])
#     counter=counter+1
#   }
# }
# avAreaZnf=suma/counter
# znfDv=mean(unlist((allClassDivsty[1])))
# tauZnf=avAreaZnf*znfDv
# print(tau)
# print(tauZnf)
# 
# avAreaZnf=mean(lifetimeZnf$Freqs)
# znfDv=mean(unlist((allClassDivsty[1])))
# tauZnf2=avAreaZnf*znfDv
# print(tauZnf2)
# 
# sum(lifetime$Freqs)/nrow(lifetime)
# mean(lifetime$Freqs)
# 
# avAreaZnf=mean(lifetimeZnf$Freqs)
# tauZnf = avAreaZnf*profile$Interval
# tauDivZnf=tauZnf*meanDiversities[1]
# 
# avSqAreaZnf=mean(lifetimeZnf$SqFreqs)
# tau2Znf=avSqAreaZnf/avAreaZnf*profile$Interval
# tau2DivZnf=tau2Znf*meanDiversities[1]
# 
# 
# umbral=1
# countZnfInvasions=1
# znfInvasionArray=c()
# for(jj in 1:nrow(lifetimeZnf)){
#   if(lifetimeZnf[jj,]$SqFreqs>umbral){
#     textoA=lifetimeZnf[jj,]$ZnfNumber
#     while(nchar(textoA)<3){
#       textoA <- paste("0", textoA,sep="")
#     }
#     #textoB=paste(substr(textoA,1,3), "-", substr(textoA,4,6), "-", substr(textoA,7,9), sep="")
#     lugar=(lifetimeZnf[jj,]$Moment-profile$BurnIn)/profile$Interval
#     text(lugar,-0.05,textoA,cex=1.2,col=col_vector[colorZnf[lifetimeZnf[jj,]$Znf]])
#     # text(lugar,umbral/10,"*")
#     znfInvasionArray[countZnfInvasions]=lugar
#     countZnfInvasions=countZnfInvasions+1
#   }
# }




#dev.off()
