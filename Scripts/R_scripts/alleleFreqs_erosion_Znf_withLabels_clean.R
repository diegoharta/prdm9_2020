
library(RColorBrewer)
n <- 100
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vect = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
col_vector = sample(col_vect, n,replace = TRUE)

col_vector = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#b15928')
col_vector = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf','#377eb8','#4daf4a','#984ea3','#e41a1c','#ff7f00','#f781bf','#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf','#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf')
label="prueba6_1.999_N200_DNullC0.04aU3_p2X3k110c"
label="prueba32_pzife_2.011_N200_p2_X3_DNull_C0_U3"

#setwd(paste("~/Documents/Projects/PZIFE/C_scripts_and_data/",label,sep=""))
setwd(paste("~/Documents/Projects/PZIFE/C_scripts_and_data/data2FromCluster/TR_2019_02_04/",label,sep=""))


#png(filename = "Rplot%03d.png",
#   width = 480, height = 480, units = "px", pointsize = 12,
#  bg = "white",  res = NA, …,
# type = c("cairo", "cairo-png", "Xlib", "quartz"), antialias)



# PRINT PDF
#png(paste("alleleFreqs_motifActivity_znfFreqs_",label,".png",sep=""), width=837, height= 653,units="px",pointsize = 12)



m <- rbind(c(1,1,1), c(2,2,2),c(3,3,3))
layout(m,heights = c(3,2,2.5))

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
    j=1
    while(d[k,j]!=0 && d[k,j]<=sortUq[i] && j<=(length(sortUq)-1)){ 
      if(d[k,j]==sortUq[i]){ 
        newA[k,i-1]=a[k,j]
        newB[k,i-1]=b[k,j]
        newC[k,i-1]=c[k,j]
        newE[k,i-1]=e[k,j]
      }
      j=j+1
    }
   }
}


#PRINTS ALLELE FREQUENCIES
maxim = colMax(newA)
init=0
par(mar = c(0.5,5, 0.5, 0.5),xaxs="i")
plot(-100,-100,ylim=c(0,1.7),xlim=c(init,nrow(a)),ylab="Allelic Class Frequencies",xaxt="n",xlab="",cex.lab=1.4)
count=1
abline(h=1,col=1)
for(i in 1:length(maxim)){
  if(maxim[i] > 0.4){
    selected[count]=i #guarda las trayectorias que pasan el threshold
    lines(newA[,i],col=col_vector[count])
    count=count+1
  }
}

#PRINTS ALLELIC CLASS NUMBER AND POINTS AT TIME OF OCCURRENCE
lifetime=read.table(paste("allelicClassLifetime_",label,".dat",sep=""),header=TRUE)
lifetimeZnf=read.table(paste("znfLifetime_",label,".dat",sep=""),header=TRUE)
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
umbral=1
countInvasions=1
countPosition=0
classInvasionArray=c()
for(jj in 1:nrow(lifetime)){
  if(lifetime[jj,]$SqFreqs>umbral){
    countPosition=countPosition+1
    textoA=lifetime[jj,]$BindingMotif
    while(nchar(textoA)<9){
      textoA <- paste("0", textoA,sep="")
    }  
    textoB=paste(substr(textoA,1,3), "-", substr(textoA,4,6), "-", substr(textoA,7,9), sep="")
    lugar=(lifetime[jj,]$Moment-profile$BurnIn)/profile$Interval
    #text(lugar,1-.1*(lugar%%5),textoB,cex=.8,col=col_vector[newColorOrder[jj]])
    text(lugar,1.7-.1*((countPosition)%%7),textoB,cex=1.2,col=col_vector[newColorOrder[jj]])
    # text(lugar,umbral,"*")
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

avArea=mean(lifetime$Freqs)
tau = avArea*profile$Interval*profile$Interval/(profile$Generations-profile$BurnIn)
tauDiv=tau*meanDiversities[4]

avSqArea=mean(lifetime$SqFreqs)
tau2=avSqArea/avArea*profile$Interval#*profile$Interval/(profile$Generations-profile$BurnIn)
tau2Div=tau2*meanDiversities[4]



#PRINTS MOTIF ACTIVITY
par(mar = c(0.5,5, 0.5, 0.5))
plot(100,1000,ylim=c(0,1),xlim=c(init,nrow(b)),ylab="Recombination Activity", xaxt="n",xlab="",cex.lab=1.4)
for(i in 1:(count-1)){
  for(j in 1:nrow(newB)){
    if(newB[j,selected[i]]!=0){
      points(j,newB[j,selected[i]],col=col_vector[i],cex=0.8,pch=20)
      #points(newB[,i],col=i,cex=0.2)
    }
  }
}

subtitulo=bquote(paste("N = ",.(profile$PopulationSize),"  ",alpha," = ",.(profile$Alpha),"  ",rho," = ",.(profile$ErosionRate),
                       "  D = ",.(profile$BirthRate),"  ","C = ",.(profile$GeneConversionRate),"  ","U = ",.(profile$PointMutRate),"  ",
                       sep =""))


#PRINTS Znf ALLELE FREQUENCIES
par(mar = c(6,5, 0.5, 0.5))
colorZnf=c()
maxim = colMax(znfStats)
summa = apply(znfStats,2,sum)
init=0
par(mar = c(5,5, 0.5, 0.5))
plot(-100,-100,ylim=c(0,1),xlim=c(init,nrow(znfStats)),ylab="Zinc Finger Frequencies",xlab = "Generations after burn-in",cex.lab=1.4, cex.sub=1, sub = subtitulo)
countZ=1
for(i in 1:length(maxim)){
  if(maxim[i] > .3){
    selected2[countZ]=i #guarda las trayectorias que pasan el threshold
    #selected2Color[countZ]=
    x1 <- runif(1, 1, 9)
    lines(znfStats[,i],col=col_vector[countZ])
    colorZnf[i-1]=countZ
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


umbral=1
countZnfInvasions=1
znfInvasionArray=c()
for(jj in 1:nrow(lifetimeZnf)){
  if(lifetimeZnf[jj,]$SqFreqs>umbral){
    textoA=lifetimeZnf[jj,]$ZnfNumber
    while(nchar(textoA)<3){
      textoA <- paste("0", textoA,sep="")
    }
    #textoB=paste(substr(textoA,1,3), "-", substr(textoA,4,6), "-", substr(textoA,7,9), sep="")
    lugar=(lifetimeZnf[jj,]$Moment-profile$BurnIn)/profile$Interval
    text(lugar,.95,textoA,cex=1.2,col=col_vector[colorZnf[lifetimeZnf[jj,]$Znf]])
    # text(lugar,umbral/10,"*")
    znfInvasionArray[countZnfInvasions]=lugar
    countZnfInvasions=countZnfInvasions+1
  }
}
