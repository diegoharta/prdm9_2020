#This file intends to show the results from 1 PZIFE simulation
#It shows the relevant statistics that characterize the evolutionary scenario under which the Red-Queen is developing
#It generates one pdf file 


label="prueba_1.92-N1000-K20-s1-C1-p1a-u3-X3-d1-100.-Z3-j6-a"


setwd(paste("~/Documents/Projects/PZIFE/C_scripts_and_data/",label,sep=""))

m <- rbind(c(1, 1,1), c(2,2,2),c(3,3,3))
layout(m)
par(mar = c(0.5,5, 0.5, 0.5))

#Read files
a=read.table(paste("alleleFreqs_",label,".dat",sep=""))
b=read.table(paste("motifActivity_",label,".dat",sep=""))
c=read.table(paste("alleleZnfArraySize_",label,".dat",sep=""))
#d=read.table(paste("referenceOfActiveAlleles_",label,".dat",sep=""))


colMax <- function(X) apply(X, 2, max)
selected=mat.or.vec(ncol(a),1)

# PRINT ALLELE FREQUENCIES
maxim = colMax(a)
init=0
par(mar = c(0.5,5, 0.5, 0.5))
plot(-100,-100,ylim=c(0,1),xlim=c(init,nrow(a)),ylab="Allele Frequencies",xaxt="n",xlab="")
count=1
for(i in 1:length(maxim)){
  if(maxim[i] > 0.2){
    selected[count]=i #guarda las trayectorias que pasan el threshold
    lines(a[,i],col=i)
    count=count+1
  }
}

#print motif activity
par(mar = c(0.5,5, 0.5, 0.5))
plot(-100,-1000,ylim=c(0,1),xlim=c(init,nrow(b)),ylab="Motif Activity",xaxt="n",xlab="")
for(i in 1:(count-1)){
  points(b[,selected[i]],col=selected[i],cex=0.2)
}

#print size of znf arrays 
par(mar = c(5,5, 0.5, 0.5))
plot(-1000,-1000,ylim=c(0,20),xlim=c(init,nrow(c)),ylab="Allele Znf Array Size",xlab = "Generations after burn-in")
for(i in 1:(count-1)){
  points(c[,selected[i]],col=selected[i],cex=0.2)
}


# PRINT PDF
pdf(paste("alleleFreqs_motifActivity_alleleArraySize_",label,"_ZOOM2.pdf",sep=""), width=8, height= 4)
m <- rbind(c(1, 1,1), c(2,2,2),c(3,3,3))
layout(m)
par(mar = c(0.5,5, 0.5, 0.5))
maxim = colMax(a)
plot(-100,-100,ylim=c(0,1),xlim=c(init,nrow(a)),ylab="Allele Frequencies",xaxt="n",xlab="")
count=1
for(i in 1:length(maxim)){
  if(maxim[i] > 0.2){
    selected[count]=i #guarda las trayectorias que pasan el threshold
    lines(a[,i],col=i)
    count=count+1
  }
}

par(mar = c(0.5,5, 0.5, 0.5))
plot(-100,-1000,ylim=c(0,1),xlim=c(init,nrow(b)),ylab="Motif Activity", xaxt="n",xlab="")
for(i in 1:(count-1)){
  points(b[,selected[i]],col=selected[i],cex=0.2)
  #points(b[,i],col=i,cex=0.2)
  
}

par(mar = c(5,5, 0.5, 0.5))
plot(-1000,-1000,ylim=c(0,20),xlim=c(init,nrow(c)),ylab="Allele Znf Array Size",xlab = "Generations after burn-in")
for(i in 1:(count-1)){
  points(c[,selected[i]],col=selected[i],cex=0.2)
  #points(b[,i],col=i,cex=0.2)
  
}

dev.off()
