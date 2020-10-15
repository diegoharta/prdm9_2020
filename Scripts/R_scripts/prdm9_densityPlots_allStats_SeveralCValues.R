setwd("~/Documents/Projects/PZIFE/Prdm9Nicolas/dataFromCluster/TR_2019_02_11/")

col_vector = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf','#377eb8','#4daf4a','#984ea3','#e41a1c','#ff7f00','#f781bf','#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf','#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf')

previo="std_output_"
final="txt"
N="N5000a"

m <- rbind(c(1,2),c(3,4),c(5,6),c(7,8),c(9,9))
layout(m,heights = c(2.5,2.5,2.5,2.5,1.5))

par(mar = c(5,5, 0.5, 0.5))
Carray=c(0,0.30103,0.69897,1,2)
Simarray=c(1,2,3,4,5,6,7,8,10,11,12,13,14,15,16,17,18,19,20)
numsims=8

distrib=c()
for(x in 1:length(Carray))
{
  #plot(-100,-100,xlim=c(0,21),ylim=c(0,1),xlab="MeanRec")
  for(j in 1:length(Simarray)){
    i=Simarray[j]
    label=paste(previo,"prueba1_cluster_prdm9_",N,"_t1M_r",i,"_p2_X3_DNull_C",Carray[x],"_U3.",final,sep="")
    a=read.table(label,header=TRUE)
    distrib[j]=a$meanrec
    # points(i,a$meanrec)
  }
  if(x==1){
    plot(density(distrib),xlab="MeanRec",col=col_vector[x],xlim=c(0,1),main="")
  }else{
    lines(density(distrib),col=col_vector[x])
  }
}

for(x in 1:length(Carray))
{
  #plot(-100,-100,xlim=c(0,21),ylim=c(0,1),xlab="MeanRec")
  for(j in 1:length(Simarray)){     i=Simarray[j]
   label=paste(previo,"prueba1_cluster_prdm9_",N,"_t1M_r",i,"_p2_X3_DNull_C",Carray[x],"_U3.",final,sep="")
    a=read.table(label,header=TRUE)
    distrib[j]=a$meandiv
    #points(i,a$meandiv)
  }
  if(x==1){
    plot(density(distrib),xlab="MeanDiv",col=col_vector[x],xlim=c(0,25),main="")
  }else{
    lines(density(distrib),col=col_vector[x])
  }
}

for(x in 1:length(Carray))
{
  #plot(-100,-100,xlim=c(0,21),ylim=c(0,1),xlab="MeanRec")
  for(j in 1:length(Simarray)){     i=Simarray[j]
   label=paste(previo,"prueba1_cluster_prdm9_",N,"_t1M_r",i,"_p2_X3_DNull_C",Carray[x],"_U3.",final,sep="")
    a=read.table(label,header=TRUE)
    distrib[j]=a$meanclassdiv
    #points(i,a$meanclassdiv)
  }
  if(x==1){
    plot(density(distrib),xlab="MeanClassDiv",col=col_vector[x],xlim=c(0,10),main="")
  }else{
    lines(density(distrib),col=col_vector[x])
  }
}

for(x in 1:length(Carray))
{
  #plot(-100,-100,xlim=c(0,21),ylim=c(0,1),xlab="MeanRec")
  for(j in 1:length(Simarray)){     i=Simarray[j]
   label=paste(previo,"prueba1_cluster_prdm9_",N,"_t1M_r",i,"_p2_X3_DNull_C",Carray[x],"_U3.",final,sep="")
    a=read.table(label,header=TRUE)
    distrib[j]=a$meanzfdiv
    #points(i,a$meanzfdiv)
  }
  if(x==1){
    plot(density(distrib),xlab="MeanZnfDiv",col=col_vector[x],xlim=c(0,6),ylim=c(0,2),main="")
  }else{
    lines(density(distrib),col=col_vector[x])
  }
}

distribTau=c()
for(x in 1:length(Carray))
{
  #plot(-100,-100,xlim=c(0,21),ylim=c(0,1),xlab="MeanRec")
  for(j in 1:length(Simarray)){     i=Simarray[j]
   label=paste(previo,"prueba1_cluster_prdm9_",N,"_t1M_r",i,"_p2_X3_DNull_C",Carray[x],"_U3.",final,sep="")
    a=read.table(label,header=TRUE)
    distrib[j]=a$tau
    points(i,a$tau)
  }
  if(x==1){
    plot(density(distrib),xlab="Tau",col=col_vector[x],xlim=c(30,60),main="")
  }else{
    lines(density(distrib),col=col_vector[x])
  }
}

for(x in 1:length(Carray))
{
  #plot(-100,-100,xlim=c(0,21),ylim=c(0,1),xlab="MeanRec")
  for(j in 1:length(Simarray)){     i=Simarray[j]
   label=paste(previo,"prueba1_cluster_prdm9_",N,"_t1M_r",i,"_p2_X3_DNull_C",Carray[x],"_U3.",final,sep="")
    a=read.table(label,header=TRUE)
    distrib[j]=a$taudiv
    points(i,a$taudiv)
  }
  if(x==1){
    plot(density(distrib),xlab="TauDiv",col=col_vector[x],xlim=c(0,400),main="")
  }else{
    lines(density(distrib),col=col_vector[x])
  }
}

for(x in 1:length(Carray))
{
  #plot(-100,-100,xlim=c(0,21),ylim=c(0,1),xlab="MeanRec")
  for(j in 1:length(Simarray)){     i=Simarray[j]
   label=paste(previo,"prueba1_cluster_prdm9_",N,"_t1M_r",i,"_p2_X3_DNull_C",Carray[x],"_U3.",final,sep="")
    a=read.table(label,header=TRUE)
    distrib[j]=a$tau2
   # points(i,a$tau2)
  }
  if(x==1){
    plot(density(distrib),xlab="MeanTau2",col=col_vector[x],xlim=c(0,100),main="")
  }else{
    lines(density(distrib),col=col_vector[x])
  }
}

for(x in 1:length(Carray))
{
  #plot(-100,-100,xlim=c(0,21),ylim=c(0,1),xlab="MeanRec")
  for(j in 1:length(Simarray)){     i=Simarray[j]
   label=paste(previo,"prueba1_cluster_prdm9_",N,"_t1M_r",i,"_p2_X3_DNull_C",Carray[x],"_U3.",final,sep="")
    a=read.table(label,header=TRUE)
    distrib[j]=a$tau2div
    #points(i,a$tau2div)
  }
  if(x==1){
    plot(density(distrib),xlab="Tau2Div",col=col_vector[x],xlim=c(100,130),main="")
  }else{
    lines(density(distrib),col=col_vector[x])
  }
}

plot.new()
legend("top",c("C=4", "C=2", "C=0.8","C=0.4","C=0.04"),ncol = 5, 
       col=c(col_vector[1],col_vector[2],col_vector[3],col_vector[4],col_vector[5]),
       #pch=c(tipo[1],tipo[2],tipo[3],NA,NA,tipo[5],tipo[5],tipo[5]),
       lty=c(1,1,1,1,1),#pt.cex=c(1,1,1,2,2,tamany[1],tamany[2],tamany[3]),
       lwd=c(3,3,3,3,3),bty = "n",x.intersp=0.05)

