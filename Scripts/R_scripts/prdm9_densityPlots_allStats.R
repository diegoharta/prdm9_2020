setwd("~/Documents/Projects/PZIFE/Prdm9Nicolas/dataFromCluster/TR_2019_02_11/")

col_vector = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf','#377eb8','#4daf4a','#984ea3','#e41a1c','#ff7f00','#f781bf','#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf','#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf')


m <- rbind(c(1,2),c(3,4),c(5,6),c(7,8))
layout(m,heights = c(2.5,2.5,2.5,2.5))

par(mar = c(5,5, 0.5, 0.5))
Carray=c(0)
numsims=20

distrib=c()
for(x in 1:length(Carray))
{
  #plot(-100,-100,xlim=c(0,21),ylim=c(0,1),xlab="MeanRec")
  for(i in 1:numsims){
    label=paste("prueba1_cluster_prdm9_N5000_t1M_r",i,"_p2_X3_C1_U3.summary",sep="")
    a=read.table(label,header=TRUE)
    distrib[i]=a$meanrec
    points(i,a$meanrec)
  }
  if(x==1){
    plot(density(distrib),xlab="MeanRec",col=col_vector[x])
  }else{
    lines(density(distrib),col=col_vector[x])
  }
}

for(x in 1:length(Carray))
{
  #plot(-100,-100,xlim=c(0,21),ylim=c(0,1),xlab="MeanRec")
  for(i in 1:numsims){
    label=paste("prueba1_cluster_prdm9_N5000_t1M_r",i,"_p2_X3_C1_U3.summary",sep="")
    a=read.table(label,header=TRUE)
    distrib[i]=a$meandiv
    points(i,a$meandiv)
  }
  if(x==1){
    plot(density(distrib),xlab="MeanDiv",col=col_vector[x])
  }else{
    lines(density(distrib),col=col_vector[x])
  }
}

for(x in 1:length(Carray))
{
  #plot(-100,-100,xlim=c(0,21),ylim=c(0,1),xlab="MeanRec")
  for(i in 1:numsims){
    label=paste("prueba1_cluster_prdm9_N5000_t1M_r",i,"_p2_X3_C1_U3.summary",sep="")
    a=read.table(label,header=TRUE)
    distrib[i]=a$meanclassdiv
    points(i,a$meanclassdiv)
  }
  if(x==1){
    plot(density(distrib),xlab="MeanClassDiv",col=col_vector[x])
  }else{
    lines(density(distrib),col=col_vector[x])
  }
}

for(x in 1:length(Carray))
{
  #plot(-100,-100,xlim=c(0,21),ylim=c(0,1),xlab="MeanRec")
  for(i in 1:numsims){
    label=paste("prueba1_cluster_prdm9_N5000_t1M_r",i,"_p2_X3_C1_U3.summary",sep="")
    a=read.table(label,header=TRUE)
    distrib[i]=a$meanzfdiv
    points(i,a$meanzfdiv)
  }
  if(x==1){
    plot(density(distrib),xlab="MeanZnfDiv",col=col_vector[x])
  }else{
    lines(density(distrib),col=col_vector[x])
  }
}

for(x in 1:length(Carray))
{
  #plot(-100,-100,xlim=c(0,21),ylim=c(0,1),xlab="MeanRec")
  for(i in 1:numsims){
    label=paste("prueba1_cluster_prdm9_N5000_t1M_r",i,"_p2_X3_C1_U3.summary",sep="")
    a=read.table(label,header=TRUE)
    distrib[i]=a$tau
    points(i,a$tau)
  }
  if(x==1){
    plot(density(distrib),xlab="Tau",col=col_vector[x])
  }else{
    lines(density(distrib),col=col_vector[x])
  }
}

for(x in 1:length(Carray))
{
  #plot(-100,-100,xlim=c(0,21),ylim=c(0,1),xlab="MeanRec")
  for(i in 1:numsims){
    label=paste("prueba1_cluster_prdm9_N5000_t1M_r",i,"_p2_X3_C1_U3.summary",sep="")
    a=read.table(label,header=TRUE)
    distrib[i]=a$taudiv
    points(i,a$taudiv)
  }
  if(x==1){
    plot(density(distrib),xlab="TauDiv",col=col_vector[x])
  }else{
    lines(density(distrib),col=col_vector[x])
  }
}

for(x in 1:length(Carray))
{
  #plot(-100,-100,xlim=c(0,21),ylim=c(0,1),xlab="MeanRec")
  for(i in 1:numsims){
    label=paste("prueba1_cluster_prdm9_N5000_t1M_r",i,"_p2_X3_C1_U3.summary",sep="")
    a=read.table(label,header=TRUE)
    distrib[i]=a$tau2
    points(i,a$tau2)
  }
  if(x==1){
    plot(density(distrib),xlab="MeanTau2",col=col_vector[x])
  }else{
    lines(density(distrib),col=col_vector[x])
  }
}

for(x in 1:length(Carray))
{
  #plot(-100,-100,xlim=c(0,21),ylim=c(0,1),xlab="MeanRec")
  for(i in 1:numsims){
    label=paste("prueba1_cluster_prdm9_N5000_t1M_r",i,"_p2_X3_C1_U3.summary",sep="")
    a=read.table(label,header=TRUE)
    distrib[i]=a$tau2div
    points(i,a$tau2div)
  }
  if(x==1){
    plot(density(distrib),xlab="Tau2Div",col=col_vector[x])
  }else{
    lines(density(distrib),col=col_vector[x])
  }
}