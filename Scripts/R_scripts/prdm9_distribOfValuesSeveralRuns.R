setwd("~/Documents/Projects/PZIFE/Prdm9Nicolas/dataFromCluster/TR_2019_02_14/")

col_vector = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf','#377eb8','#4daf4a','#984ea3','#e41a1c','#ff7f00','#f781bf','#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf','#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf')


m <- rbind(c(1,2),c(3,4),c(5,6),c(7,8))
layout(m,heights = c(2.5,2.5,2.5,2.5))

par(mar = c(5,5, 0.5, 0.5))
Carray=c(0,1,2)
numsims=20
inicio="std_output_"
final=".txt"

distrib=c()
for(x in 1:length(Carray))
{
  #plot(-100,-100,xlim=c(0,21),ylim=c(0,1),xlab="MeanRec")
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_p2_X3_DNull_C",Carray[x],"_U3",final,sep="")
    a=read.table(label,header=TRUE)
    distrib[i]=a$meanrec
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
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_p2_X3_DNull_C",Carray[x],"_U3",final,sep="")
    a=read.table(label,header=TRUE)
    distrib[i]=a$allele_div
    #points(i,a$meandiv)
  }
  if(x==1){
    plot(density(distrib),xlab="MeanDiv",col=col_vector[x],xlim=c(0,20),main="")
  }else{
    lines(density(distrib),col=col_vector[x])
  }
}

for(x in 1:length(Carray))
{
  #plot(-100,-100,xlim=c(0,21),ylim=c(0,1),xlab="MeanRec")
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_p2_X3_DNull_C",Carray[x],"_U3",final,sep="")
    a=read.table(label,header=TRUE)
    distrib[i]=a$class_div
    #points(i,a$meanclassdiv)
  }
  if(x==1){
    plot(density(distrib),xlab="MeanClassDiv",col=col_vector[x],xlim=c(0,6),main="")
  }else{
    lines(density(distrib),col=col_vector[x])
  }
}

for(x in 1:length(Carray))
{
  #plot(-100,-100,xlim=c(0,21),ylim=c(0,1),xlab="MeanRec")
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_p2_X3_DNull_C",Carray[x],"_U3",final,sep="")
    a=read.table(label,header=TRUE)
    distrib[i]=a$zf_divpop
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
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_p2_X3_DNull_C",Carray[x],"_U3",final,sep="")
    a=read.table(label,header=TRUE)
    distrib[i]=a$class_tau
    #points(i,a$tau)
  }
  if(x==1){
    plot(density(distrib),xlab="Tau",col=col_vector[x],xlim=c(0,1),main="")
  }else{
    lines(density(distrib),col=col_vector[x])
  }
}

# for(x in 1:length(Carray))
# {
#   #plot(-100,-100,xlim=c(0,21),ylim=c(0,1),xlab="MeanRec")
#   for(i in 1:numsims){
#     label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_p2_X3_DNull_C",Carray[x],"_U3",final,sep="")
#     a=read.table(label,header=TRUE)
#     distrib[i]=a$taudiv
#     points(i,a$taudiv)
#   }
#   if(x==1){
#     plot(density(distrib),xlab="TauDiv",col=col_vector[x],xlim=c(100,1000),main="")
#   }else{
#     lines(density(distrib),col=col_vector[x])
#   }
# }

for(x in 1:length(Carray))
{
  #plot(-100,-100,xlim=c(0,21),ylim=c(0,1),xlab="MeanRec")
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_p2_X3_DNull_C",Carray[x],"_U3",final,sep="")
    a=read.table(label,header=TRUE)
    distrib[i]=a$zf_tau
  #  points(i,a$tau2)
  }
  if(x==1){
    plot(density(distrib),xlab="ZnfTau",col=col_vector[x],xlim=c(0,10),main="")
  }else{
    lines(density(distrib),col=col_vector[x])
  }
}

distribDivZnf=c()
distribDivAC=c()
for(x in 1:length(Carray))
{
  #plot(-100,-100,xlim=c(0,21),ylim=c(0,1),xlab="MeanRec")
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_p2_X3_DNull_C",Carray[x],"_U3",final,sep="")
    a=read.table(label,header=TRUE)
    distribDivZnf[i]=a$zf_divpop
    distribDivAC[i]=a$class_div
  
  }
  if(x==1){
    plot(distribDivZnf,distribDivAC,xlab="ZnfDiv",col=col_vector[x],xlim=c(0,10),ylim=c(0,5),main="")
  }else{
    points(distribDivZnf,distribDivAC,col=col_vector[x])
    #  points(i,a$tau2)
  }
}

# 
# for(x in 1:length(Carray))
# {
#   #plot(-100,-100,xlim=c(0,21),ylim=c(0,1),xlab="MeanRec")
#   for(i in 1:numsims){
#     label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_p2_X3_DNull_C",Carray[x],"_U3",final,sep="")
#     a=read.table(label,header=TRUE)
#     distrib[i]=a$tau2div
#     points(i,a$tau2div)
#   }
#   if(x==1){
#     plot(density(distrib),xlab="Tau2Div",col=col_vector[x],xlim=c(110,120),main="")
#   }else{
#     lines(density(distrib),col=col_vector[x])
#   }
# }