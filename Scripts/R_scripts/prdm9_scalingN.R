setwd("~/Documents/Projects/PZIFE/Prdm9Nicolas/dataFromCluster/TR_2019_02_19_c/")

col_vector = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf','#377eb8','#4daf4a','#984ea3','#e41a1c','#ff7f00','#f781bf','#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf','#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf')
pdf("scalingN.pdf",height = 5, width=6)

m <- rbind(c(1,2),c(3,4))
layout(m,heights = c(2.5,2.5))

par(mar = c(5,5, 0.5, 0.5))
numsims=2
inicio="std_output_"
final=".txtnewalt"

xmin=90
xmax=51000
yemin=c(0,1.5,1,1)
yemax=c(1,3.5,4,150)

alphaArray=c(0,1,2,3,4)
rhoArray=c(1,2,3,4,5)
Carray=c(-1,0,1,2,3)
Uarray=c(1,2,3,4,5)
Narray=c(100,500,1000,5000,10000,50000)

#Varying N for set p2X3C1U3

m <- rbind(c(1,2),c(3,4))
layout(m,heights = c(2.5,2.5))

par(mar = c(5,5, 0.5, 0.5))
numsims=2
inicio="std_output_"
final=".txt"

yemin=c(0,1,1,1)
yemax=c(1,4,4.5,700)
equislabel=expression(paste("N",sep=""))
anotacion=expression(paste(alpha,"=0.01,  ",rho,"=0.001,  C=0.4,  U=0.004"))

count=1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="MeanRec",xlab=equislabel,log="x",sub=anotacion)
for(k in 1:length(Narray)){
  alpha=alphaArray[3]
  U=Uarray[3]
  rho=rhoArray[3]
  C=Carray[3]
  N=Narray[k]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",N,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    (label)
    a=read.table(label,header=TRUE)
    distrib[i]=a$meanrecomb
    points(N,distrib[i],col=col_vector[k])
  }
}

count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="MeanClassDiv",xlab=equislabel,log="x")
for(k in 1:length(Narray)){
  alpha=alphaArray[3]
  U=Uarray[3]
  rho=rhoArray[3]
  C=Carray[3]
  N=Narray[k]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",N,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    (label)
    a=read.table(label,header=TRUE)
    distrib[i]=a$cl_div
    points(N,distrib[i],col=col_vector[k])
  }
}

count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="Znf_div",xlab=equislabel,log="x")
for(k in 1:length(Narray)){
  alpha=alphaArray[3]
  U=Uarray[3]
  rho=rhoArray[3]
  C=Carray[3]
  N=Narray[k]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",N,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    (label)
    a=read.table(label,header=TRUE)
    distrib[i]=a$zf_div
    points(N,distrib[i],col=col_vector[k])
  }
}

count=count+1
plot(100,50000,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="4Ns",xlab=equislabel,log="x")
for(k in 1:length(Narray)){
  alpha=alphaArray[3]
  U=Uarray[3]
  rho=rhoArray[3]
  C=Carray[3]
  N=Narray[k]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",N,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    (label)
    a=read.table(label,header=TRUE)
    distrib[i]=a$X4Ns
    points(N,distrib[i],col=col_vector[k])
  }
}

#Varying N for set p2X2C0U2

m <- rbind(c(1,2),c(3,4))
layout(m,heights = c(2.5,2.5))

par(mar = c(5,5, 0.5, 0.5))
numsims=2
inicio="std_output_"
final=".txt"

yemin=c(0,1,1,1)
yemax=c(1,30,10,700)
equislabel=expression(paste("N",sep=""))
anotacion=expression(paste(alpha,"=0.01,  ",rho,"=0.01,  C=4,  U=0.04"))

count=1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="MeanRec",xlab=equislabel,log="x",sub=anotacion)
for(k in 1:length(Narray)){
  alpha=alphaArray[3]
  U=Uarray[2]
  rho=rhoArray[2]
  C=Carray[2]
  N=Narray[k]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",N,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    (label)
    a=read.table(label,header=TRUE)
    distrib[i]=a$meanrecomb
    points(N,distrib[i],col=col_vector[k])
  }
}

count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="MeanClassDiv",xlab=equislabel,log="x")
for(k in 1:length(Narray)){
  alpha=alphaArray[3]
  U=Uarray[2]
  rho=rhoArray[2]
  C=Carray[2]
  N=Narray[k]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",N,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    (label)
    a=read.table(label,header=TRUE)
    distrib[i]=a$cl_div
    points(N,distrib[i],col=col_vector[k])
  }
}

count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="Znf_div",xlab=equislabel,log="x")
for(k in 1:length(Narray)){
  alpha=alphaArray[3]
  U=Uarray[2]
  rho=rhoArray[2]
  C=Carray[2]
  N=Narray[k]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",N,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    (label)
    a=read.table(label,header=TRUE)
    distrib[i]=a$zf_div
    points(N,distrib[i],col=col_vector[k])
  }
}

count=count+1
plot(100,50000,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="4Ns",xlab=equislabel,log="x")
for(k in 1:length(Narray)){
  alpha=alphaArray[3]
  U=Uarray[2]
  rho=rhoArray[2]
  C=Carray[2]
  N=Narray[k]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",N,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    (label)
    a=read.table(label,header=TRUE)
    distrib[i]=a$X4Ns
    points(N,distrib[i],col=col_vector[k])
  }
}


#Varying N for set p2X3C1U3

m <- rbind(c(1,2),c(3,4))
layout(m,heights = c(2.5,2.5))

par(mar = c(5,5, 0.5, 0.5))
numsims=2
inicio="std_output_"
final=".txt"

yemin=c(0,1,1,1)
yemax=c(1,4,4.5,2000)
equislabel=expression(paste("N",sep=""))
anotacion=expression(paste(alpha,"=0.1,  ",rho,"=0.001,  C=0.4,  U=0.004"))

count=1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="MeanRec",xlab=equislabel,log="x",sub=anotacion)
for(k in 1:length(Narray)){
  alpha=alphaArray[2]
  U=Uarray[3]
  rho=rhoArray[3]
  C=Carray[3]
  N=Narray[k]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",N,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    (label)
    a=read.table(label,header=TRUE)
    distrib[i]=a$meanrecomb
    points(N,distrib[i],col=col_vector[k])
  }
}

count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="MeanClassDiv",xlab=equislabel,log="x")
for(k in 1:length(Narray)){
  alpha=alphaArray[2]
  U=Uarray[3]
  rho=rhoArray[3]
  C=Carray[3]
  N=Narray[k]
  
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",N,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    (label)
    a=read.table(label,header=TRUE)
    distrib[i]=a$cl_div
    points(N,distrib[i],col=col_vector[k])
  }
}

count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="Znf_div",xlab=equislabel,log="x")
for(k in 1:length(Narray)){
  alpha=alphaArray[2]
  U=Uarray[3]
  rho=rhoArray[3]
  C=Carray[3]
  N=Narray[k]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",N,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    (label)
    a=read.table(label,header=TRUE)
    distrib[i]=a$zf_div
    points(N,distrib[i],col=col_vector[k])
  }
}

count=count+1
plot(100,50000,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="4Ns",xlab=equislabel,log="x")
for(k in 1:length(Narray)){
  alpha=alphaArray[2]
  U=Uarray[3]
  rho=rhoArray[3]
  C=Carray[3]
  N=Narray[k]
  
    for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",N,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    (label)
    a=read.table(label,header=TRUE)
    distrib[i]=a$X4Ns
    points(N,distrib[i],col=col_vector[k])
  }
}


#Varying N for set p2X3C1U3

m <- rbind(c(1,2),c(3,4))
layout(m,heights = c(2.5,2.5))

par(mar = c(5,5, 0.5, 0.5))
numsims=2
inicio="std_output_"
final=".txt"

yemin=c(0,1,1,1)
yemax=c(1,40,10,2000)
equislabel=expression(paste("N",sep=""))
anotacion=expression(paste(alpha,"=0.1,  ",rho,"=0.01,  C=4,  U=0.04"))

count=1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="MeanRec",xlab=equislabel,log="x",sub=anotacion)
for(k in 1:length(Narray)){
  alpha=alphaArray[2]
  U=Uarray[2]
  rho=rhoArray[2]
  C=Carray[2]
  N=Narray[k]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",N,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    (label)
    a=read.table(label,header=TRUE)
    distrib[i]=a$meanrecomb
    points(N,distrib[i],col=col_vector[k])
  }
}

count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="MeanClassDiv",xlab=equislabel,log="x")
for(k in 1:length(Narray)){
  alpha=alphaArray[2]
  U=Uarray[2]
  rho=rhoArray[2]
  C=Carray[2]
  N=Narray[k]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",N,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    (label)
    a=read.table(label,header=TRUE)
    distrib[i]=a$cl_div
    points(N,distrib[i],col=col_vector[k])
  }
}

count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="Znf_div",xlab=equislabel,log="x")
for(k in 1:length(Narray)){
  alpha=alphaArray[2]
  U=Uarray[2]
  rho=rhoArray[2]
  C=Carray[2]
  N=Narray[k]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",N,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    (label)
    a=read.table(label,header=TRUE)
    distrib[i]=a$zf_div
    points(N,distrib[i],col=col_vector[k])
  }
}

count=count+1
plot(100,50000,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="4Ns",xlab=equislabel,log="x")
for(k in 1:length(Narray)){
  alpha=alphaArray[2]
  U=Uarray[2]
  rho=rhoArray[2]
  C=Carray[2]
  N=Narray[k]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",N,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    (label)
    a=read.table(label,header=TRUE)
    distrib[i]=a$X4Ns
    points(N,distrib[i],col=col_vector[k])
  }
}



#Varying N for set p2X3C1U3

Narray=c(100,500,1000,5000,10000)
m <- rbind(c(1,2),c(3,4))
layout(m,heights = c(2.5,2.5))

par(mar = c(5,5, 0.5, 0.5))
numsims=2
inicio="std_output_"
final=".txt"

yemin=c(0,1,1,1)
yemax=c(1,4,4.5,50)
equislabel=expression(paste("N",sep=""))
anotacion=expression(paste(alpha,"=0.001,  ",rho,"=0.001,  C=0.4,  U=0.004"))

count=1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="MeanRec",xlab=equislabel,log="x",sub=anotacion)
for(k in 1:length(Narray)){
  alpha=alphaArray[4]
  U=Uarray[3]
  rho=rhoArray[3]
  C=Carray[3]
  N=Narray[k]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",N,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    (label)
    a=read.table(label,header=TRUE)
    distrib[i]=a$meanrecomb
    points(N,distrib[i],col=col_vector[k])
  }
}

count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="MeanClassDiv",xlab=equislabel,log="x")
for(k in 1:length(Narray)){
  alpha=alphaArray[4]
  U=Uarray[3]
  rho=rhoArray[3]
  C=Carray[3]
  N=Narray[k]
  
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",N,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    (label)
    a=read.table(label,header=TRUE)
    distrib[i]=a$cl_div
    points(N,distrib[i],col=col_vector[k])
  }
}

count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="Znf_div",xlab=equislabel,log="x")
for(k in 1:length(Narray)){
  alpha=alphaArray[4]
  U=Uarray[3]
  rho=rhoArray[3]
  C=Carray[3]
  N=Narray[k]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",N,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    (label)
    a=read.table(label,header=TRUE)
    distrib[i]=a$zf_div
    points(N,distrib[i],col=col_vector[k])
  }
}

count=count+1
plot(100,50000,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="4Ns",xlab=equislabel,log="x")
for(k in 1:length(Narray)){
  alpha=alphaArray[4]
  U=Uarray[3]
  rho=rhoArray[3]
  C=Carray[3]
  N=Narray[k]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",N,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    (label)
    a=read.table(label,header=TRUE)
    distrib[i]=a$X4Ns
    points(N,distrib[i],col=col_vector[k])
  }
}


#Varying N for set p2X3C1U3

m <- rbind(c(1,2),c(3,4))
layout(m,heights = c(2.5,2.5))

par(mar = c(5,5, 0.5, 0.5))
numsims=2
inicio="std_output_"
final=".txt"

yemin=c(0,1,1,1)
yemax=c(1,40,6,50)
equislabel=expression(paste("N",sep=""))
anotacion=expression(paste(alpha,"=0.001,  ",rho,"=0.01,  C=4,  U=0.04"))

count=1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="MeanRec",xlab=equislabel,log="x",sub=anotacion)
for(k in 1:length(Narray)){
  alpha=alphaArray[4]
  U=Uarray[2]
  rho=rhoArray[2]
  C=Carray[2]
  N=Narray[k]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",N,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    (label)
    a=read.table(label,header=TRUE)
    distrib[i]=a$meanrecomb
    points(N,distrib[i],col=col_vector[k])
  }
}

count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="MeanClassDiv",xlab=equislabel,log="x")
for(k in 1:length(Narray)){
  alpha=alphaArray[4]
  U=Uarray[2]
  rho=rhoArray[2]
  C=Carray[2]
  N=Narray[k]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",N,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    (label)
    a=read.table(label,header=TRUE)
    distrib[i]=a$cl_div
    points(N,distrib[i],col=col_vector[k])
  }
}

count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="Znf_div",xlab=equislabel,log="x")
for(k in 1:length(Narray)){
  alpha=alphaArray[4]
  U=Uarray[2]
  rho=rhoArray[2]
  C=Carray[2]
  N=Narray[k]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",N,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    (label)
    a=read.table(label,header=TRUE)
    distrib[i]=a$zf_div
    points(N,distrib[i],col=col_vector[k])
  }
}

count=count+1
plot(100,50000,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="4Ns",xlab=equislabel,log="x")
for(k in 1:length(Narray)){
  alpha=alphaArray[4]
  U=Uarray[2]
  rho=rhoArray[2]
  C=Carray[2]
  N=Narray[k]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",N,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    (label)
    a=read.table(label,header=TRUE)
    distrib[i]=a$X4Ns
    points(N,distrib[i],col=col_vector[k])
  }
}


#Varying N for set X3C1U3 several alpha

m <- rbind(c(1,2),c(3,4))
layout(m,heights = c(2.5,2.5))

par(mar = c(5,5, 0.5, 0.5))
numsims=2
inicio="std_output_"
final=".txt"

alphaArray=c(1,2,3)
yemin=c(0,1,1,1)
yemax=c(1,4,4.5,700)
equislabel=expression(paste("N",sep=""))
anotacion=expression(paste(rho,"=0.001,  C=0.4,  U=0.004"))

count=1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="MeanRec",xlab=equislabel,log="x",sub=anotacion)
for(j in 1:length(alphaArray)){
for(k in 1:length(Narray)){
  
  alpha=alphaArray[j]
  U=Uarray[3]
  rho=rhoArray[3]
  C=Carray[3]
  N=Narray[k]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",N,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    (label)
    a=read.table(label,header=TRUE)
    distrib[i]=a$meanrecomb
    points(N,distrib[i],col=col_vector[j+1])
  }
}
  
}

count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="MeanClassDiv",xlab=equislabel,log="x")
for(j in 1:length(alphaArray)){
for(k in 1:length(Narray)){
  alpha=alphaArray[j]
  U=Uarray[3]
  rho=rhoArray[3]
  C=Carray[3]
  N=Narray[k]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",N,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    (label)
    a=read.table(label,header=TRUE)
    distrib[i]=a$cl_div
    points(N,distrib[i],col=col_vector[j+1])
  }
}
}
count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="Znf_div",xlab=equislabel,log="x")
for(j in 1:length(alphaArray)){
for(k in 1:length(Narray)){
  alpha=alphaArray[j]
  U=Uarray[3]
  rho=rhoArray[3]
  C=Carray[3]
  N=Narray[k]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",N,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    (label)
    a=read.table(label,header=TRUE)
    distrib[i]=a$zf_div
    points(N,distrib[i],col=col_vector[j+1])
  }
}
}
count=count+1
plot(100,50000,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="4Ns",xlab=equislabel,log="x")
for(j in 1:length(alphaArray)){
for(k in 1:length(Narray)){
  alpha=alphaArray[j]
  U=Uarray[3]
  rho=rhoArray[3]
  C=Carray[3]
  N=Narray[k]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",N,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    (label)
    a=read.table(label,header=TRUE)
    distrib[i]=a$X4Ns
    points(N,distrib[i],col=col_vector[j+1])
  }
}

}


#Varying N for set p2X3C1U3

m <- rbind(c(1,2),c(3,4))
layout(m,heights = c(2.5,2.5))

par(mar = c(5,5, 0.5, 0.5))
numsims=2
inicio="std_output_"
final=".txt"

yemin=c(0,1,1,1)
yemax=c(1,40,10,500)
equislabel=expression(paste("N",sep=""))
anotacion=expression(paste(rho,"=0.01,  C=4,  U=0.04"))

count=1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="MeanRec",xlab=equislabel,log="x",sub=anotacion)
for(j in 1:length(alphaArray)){
for(k in 1:length(Narray)){
  alpha=alphaArray[j]
  U=Uarray[2]
  rho=rhoArray[2]
  C=Carray[2]
  N=Narray[k]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",N,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    (label)
    a=read.table(label,header=TRUE)
    distrib[i]=a$meanrecomb
    points(N,distrib[i],col=col_vector[j+1])
  }
}
}
count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="MeanClassDiv",xlab=equislabel,log="x")
for(j in 1:length(alphaArray)){
for(k in 1:length(Narray)){
  alpha=alphaArray[j]
  U=Uarray[2]
  rho=rhoArray[2]
  C=Carray[2]
  N=Narray[k]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",N,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    (label)
    a=read.table(label,header=TRUE)
    distrib[i]=a$cl_div
    points(N,distrib[i],col=col_vector[j+1])
  }
}
}
count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="Znf_div",xlab=equislabel,log="x")
for(j in 1:length(alphaArray)){
for(k in 1:length(Narray)){
  alpha=alphaArray[j]
  U=Uarray[2]
  rho=rhoArray[2]
  C=Carray[2]
  N=Narray[k]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",N,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    (label)
    a=read.table(label,header=TRUE)
    distrib[i]=a$zf_div
    points(N,distrib[i],col=col_vector[j+1])
  }
}
}
count=count+1
plot(100,50000,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="4Ns",xlab=equislabel,log="x")
for(j in 1:length(alphaArray)){
for(k in 1:length(Narray)){
  alpha=alphaArray[j]
  U=Uarray[2]
  rho=rhoArray[2]
  C=Carray[2]
  N=Narray[k]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",N,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    (label)
    a=read.table(label,header=TRUE)
    distrib[i]=a$X4Ns
    points(N,distrib[i],col=col_vector[j+1])
  }
}
}

dev.off()
