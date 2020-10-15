setwd("~/Documents/Projects/PZIFE/Prdm9Nicolas/dataFromCluster/TR_2019_02_19_b/")

col_vector = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf','#377eb8','#4daf4a','#984ea3','#e41a1c','#ff7f00','#f781bf','#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf','#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf')
pdf("scaling.pdf",height = 5, width=6)

m <- rbind(c(1,2),c(3,4))
layout(m,heights = c(2.5,2.5))

par(mar = c(5,5, 0.5, 0.5))
numsims=2
inicio="std_output_"
final=".txtnewalt"

xmin=-4.2
xmax=0.2
yemin=c(0,1.5,1,1)
yemax=c(1,3.5,4,150)

alphaArray=c(0,1,2,3,4)
rhoArray=c(1,2,3,4,5)
Carray=c(-1,0,1,2,3)
Uarray=c(1,2,3,4,5)
Narray=c(100,500,1000,5000,10000,50000)

fixed1<-rhoArray
fixed2<-Carray
fixed3<-Uarray
variable<-alphaArray

equislabel="log alpha"

count=1
distrib=c()
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="MeanRec",xlab=equislabel)
for(k in 1:length(variable)){
    #alpha=alphaArray[k]
    rho=fixed1[3]
    C=fixed2[2]
    U=fixed3[3]
    alpha=variable[k]
      
    for(i in 1:numsims){
      label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
      (label)
      a=read.table(label,header=TRUE)
      distrib[i]=a$meanrecomb
     points(-k+1,distrib[i],col=col_vector[k])
    }
}

count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="MeanClassDiv",xlab=equislabel,log="y")
  for(k in 1:length(rhoArray)){
    alpha=alphaArray[k]
    rho=rhoArray[3]
    C=Carray[3]
    U=Uarray[3]
    
    for(i in 1:numsims){
      label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
      a=read.table(label,header=TRUE)
      distrib[i]=a$cl_div
     points(-k+1,distrib[i],col=col_vector[k])
    }
}

count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="Znf_div",xlab=equislabel)
for(k in 1:length(alphaArray)){
    alpha=alphaArray[k]
    rho=rhoArray[3]
    C=Carray[3]
    U=Uarray[3]
    
    for(i in 1:numsims){
      label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
      a=read.table(label,header=TRUE)
      distrib[i]=a$zf_div
      points(-k+1,distrib[i],col=col_vector[k])
    }
}


count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="4Ns",xlab=equislabel,log="y")
  for(k in 1:length(alphaArray)){
    alpha=alphaArray[k]
    rho=rhoArray[3]
    C=Carray[3]
    U=Uarray[3]
    
    for(i in 1:numsims){
      label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
      a=read.table(label,header=TRUE)
      distrib[i]=a$X4Ns
      points(-k+1,distrib[i],col=col_vector[k])
    }
}

#Varying rho

m <- rbind(c(1,2),c(3,4))
layout(m,heights = c(2.5,2.5))

par(mar = c(5,5, 0.5, 0.5))
numsims=2
inicio="std_output_"
final=".txtnewalt"

yemin=c(0,1.5,1.5,1)
yemax=c(1,5,4,150)

fixed1<-alphaArray
fixed2<-Carray
fixed3<-Uarray
variable<-rhoArray
equislabel="log rho + 1"

count=1
distrib=c()
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="MeanRec",xlab=equislabel)
for(k in 1:length(variable)){
  #alpha=alphaArray[k]
  rho=variable[k]
  C=fixed2[2]
  U=fixed3[3]
  alpha=fixed1[3]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    (label)
    a=read.table(label,header=TRUE)
    distrib[i]=a$meanrecomb
    points(-k+1,distrib[i],col=col_vector[k])
  }
}

count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="MeanClassDiv",xlab=equislabel,log="y")
for(k in 1:length(rhoArray)){
  alpha=alphaArray[3]
  rho=rhoArray[k]
  C=Carray[3]
  U=Uarray[3]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    a=read.table(label,header=TRUE)
    distrib[i]=a$cl_div
    points(-k+1,distrib[i],col=col_vector[k])
  }
}

count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="Znf_div",xlab=equislabel)
for(k in 1:length(alphaArray)){
  alpha=alphaArray[3]
  rho=rhoArray[k]
  C=Carray[3]
  U=Uarray[3]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    a=read.table(label,header=TRUE)
    distrib[i]=a$zf_div
    points(-k+1,distrib[i],col=col_vector[k])
  }
}

count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="4Ns",xlab=equislabel,log="y")
for(k in 1:length(alphaArray)){
  alpha=alphaArray[3]
  rho=rhoArray[k]
  C=Carray[3]
  U=Uarray[3]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    a=read.table(label,header=TRUE)
    distrib[i]=a$X4Ns
    points(-k+1,distrib[i],col=col_vector[k])
    
  }
}

#Varying C

m <- rbind(c(1,2),c(3,4))
layout(m,heights = c(2.5,2.5))

par(mar = c(5,5, 0.5, 0.5))
numsims=2
inicio="std_output_"
final=".txtnewalt"

yemin=c(0,1,1,1)
yemax=c(1,6,10,60)

fixed1<-alphaArray
fixed2<-rhoArray
fixed3<-Uarray
variable<-Carray
equislabel="log C/4 - 1"

count=1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="MeanRec",xlab=equislabel)
for(k in 1:length(variable)){
  #alpha=alphaArray[k]
  C=variable[k]
  rho=fixed2[3]
  U=fixed3[3]
  alpha=fixed1[3]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    (label)
    a=read.table(label,header=TRUE)
    distrib[i]=a$meanrecomb
    points(-k+1,distrib[i],col=col_vector[k])
  }
}
 
count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="MeanClassDiv",xlab=equislabel,log="y")
for(k in 1:length(Carray)){
  alpha=alphaArray[3]
  rho=rhoArray[3]
  C=Carray[k]
  U=Uarray[3]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    a=read.table(label,header=TRUE)
    distrib[i]=a$cl_div
    points(-k+1,distrib[i],col=col_vector[k])
  }
}

count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="Znf_div",xlab=equislabel)
for(k in 1:length(Carray)){
  alpha=alphaArray[3]
  rho=rhoArray[3]
  C=Carray[k]
  U=Uarray[3]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    a=read.table(label,header=TRUE)
    distrib[i]=a$zf_div
    points(-k+1,distrib[i],col=col_vector[k])
  }
}

count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="4Ns",xlab=equislabel,log="y")
for(k in 1:length(Carray)){
  alpha=alphaArray[3]
  rho=rhoArray[3]
  C=Carray[k]
  U=Uarray[3]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    a=read.table(label,header=TRUE)
    distrib[i]=a$X4Ns
    points(-k+1,distrib[i],col=col_vector[k])
  }
}

#Varying U

m <- rbind(c(1,2),c(3,4))
layout(m,heights = c(2.5,2.5))

par(mar = c(5,5, 0.5, 0.5))
numsims=2
inicio="std_output_"
final=".txtnewalt"

yemin=c(0,1,1,1)
yemax=c(1,20,20,200)

fixed1<-alphaArray
fixed2<-rhoArray
fixed3<-Carray
variable<-Uarray
equislabel="log U/4 + 1"

count=1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="MeanRec",xlab=equislabel)
for(k in 1:length(variable)){
  #alpha=alphaArray[k]
  U=variable[k]
  rho=fixed2[3]
  C=fixed3[2]
  alpha=fixed1[3]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    (label)
    a=read.table(label,header=TRUE)
    distrib[i]=a$meanrecomb
    points(-k+1,distrib[i],col=col_vector[k])
  }
}
 
count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="MeanClassDiv",xlab=equislabel,log="y")
for(k in 1:length(Uarray)){
  alpha=alphaArray[3]
  rho=rhoArray[3]
  C=Carray[3]
  U=Uarray[k]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    a=read.table(label,header=TRUE)
    distrib[i]=a$cl_div
    points(-k+1,distrib[i],col=col_vector[k])
  }
}

count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="Znf_div",xlab=equislabel)
for(k in 1:length(Uarray)){
  alpha=alphaArray[3]
  rho=rhoArray[3]
  C=Carray[3]
  U=Uarray[k]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    a=read.table(label,header=TRUE)
    distrib[i]=a$zf_div
    points(-k+1,distrib[i],col=col_vector[k])
  }
}

count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="4Ns",xlab=equislabel,log="y")
for(k in 1:length(Uarray)){
  alpha=alphaArray[3]
  rho=rhoArray[3]
  C=Carray[3]
  U=Uarray[k]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    a=read.table(label,header=TRUE)
    distrib[i]=a$X4Ns
    points(-k+1,distrib[i],col=col_vector[k])
    
  }
}


#Varying rho and alpha

m <- rbind(c(1,2),c(3,4))
layout(m,heights = c(2.5,2.5))

par(mar = c(5,5, 0.5, 0.5))
numsims=2
inicio="std_output_"
final=".txtnewalt"

yemin=c(0,1,1,1)
yemax=c(1,4,4.5,1500)
equislabel=expression(paste("log(",alpha,") & log(",rho,")+1",sep=""))

count=1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="MeanRec",xlab=equislabel)
for(k in 1:length(alphaArray)){
  alpha=alphaArray[k]
  U=Uarray[3]
  rho=rhoArray[k]
  C=Carray[3]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    (label)
    a=read.table(label,header=TRUE)
    distrib[i]=a$meanrecomb
    points(-k+1,distrib[i],col=col_vector[k])
  }
}
 
count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="MeanClassDiv",xlab=equislabel,log="y")
for(k in 1:length(alphaArray)){
  alpha=alphaArray[k]
  rho=rhoArray[k]
  C=Carray[3]
  U=Uarray[3]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    a=read.table(label,header=TRUE)
    distrib[i]=a$cl_div
    points(-k+1,distrib[i],col=col_vector[k])
  }
}

count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="Znf_div",xlab=equislabel)
for(k in 1:length(alphaArray)){
  alpha=alphaArray[k]
  rho=rhoArray[k]
  C=Carray[3]
  U=Uarray[3]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    a=read.table(label,header=TRUE)
    distrib[i]=a$zf_div
    points(-k+1,distrib[i],col=col_vector[k])
  }
}

count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="4Ns",xlab=equislabel,log="y")
for(k in 1:length(alphaArray)){
  alpha=alphaArray[k]
  rho=rhoArray[k]
  C=Carray[3]
  U=Uarray[3]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    a=read.table(label,header=TRUE)
    distrib[i]=a$X4Ns
    points(-k+1,distrib[i],col=col_vector[k])
    
  }
}

#Varying C and U

m <- rbind(c(1,2),c(3,4))
layout(m,heights = c(2.5,2.5))

par(mar = c(5,5, 0.5, 0.5))
numsims=2
inicio="std_output_"
final=".txtnewalt"

yemin=c(0,1,1,1)
yemax=c(1,13.5,4.5,700)
equislabel="log(C/4)-1 & log(U/4)+1"

count=1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="MeanRec",xlab=equislabel)
for(k in 1:length(alphaArray)){
  alpha=alphaArray[3]
  U=Uarray[k]
  rho=rhoArray[3]
  C=Carray[k]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    (label)
    a=read.table(label,header=TRUE)
    distrib[i]=a$meanrecomb
    points(-k+1,distrib[i],col=col_vector[k])
  }
}

count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="MeanClassDiv",xlab=equislabel,log="y")
for(k in 1:length(alphaArray)){
  alpha=alphaArray[3]
  U=Uarray[k]
  rho=rhoArray[3]
  C=Carray[k]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    a=read.table(label,header=TRUE)
    distrib[i]=a$cl_div
    points(-k+1,distrib[i],col=col_vector[k])
  }
}

count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="Znf_div",xlab=equislabel)
for(k in 1:length(alphaArray)){
  alpha=alphaArray[3]
  U=Uarray[k]
  rho=rhoArray[3]
  C=Carray[k]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    a=read.table(label,header=TRUE)
    distrib[i]=a$zf_div
    points(-k+1,distrib[i],col=col_vector[k])
  }
}

count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="4Ns",xlab=equislabel,log="y")
for(k in 1:length(alphaArray)){
  alpha=alphaArray[3]
  U=Uarray[k]
  rho=rhoArray[3]
  C=Carray[k]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    a=read.table(label,header=TRUE)
    distrib[i]=a$X4Ns
    points(-k+1,distrib[i],col=col_vector[k])
    
  }
}

#Varying C and U and rho

m <- rbind(c(1,2),c(3,4))
layout(m,heights = c(2.5,2.5))

par(mar = c(5,5, 0.5, 0.5))
numsims=2
inicio="std_output_"
final=".txtnewalt"


alphaArray=c(0,1,2,3,4)
rhoArray=c(1,2,3,4,5)
Carray=c(0,0,1,2,3)
Uarray=c(1,2,3,4,5)

yemin=c(0,1,1,1)
yemax=c(1,40,16.5,220)
equislabel=expression(paste("log(C/4)-1 & log(U/4)+1 & log(",rho,")+1",sep=""))

count=1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="MeanRec",xlab=equislabel)
for(j in 1:length(alphaArray)){
for(k in 1:length(alphaArray)){
  alpha=alphaArray[j]
  U=Uarray[k]
  rho=rhoArray[k]
  C=Carray[k]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    (label)
    a=read.table(label,header=TRUE)
    distrib[i]=a$meanrecomb
    points(-k+1,distrib[i],col=col_vector[j])
  }
}
}

count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="MeanClassDiv",xlab=equislabel,log="y")
for(j in 1:length(alphaArray)){
for(k in 1:length(alphaArray)){
  alpha=alphaArray[j]
  U=Uarray[k]
  rho=rhoArray[k]
  C=Carray[k]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    a=read.table(label,header=TRUE)
    distrib[i]=a$cl_div
    points(-k+1,distrib[i],col=col_vector[j])
  }
abline(a=20, b=1)
}
}
count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="Znf_div",xlab=equislabel)
for(j in 1:length(alphaArray)){
  for(k in 1:length(alphaArray)){
    alpha=alphaArray[j]
    U=Uarray[k]
    rho=rhoArray[k]
    C=Carray[k]
    
    for(i in 1:numsims){
      label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
      a=read.table(label,header=TRUE)
      distrib[i]=a$zf_div
      points(-k+1,distrib[i],col=col_vector[j])
    }
  }
}

count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="4Ns",xlab=equislabel,log="y")
for(j in 1:length(alphaArray)){
  for(k in 1:length(alphaArray)){
    alpha=alphaArray[j]
    U=Uarray[k]
    rho=rhoArray[k]
    C=Carray[k]
    
    for(i in 1:numsims){
      label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
      a=read.table(label,header=TRUE)
      distrib[i]=a$X4Ns
      points(-k+1,distrib[i],col=col_vector[j])
    }
  }
}





dev.off()
