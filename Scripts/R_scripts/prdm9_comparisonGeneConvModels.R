setwd("~/Documents/Projects/PZIFE/Prdm9Nicolas/dataFromCluster/TR_2019_02_27/")

col_vector = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf','#377eb8','#4daf4a','#984ea3','#e41a1c','#ff7f00','#f781bf','#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf','#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf')
#pdf("scaling.pdf",height = 5, width=6)

xmin=-4.2
xmax=0.2
yemin=c(0,1,1,1)
yemax=c(1,20,12,150)

alphaArray=c(2)
rhoArray=c(2,3)
Carray=c(0,1,2)
Uarray=c(2,3)
Narray=c(100,500,1000,5000,10000,50000)

equislabel="log alpha"

#Varying C and U and rho

m <- rbind(c(1,2),c(3,4))
layout(m,heights = c(2.5,2.5))

par(mar = c(5,5, 0.5, 0.5))
numsims=2
inicio="std_output_prueba2_cluster_prdm9_N1000_g0.25_r"
final=".txt"

equislabel=expression(paste("log(C/4)-1 & log(U/4)+1 & log(",rho,")+1",sep=""))

count=1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="MeanRec",xlab=equislabel)
for(j in 1:length(Carray)){
  for(k in 1:length(Uarray)){
    for(l in 1:length(rhoArray)){
    alpha=alphaArray[1]
    U=Uarray[k]
    rho=rhoArray[l]
    C=Carray[j]
    
    for(i in 1:numsims){
      label=paste(inicio,i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
      (label)
      a=read.table(label,header=TRUE)
      distrib[i]=a$meanrecomb
      points(-(j*5+k*2+l)/4+1.5,distrib[i],col=col_vector[k],pch=j-1)
    }
  }
}
}

inicio="std_output_prueba1_cluster_prdm9_N1000_t1M_g0.33_r"
for(j in 1:length(Carray)){
  for(k in 1:length(Uarray)){
    for(l in 1:length(rhoArray)){
      alpha=alphaArray[1]
      U=Uarray[k]
      rho=rhoArray[l]
      C=Carray[j]
      
      for(i in 1:numsims){
        label=paste(inicio,i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
        (label)
        a=read.table(label,header=TRUE)
        distrib[i]=a$meanrecomb
        points(-(j*5+k*2+l)/4+1.5,distrib[i],col=col_vector[k],pch=j+14)
      }
    }
  }
}

inicio="std_output_prueba1_cluster_prdm9_N1000__r"
      alpha=alphaArray[1]
      U=Uarray[2]
      rho=rhoArray[2]
      C=Carray[2]
      
      for(i in 1:numsims){
        label=paste(inicio,i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
        (label)
        a=read.table(label,header=TRUE)
        distrib[i]=a$meanrecomb
        points(-(2*5+2*2+2)/4+1.5,distrib[i],col=col_vector[4],pch=7)
      }
    


## allelic class diversity


count=count+1
inicio="std_output_prueba2_cluster_prdm9_N1000_g0.25_r"
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="MeanClassDiv",xlab=equislabel,log="y")
for(j in 1:length(Carray)){
  for(k in 1:length(Uarray)){
    for(l in 1:length(rhoArray)){
      alpha=alphaArray[1]
      U=Uarray[k]
      rho=rhoArray[l]
      C=Carray[j]
      
      for(i in 1:numsims){
        label=paste(inicio,i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
        (label)
        a=read.table(label,header=TRUE)
        distrib[i]=a$cl_div
        points(-(j*5+k*2+l)/4+1.5,distrib[i],col=col_vector[k],pch=j-1)
      }
    }
  }
}

inicio="std_output_prueba1_cluster_prdm9_N1000_t1M_g0.33_r"
for(j in 1:length(Carray)){
  for(k in 1:length(Uarray)){
    for(l in 1:length(rhoArray)){
      alpha=alphaArray[1]
      U=Uarray[k]
      rho=rhoArray[l]
      C=Carray[j]
      
      for(i in 1:numsims){
        label=paste(inicio,i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
        (label)
        a=read.table(label,header=TRUE)
        distrib[i]=a$cl_div
        points(-(j*5+k*2+l)/4+1.5,distrib[i],col=col_vector[k],pch=j+14)
      }
    }
  }
}
inicio="std_output_prueba1_cluster_prdm9_N1000__r"
alpha=alphaArray[1]
U=Uarray[2]
rho=rhoArray[2]
C=Carray[2]

for(i in 1:numsims){
  label=paste(inicio,i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
  (label)
  a=read.table(label,header=TRUE)
  distrib[i]=a$cl_div
  points(-(2*5+2*2+2)/4+1.5,distrib[i],col=col_vector[4],pch=7)
}




## zinc finger diversity


count=count+1
inicio="std_output_prueba2_cluster_prdm9_N1000_g0.25_r"
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="MeanZincFingerDiv",xlab=equislabel,log="y")
for(j in 1:length(Carray)){
  for(k in 1:length(Uarray)){
    for(l in 1:length(rhoArray)){
      alpha=alphaArray[1]
      U=Uarray[k]
      rho=rhoArray[l]
      C=Carray[j]
      
      for(i in 1:numsims){
        label=paste(inicio,i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
        (label)
        a=read.table(label,header=TRUE)
        distrib[i]=a$zf_div
        points(-(j*5+k*2+l)/4+1.5,distrib[i],col=col_vector[k],pch=j-1)
      }
    }
  }
}

inicio="std_output_prueba1_cluster_prdm9_N1000_t1M_g0.33_r"
for(j in 1:length(Carray)){
  for(k in 1:length(Uarray)){
    for(l in 1:length(rhoArray)){
      alpha=alphaArray[1]
      U=Uarray[k]
      rho=rhoArray[l]
      C=Carray[j]
      
      for(i in 1:numsims){
        label=paste(inicio,i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
        (label)
        a=read.table(label,header=TRUE)
        distrib[i]=a$zf_div
        points(-(j*5+k*2+l)/4+1.5,distrib[i],col=col_vector[k],pch=j+14)
      }
    }
  }
}
inicio="std_output_prueba1_cluster_prdm9_N1000__r"
alpha=alphaArray[1]
U=Uarray[2]
rho=rhoArray[2]
C=Carray[2]

for(i in 1:numsims){
  label=paste(inicio,i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
  (label)
  a=read.table(label,header=TRUE)
  distrib[i]=a$zf_div
  points(-(2*5+2*2+2)/4+1.5,distrib[i],col=col_vector[4],pch=7)
}





## 4Ns


count=count+1
inicio="std_output_prueba2_cluster_prdm9_N1000_g0.25_r"
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="4Ns",xlab=equislabel,log="y")
for(j in 1:length(Carray)){
  for(k in 1:length(Uarray)){
    for(l in 1:length(rhoArray)){
      alpha=alphaArray[1]
      U=Uarray[k]
      rho=rhoArray[l]
      C=Carray[j]
      
      for(i in 1:numsims){
        label=paste(inicio,i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
        (label)
        a=read.table(label,header=TRUE)
        distrib[i]=a$X4Ns
        points(-(j*5+k*2+l)/4+1.5,distrib[i],col=col_vector[k],pch=j-1)
      }
    }
  }
}

inicio="std_output_prueba1_cluster_prdm9_N1000_t1M_g0.33_r"
for(j in 1:length(Carray)){
  for(k in 1:length(Uarray)){
    for(l in 1:length(rhoArray)){
      alpha=alphaArray[1]
      U=Uarray[k]
      rho=rhoArray[l]
      C=Carray[j]
      
      for(i in 1:numsims){
        label=paste(inicio,i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
        (label)
        a=read.table(label,header=TRUE)
        distrib[i]=a$X4Ns
        points(-(j*5+k*2+l)/4+1.5,distrib[i],col=col_vector[k],pch=j+14)
      }
    }
  }
}
inicio="std_output_prueba1_cluster_prdm9_N1000__r"
alpha=alphaArray[1]
U=Uarray[2]
rho=rhoArray[2]
C=Carray[2]

for(i in 1:numsims){
  label=paste(inicio,i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
  (label)
  a=read.table(label,header=TRUE)
  distrib[i]=a$X4Ns
  points(-(2*5+2*2+2)/4+1.5,distrib[i],col=col_vector[4],pch=7)
}


#dev.off()
