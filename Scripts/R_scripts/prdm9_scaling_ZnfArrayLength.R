setwd("~/Documents/Projects/PZIFE/Prdm9Nicolas/dataFromCluster/TR_2019_02_19_e/")

col_vector = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf','#377eb8','#4daf4a','#984ea3','#e41a1c','#ff7f00','#f781bf','#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf','#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf')
pdf("scaling.pdf",height = 8, width=6)

m <- rbind(c(1,2),c(3,4),c(5,6),c(7,7))
layout(m,heights = c(2.5,2.5,2.5,2))

par(mar = c(5,5, 0.5, 0.5))
numsims=2
inicio="std_output_"
final=".txtnewalt"
final=".txt"

xmin=4
xmax=22
yemin=c(0.6,1.5,1,1,0.5,0)
yemax=c(1,20,10,150,1,.4)

alphaArray=c(1)
rhoArray=c(2.60206,3.60206)
Carray=c(0,1)
Uarray=c(2,3)
Farray=c(6,8,10,12,14,16,18,20)

Narray=c(1000)

equislabel="znf length"

count=1
distrib=c()
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="MeanRec",xlab=equislabel)
abline(h=0.9,col="grey")
abline(h=0.8,col="grey")
abline(h=0.7,col="grey")
abline(h=0.6,col="grey")
for(k in 1:length(Farray)){
  for(j in 1:length(rhoArray)){
  znf=Farray[k]
  alpha=alphaArray[1]
  rho=rhoArray[j]
  C=Carray[1]
  U=Uarray[1]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_F",znf,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    a=read.table(label,header=TRUE)
    distrib[i]=a$meanrecomb
    points(k*2+4,distrib[i],col=col_vector[j])
  }
  }
}




count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="MeanClassDiv",xlab=equislabel,log="y")
for(k in 1:length(Farray)){
  for(j in 1:length(rhoArray)){
    znf=Farray[k]
    alpha=alphaArray[1]
    rho=rhoArray[j]
    C=Carray[1]
    U=Uarray[1]
    
    for(i in 1:numsims){
      label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_F",znf,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
      a=read.table(label,header=TRUE)
      distrib[i]=a$cl_div
      points(k*2+4,distrib[i],col=col_vector[j])
    }
  }
}

count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="Znf_div",xlab=equislabel)
for(k in 1:length(Farray)){
  for(j in 1:length(rhoArray)){
    znf=Farray[k]
    alpha=alphaArray[1]
    rho=rhoArray[j]
    C=Carray[1]
    U=Uarray[1]
    
    for(i in 1:numsims){
      label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_F",znf,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
      a=read.table(label,header=TRUE)
      distrib[i]=a$zf_div
      points(k*2+4,distrib[i],col=col_vector[j])
    }
  }
}

count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="4Ns",xlab=equislabel,log="y")
for(k in 1:length(Farray)){
  for(j in 1:length(rhoArray)){
    znf=Farray[k]
    alpha=alphaArray[1]
    rho=rhoArray[j]
    C=Carray[1]
    U=Uarray[1]
    
    for(i in 1:numsims){
      label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_F",znf,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
      a=read.table(label,header=TRUE)
      distrib[i]=a$X4Ns
      points(k*2+4,distrib[i],col=col_vector[j])
    }
  }
}

count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="FracInvZnfBornWithinBindReg",xlab=equislabel)
for(k in 1:length(Farray)){
  for(j in 1:length(rhoArray)){
    znf=Farray[k]
    alpha=alphaArray[1]
    rho=rhoArray[j]
    C=Carray[1]
    U=Uarray[1]
    
    for(i in 1:numsims){
      label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_F",znf,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
      a=read.table(label,header=TRUE)
      distrib[i]=a$FracInvZnfBornWithinBindReg
      points(k*2+4,distrib[i],col=col_vector[j])
    }
  }
}



color_count=0
count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="SaturationIndex",xlab=equislabel)
for(k in 1:length(Farray)){
      for(j in 1:length(rhoArray)){
        znf=Farray[k]
        alpha=alphaArray[1]
        rho=rhoArray[j]
        C=Carray[1]
        U=Uarray[1]
        
      
      for(i in 1:numsims){
        label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_F",znf,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
        a=read.table(label,header=TRUE)
        distrib[i]=a$cl_div/a$MaxClassDivBasedOnZnmfDiv1
        points(k*2+4,distrib[i],col=col_vector[j])
      }
    }
}




plot.new()

par(mar = c(0.5,0.5, 0.5, 0.5))
legend("top",c(expression(paste(rho,"=0.01",sep="")),expression(paste(rho,"=0.001",sep=""))),ncol = 2, 
       col=c(col_vector[1],col_vector[2]),pch=c(1,1),
       bty = "n",x.intersp=0.5)


par(mar = c(5,5, 0.5, 0.5))
equislabel="Zinc finger array length"
num=1
rhops=rhoArray[num]
subtit=expression(paste(rho,"=",0.01,sep=""))


count=1
color_count=0
distrib=c()

plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="MeanRec",xlab=equislabel,sub=subtit)
abline(h=0.9,col="grey")
abline(h=0.8,col="grey")
abline(h=0.7,col="grey")
abline(h=0.6,col="grey")
 

    for(jj in 1:length(Carray)){
      for(jjj in 1:length(Uarray)){
  color_count=color_count+1  
  for(k in 1:length(Farray)){
  znf=Farray[k]
  alpha=alphaArray[1]
  rho=rhoArray[num]
  C=Carray[jj]
  U=Uarray[jjj]
  
  for(i in 1:numsims){
    label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_F",znf,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    a=read.table(label,header=TRUE)
    distrib[i]=a$meanrecomb
    points(k*2+4,distrib[i],col=col_vector[color_count])
  }
}
}}

color_count=0
count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="MeanClassDiv",xlab=equislabel,log="y")


  for(jj in 1:length(Carray)){
    for(jjj in 1:length(Uarray)){
      color_count=color_count+1  
      for(k in 1:length(Farray)){
        znf=Farray[k]
        alpha=alphaArray[1]
        rho=rhoArray[num]
        C=Carray[jj]
        U=Uarray[jjj]
        
        for(i in 1:numsims){
          label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_F",znf,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
          a=read.table(label,header=TRUE)
          distrib[i]=a$cl_div
          points(k*2+4,distrib[i],col=col_vector[color_count])
        }
      }
    }}
color_count=0
count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="Znf_div",xlab=equislabel)


  for(jj in 1:length(Carray)){
    for(jjj in 1:length(Uarray)){
      color_count=color_count+1  
      for(k in 1:length(Farray)){
        znf=Farray[k]
        alpha=alphaArray[1]
        rho=rhoArray[num]
        C=Carray[jj]
        U=Uarray[jjj]
        
        for(i in 1:numsims){
          label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_F",znf,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
          a=read.table(label,header=TRUE)
          distrib[i]=a$zf_div
          points(k*2+4,distrib[i],col=col_vector[color_count])
        }
      }
    }}

color_count=0
count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="4Ns",xlab=equislabel,log="y")

  for(jj in 1:length(Carray)){
    for(jjj in 1:length(Uarray)){
      color_count=color_count+1  
      for(k in 1:length(Farray)){
        znf=Farray[k]
        alpha=alphaArray[1]
        rho=rhoArray[num]
        C=Carray[jj]
        U=Uarray[jjj]
        
        for(i in 1:numsims){
          label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_F",znf,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
          a=read.table(label,header=TRUE)
          distrib[i]=a$X4Ns
          points(k*2+4,distrib[i],col=col_vector[color_count])
        }
      }
    }}

color_count=0
count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="FracInvZnfBornWithinBindReg",xlab=equislabel)

for(jj in 1:length(Carray)){
  for(jjj in 1:length(Uarray)){
    color_count=color_count+1  
    for(k in 1:length(Farray)){
      znf=Farray[k]
      alpha=alphaArray[1]
      rho=rhoArray[num]
      C=Carray[jj]
      U=Uarray[jjj]
      
      for(i in 1:numsims){
        label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_F",znf,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
        a=read.table(label,header=TRUE)
        distrib[i]=a$FracInvZnfBornWithinBindReg
        points(k*2+4,distrib[i],col=col_vector[color_count])
      }
    }
  }}


color_count=0
count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="SaturationIndex",xlab=equislabel)


for(jj in 1:length(Carray)){
  for(jjj in 1:length(Uarray)){
    color_count=color_count+1  
    for(k in 1:length(Farray)){
      znf=Farray[k]
      alpha=alphaArray[1]
      rho=rhoArray[num]
      C=Carray[jj]
      U=Uarray[jjj]
      
      for(i in 1:numsims){
        label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_F",znf,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
        a=read.table(label,header=TRUE)
        distrib[i]=a$cl_div/a$MaxClassDivBasedOnZnmfDiv1
        points(k*2+4,distrib[i],col=col_vector[color_count])
      }
    }
  }}


plot.new()
par(mar = c(0.5,0.5, 0.5, 0.5))
legend("top",c("C=4  U=0.04", "C=4  U=0.004", "C=0.4  U=0.04", "C=0.4  U=0.004"),ncol = 2, 
       col=c(col_vector[1],col_vector[2],col_vector[3],col_vector[4]),pch=c(1,1,1,1),
       bty = "n",x.intersp=0.5)


par(mar = c(5,5, 0.5, 0.5))
equislabel="znf length"
subtit=expression(paste(rho,"=",0.001,sep=""))
num=2

count=1
color_count=0
distrib=c()

plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="MeanRec",xlab=equislabel,sub=subtit)
abline(h=0.9,col="grey")
abline(h=0.8,col="grey")
abline(h=0.7,col="grey")
abline(h=0.6,col="grey")

  for(jj in 1:length(Carray)){
    for(jjj in 1:length(Uarray)){
      color_count=color_count+1  
      for(k in 1:length(Farray)){
        znf=Farray[k]
        alpha=alphaArray[1]
        rho=rhoArray[num]
        C=Carray[jj]
        U=Uarray[jjj]
        
        for(i in 1:numsims){
          label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_F",znf,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
          a=read.table(label,header=TRUE)
          distrib[i]=a$meanrecomb
          points(k*2+4,distrib[i],col=col_vector[color_count])
        }
      }
    }}
color_count=0
count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="MeanClassDiv",xlab=equislabel,log="y")


  for(jj in 1:length(Carray)){
    for(jjj in 1:length(Uarray)){
      color_count=color_count+1  
      for(k in 1:length(Farray)){
        znf=Farray[k]
        alpha=alphaArray[1]
        rho=rhoArray[num]
        C=Carray[jj]
        U=Uarray[jjj]
        
        for(i in 1:numsims){
          label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_F",znf,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
          a=read.table(label,header=TRUE)
          distrib[i]=a$cl_div
          points(k*2+4,distrib[i],col=col_vector[color_count])
        }
      }
    }}
color_count=0
count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="Znf_div",xlab=equislabel)


  for(jj in 1:length(Carray)){
    for(jjj in 1:length(Uarray)){
      color_count=color_count+1  
      for(k in 1:length(Farray)){
        znf=Farray[k]
        alpha=alphaArray[1]
        rho=rhoArray[num]
        C=Carray[jj]
        U=Uarray[jjj]
        
        for(i in 1:numsims){
          label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_F",znf,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
          a=read.table(label,header=TRUE)
          distrib[i]=a$zf_div
          points(k*2+4,distrib[i],col=col_vector[color_count])
        }
      }
    }}

color_count=0
count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="4Ns",xlab=equislabel,log="y")


  for(jj in 1:length(Carray)){
    for(jjj in 1:length(Uarray)){
      color_count=color_count+1  
      for(k in 1:length(Farray)){
        znf=Farray[k]
        alpha=alphaArray[1]
        rho=rhoArray[num]
        C=Carray[jj]
        U=Uarray[jjj]
        
        for(i in 1:numsims){
          label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_F",znf,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
          a=read.table(label,header=TRUE)
          distrib[i]=a$X4Ns
          points(k*2+4,distrib[i],col=col_vector[color_count])
        }
      }
    }}

color_count=0
count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="FracInvZnfBornWithinBindReg",xlab=equislabel)


for(jj in 1:length(Carray)){
  for(jjj in 1:length(Uarray)){
    color_count=color_count+1  
    for(k in 1:length(Farray)){
      znf=Farray[k]
      alpha=alphaArray[1]
      rho=rhoArray[num]
      C=Carray[jj]
      U=Uarray[jjj]
      
      for(i in 1:numsims){
        label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_F",znf,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
        a=read.table(label,header=TRUE)
        distrib[i]=a$FracInvZnfBornWithinBindReg
        points(k*2+4,distrib[i],col=col_vector[color_count])
      }
    }
  }}


color_count=0
count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="SaturationIndex",xlab=equislabel)


for(jj in 1:length(Carray)){
  for(jjj in 1:length(Uarray)){
    color_count=color_count+1  
    for(k in 1:length(Farray)){
      znf=Farray[k]
      alpha=alphaArray[1]
      rho=rhoArray[num]
      C=Carray[jj]
      U=Uarray[jjj]
      
      for(i in 1:numsims){
        label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_F",znf,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
        a=read.table(label,header=TRUE)
        distrib[i]=a$cl_div/a$MaxClassDivBasedOnZnmfDiv1
        points(k*2+4,distrib[i],col=col_vector[color_count])
      }
    }
  }}




plot.new()
par(mar = c(0.5,0.5, 0.5, 0.5))
legend("top",c("C=4  U=0.04", "C=4  U=0.004", "C=0.4  U=0.04", "C=0.4  U=0.004"),ncol = 2, 
       col=c(col_vector[1],col_vector[2],col_vector[3],col_vector[4]),pch=c(1,1,1,1),
       bty = "n",x.intersp=0.5)


dev.off()

