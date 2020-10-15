setwd("~/Documents/Projects/PZIFE/Prdm9Nicolas/dataFromCluster/TR_2019_02_27/")

col_vector = c('#e41a1c','#377eb8','#4daf4a','#984ea3',"black",'#ff7f00','#a65628','#f781bf','#377eb8','#4daf4a','#984ea3','#e41a1c','#ff7f00','#f781bf','#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf','#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf')
#pdf("scaling.pdf",height = 5, width=6)


tamany=c(1,1.2,1.8,2)
tipo=c(22,23,24,25,1)

Propor=1
xmin=-3000.2
xmax=-.5
yemin=c(0,1,1,1,0.5)
yemax=c(1,10,12,100,1.5)

alphaArray=c(2)
rhoArray=c(2,3)
Carray=c(0,1,2)
Uarray=c(2,3)
Narray=c(100,500,1000,5000,10000,50000)

ylabs=c("MeanRecRate","AllelicClassDiv","ZnDiv","4Ns","Real / theor GC rate")
geneConvRate=c("g0.5","g0.33b","g0.25b","g0.143","g0")

equislabel="log alpha"

#Varying C and U and rho

mm <- rbind(c(1,2),c(3,4),c(5,6))
layout(mm,heights = c(2.5,2.5,2.5))
m=1
par(mar = c(5,5, 0.5, 0.5))
numsims=2
inicio="std_output_prueba2_cluster_prdm9_N1000_g0.25b_r"
final=".txt"

pointType=c(1,5,8,11,14,17)

equislabel=expression(paste("C & U & ",rho,sep=""))
countP=1
count=1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab=ylabs[count],xlab=equislabel)
for(jj in 1:length(geneConvRate)){
  inicio=paste("std_output_prueba2_cluster_prdm9_N1000_",geneConvRate[jj],"_r",sep="")
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
          #points(-(j*100+k*30+l)/4-jj/10+1.5,distrib[i],col=col_vector[jj],pch=k)
          points(((j-1)*1000+((k-1)*400)+((l-1)*200)+(jj-1)*30)*-Propor,distrib[i],col=col_vector[jj],pch=tipo[k],cex=tamany[l])
        }
      }
    }
  }
}


count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab=ylabs[count],xlab=equislabel)
for(jj in 1:length(geneConvRate)){
  inicio=paste("std_output_prueba2_cluster_prdm9_N1000_",geneConvRate[jj],"_r",sep="")
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
          #points(-(j*100+k*30+l)/4-jj/10+1.5,distrib[i],col=col_vector[jj],pch=k)
          points(((j-1)*1000+((k-1)*400)+((l-1)*200)+(jj-1)*30)*-Propor,distrib[i],col=col_vector[jj],pch=tipo[k],cex=tamany[l])
        }
      }
    }
  }
}

count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab=ylabs[count],xlab=equislabel)
for(jj in 1:length(geneConvRate)){
  inicio=paste("std_output_prueba2_cluster_prdm9_N1000_",geneConvRate[jj],"_r",sep="")
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
          #points(-(j*100+k*30+l)/4-jj/10+1.5,distrib[i],col=col_vector[jj],pch=k)
          points(((j-1)*1000+((k-1)*400)+((l-1)*200)+(jj-1)*30)*-Propor,distrib[i],col=col_vector[jj],pch=tipo[k],cex=tamany[l])
        }
      }
    }
  }
}

count=count+1
plot(100,10000,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab=ylabs[count],xlab=equislabel)
for(jj in 1:length(geneConvRate)){
  inicio=paste("std_output_prueba2_cluster_prdm9_N1000_",geneConvRate[jj],"_r",sep="")
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
          #points(-(j*100+k*30+l)/4-jj/10+1.5,distrib[i],col=col_vector[jj],pch=k)
          points(((j-1)*1000+((k-1)*400)+((l-1)*200)+(jj-1)*30)*-Propor,distrib[i],col=col_vector[jj],pch=tipo[k],cex=tamany[l])
        }
      }
    }
  }
}


count=count+1
plot(100,10000,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab=ylabs[count],xlab=equislabel)
for(jj in 1:length(geneConvRate)){
  inicio=paste("std_output_prueba2_cluster_prdm9_N1000_",geneConvRate[jj],"_r",sep="")
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
          distrib[i]=a$realConvRate/a$theorConvRate
          #points(-(j*100+k*30+l)/4-jj/10+1.5,distrib[i],col=col_vector[jj],pch=k)
          points(((j-1)*1000+((k-1)*400)+((l-1)*200)+(jj-1)*30)*-Propor,distrib[i],col=col_vector[jj],pch=tipo[k],cex=tamany[l])
        }
      }
    }
  }
}

#dev.off()
