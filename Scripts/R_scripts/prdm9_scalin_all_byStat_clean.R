setwd("~/Documents/Projects/PZIFE/Prdm9Nicolas/dataFromCluster/TR_2019_03_01/")

col_vector = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf','#377eb8','#4daf4a','#984ea3','#e41a1c','#ff7f00','#f781bf','#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf','#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf')
pdf("scaling_pXCU_2_byStatistic_CNull_bueno_4Ns.pdf",height = 5, width=6)

m <- rbind(c(1,2),c(3,4))
layout(m,heights = c(2.5,2.5))

par(mar = c(5,5, 0.5, 0.5))
numsims=2
inicio="std_output_prueba1_cluster_prdm9_N1000__r"
final=".txt"

xmin=-4.2
xmax=0.2

alphaArray=c(-0.39794, 0, 0.124939, 0.30103, 0.60206,1, 1.12494, 1.30103,1.60206,2, 2.12494,2.30103,2.60206,3,3.12494, 3.30103,
             3.60206,4)
rhoArray=c(0.60206,1, 1.12494, 1.30103,1.60206,2, 2.12494,2.30103,2.60206,3,3.12494, 3.30103,
           3.60206,4, 4.12494,4.30103,4.60206,5)
Carray=c(-1.39794, -1, -0.875061, -0.69897, -0.39794, 0, 0.124939, 0.30103, 0.60206,1, 1.12494, 1.30103,1.60206,
         2, 2.12494,2.30103,2.60206,3)
Uarray=c(0.60206,1, 1.12494, 1.30103,1.60206,2, 2.12494,2.30103,2.60206,3,3.12494, 3.30103,
         3.60206,4, 4.12494,4.30103,4.60206,5)
Narray=c(100,500,1000,5000,10000,50000)

stats=c(11)#,6,7,11)
stats[1]
a[[stats[1]]]
fixed1<-rhoArray
fixed2<-Carray
fixed3<-Uarray
variable<-alphaArray


count=1
distrib=c()

yemin=c(0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1)
yemax=c(1,1,1,1,3,4,6.5,14,10,10,8,32,700,900,60,600)
equislabel=c(expression(alpha),expression(rho),"C","U")
ylabels=c("Mean recombination activity","Allelic class diversity","Zinc finger diversity","4Ns")


for(count in 1:length(stats)){
counter=1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[(count-1)*4+counter],yemax[(count-1)*4+counter]),ylab=ylabels[count],xlab=equislabel[counter],xaxt="n",cex.lab=0.8,las=1,cex.axis=0.8)
axis(1, at=c(-4,-3,-2,-1,0), labels=c(0.0001,0.001,0.01,0.1,1),cex.axis=0.8)
for(k in 1:length(variable)){
  #alpha=alphaArray[k]
  rho=fixed1[10]
  C=fixed2[10]
  U=fixed3[10]
  alpha=variable[k]
  
  for(i in 1:numsims){
    label=paste(inicio,i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    (label)
    a=read.table(label,header=TRUE)
    distrib[i]=a[[stats[count]]]
    points(-k/4+.5,distrib[i],col=col_vector[1])
  }
  for(i in 1:numsims){
    label=paste(inicio,i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_CNull_U",U,final,sep="")
    (label)
    a=read.table(label,header=TRUE)
    distrib[i]=a[[stats[count]]]
    points(-k/4+.5,distrib[i],col=col_vector[2])
  }
}

counter=counter+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[(count-1)*4+counter],yemax[(count-1)*4+counter]),ylab=ylabels[count],xlab=equislabel[counter],xaxt="n",cex.lab=0.8,las=1,cex.axis=0.8)
axis(1, at=c(-4,-3,-2,-1,0), labels=c(0.00001,0.0001,0.001,0.01,0.1),cex.axis=0.8)
for(k in 1:length(rhoArray)){
  alpha=alphaArray[10]
  rho=rhoArray[k]
  C=Carray[10]
  U=Uarray[10]
  
  for(i in 1:numsims){
    label=paste(inicio,i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    (label)
    a=read.table(label,header=TRUE)
    distrib[i]=a[[stats[count]]]
    points(-k/4+.5,distrib[i],col=col_vector[1])
  }
  for(i in 1:numsims){
    label=paste(inicio,i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_CNull_U",U,final,sep="")
    (label)
    a=read.table(label,header=TRUE)
    distrib[i]=a[[stats[count]]]
    points(-k/4+.5,distrib[i],col=col_vector[2])
  }
}

counter=counter+1
center=10
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[(count-1)*4+counter],yemax[(count-1)*4+counter]),ylab=ylabels[count],xlab=equislabel[counter],xaxt="n",cex.lab=0.8,las=1,cex.axis=0.8)
axis(1, at=c(-4,-3,-2,-1,0), labels=c(0.004,0.04,0.4,4,40),cex.axis=0.8)
for(k in 1:length(Carray)){
  alpha=alphaArray[center]
  rho=rhoArray[center]
  C=Carray[k]
  U=Uarray[center]
  
  for(i in 1:numsims){
    label=paste(inicio,i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    (label)
    a=read.table(label,header=TRUE)
    distrib[i]=a[[stats[count]]]
    points(-k/4+.5,distrib[i],col=col_vector[1])
  }
}
for(i in 1:numsims){
  label=paste(inicio,i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_CNull_U",U,final,sep="")
  (label)
  a=read.table(label,header=TRUE)
  distrib[i]=a[[stats[count]]]
  points(-(k+1)/4+.5,distrib[i],col=col_vector[2])
}

counter=counter+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[(count-1)*4+counter],yemax[(count-1)*4+counter]),ylab=ylabels[count],xlab=equislabel[counter],xaxt="n",cex.lab=0.8,las=1,cex.axis=0.8)
axis(1, at=c(-4,-3,-2,-1,0), labels=c(0.00004,0.0004,0.004,0.04,0.4),cex.axis=0.8)
for(k in 1:length(Uarray)){
  alpha=alphaArray[center]
  rho=rhoArray[center]
  C=Carray[center]
  U=Uarray[k]
  
  for(i in 1:numsims){
    label=paste(inicio,i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
    a=read.table(label,header=TRUE)
    distrib[i]=a[[stats[count]]]
    points(-k/4+.5,distrib[i],col=col_vector[1])
  }
  for(i in 1:numsims){
    label=paste(inicio,i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_CNull_U",U,final,sep="")
    a=read.table(label,header=TRUE)
    distrib[i]=a[[stats[count]]]
    points(-k/4+.5,distrib[i],col=col_vector[2])
  }
}

}
dev.off()