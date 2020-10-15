setwd("~/Documents/Projects/PZIFE/Prdm9Nicolas/dataFromCluster/TR_2019_02_18/")

col_vector = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf','#377eb8','#4daf4a','#984ea3','#e41a1c','#ff7f00','#f781bf','#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf','#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf')


m <- rbind(c(1,2),c(3,4))
layout(m,heights = c(2.5,2.5))

par(mar = c(5,5, 0.5, 0.5))
Carray=c(0,1,2,3,4)
numsims=1
inicio="std_output_"
final=".txtnnew"

xmin=-1
xmax=5
yemin=c(0,0,0,1.8,0,.0011,0.8)
yemax=c(.5,10,10,5,6,.5,3.2)

alphaArray=c(0,1,2,3,4)
rhoArray=c(0.522879,1.52288,2.52288,3.52288,4.52288)
count=1
distrib=c()
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="MeanRec",xlab="log Alpha")
for(x in 1:length(Carray))
{
  #plot(-100,-100,xlim=c(0,21),ylim=c(0,1),xlab="MeanRec")
  for(k in 1:length(alphaArray)){
    alpha=alphaArray[k]
    rho=rhoArray[k]
    
    for(i in 1:numsims){
      label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i+1,"_p",alpha,"_X",rho,"_DNull_C",Carray[x],"_U3",final,sep="")
      a=read.table(label,header=TRUE)
      distrib[i]=a$meanrecomb
      # points(i,a$meanrec)
    }
    points(alpha,mean(distrib),col=col_vector[x])
    
  
  }
}
# 
# count=count+1
# plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="AlleleDiv")
# for(x in 1:length(Carray))
# {
#   #plot(-100,-100,xlim=c(0,21),ylim=c(yemin[count],yemax[count])0,1),xlab="MeanRec")
#   for(k in 1:length(alphaArray)){
#     alpha=alphaArray[k]
#     #rho=rhoArrray[k]
#     
#     for(i in 1:numsims){
#       label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i+1,"_p",alpha,"_X",rho,"_DNull_C",Carray[x],"_U3",final,sep="")
#       a=read.table(label,header=TRUE)
#       distrib[i]=a$allele_div
#       # points(i,a$meanrec)
#     }
#     points(alpha,mean(distrib),col=col_vector[x])
#     
#     
#   }
# }
count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="MeanClassDiv")
for(x in 1:length(Carray))
{
  #plot(-100,-100,xlim=c(0,21),ylim=c(yemin[count],yemax[count])0,1),xlab="MeanRec")
  for(k in 1:length(alphaArray)){
    alpha=alphaArray[k]
    rho=rhoArray[k]
    
    for(i in 1:numsims){
      label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i+1,"_p",alpha,"_X",rho,"_DNull_C",Carray[x],"_U3",final,sep="")
      a=read.table(label,header=TRUE)
      distrib[i]=a$cl_div
      # points(i,a$meanrec)
    }
    points(alpha,mean(distrib),col=col_vector[x])
    
    
  }
}
# count=count+1
# plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="Znf_divPop")
# for(x in 1:length(Carray))
# {
#   for(k in 1:length(alphaArray)){
#     alpha=alphaArray[k]
#     #rho=rhoArrray[k]
#     
#     for(i in 1:numsims){
#       label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i+1,"_p",alpha,"_X",rho,"_DNull_C",Carray[x],"_U3",final,sep="")
#       a=read.table(label,header=TRUE)
#       distrib[i]=a$zf_divpop
#       # points(i,a$meanrec)
#     }
#     points(alpha,mean(distrib),col=col_vector[x])
#     
#     
#   }
# }
count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="Znf_div")
for(x in 1:length(Carray))
{
  for(k in 1:length(alphaArray)){
    alpha=alphaArray[k]
    rho=rhoArray[k]
    
    for(i in 1:numsims){
      label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i+1,"_p",alpha,"_X",rho,"_DNull_C",Carray[x],"_U3",final,sep="")
      a=read.table(label,header=TRUE)
      distrib[i]=a$zf_div
      # points(i,a$meanrec)
    }
    points(alpha,mean(distrib),col=col_vector[x])
    
    
  }
}
# count=count+1
# plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="Class_tau")
# for(x in 1:length(Carray))
# {
#   for(k in 1:length(alphaArray)){
#     alpha=alphaArray[k]
#     #rho=rhoArrray[k]
#     
#     for(i in 1:numsims){
#       label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i+1,"_p",alpha,"_X",rho,"_DNull_C",Carray[x],"_U3",final,sep="")
#       a=read.table(label,header=TRUE)
#       distrib[i]=a$class_tau
#       # points(i,a$meanrec)
#     }
#     points(alpha,mean(distrib),col=col_vector[x])
#     
#     
#   }
# }
# count=count+1
# plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="Znf_tau")
# for(x in 1:length(Carray))
# {
#   for(k in 1:length(alphaArray)){
#     alpha=alphaArray[k]
#     #rho=rhoArrray[k]
#     
#     for(i in 1:numsims){
#       label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i+1,"_p",alpha,"_X",rho,"_DNull_C",Carray[x],"_U3",final,sep="")
#       a=read.table(label,header=TRUE)
#       distrib[i]=a$zf_tau
#       # points(i,a$meanrec)
#     }
#     points(alpha,mean(distrib),col=col_vector[x])
#     
#     
#   }
# }
plot.new()
legend("top",c(paste("C",Carray[1],sep=""), paste("C",Carray[2],sep=""),paste("C",Carray[3],sep=""),
               paste("C",Carray[4],sep=""),paste("C",Carray[5],sep="")),ncol = 2, 
       col=c(col_vector[1],col_vector[2],col_vector[3],col_vector[4],col_vector[5]),
       #pch=c(tipo[1],tipo[2],tipo[3],NA,NA,tipo[5],tipo[5],tipo[5]),
       lty=c(1,1,1,1,1),#pt.cex=c(1,1,1,2,2,tamany[1],tamany[2],tamany[3]),
       lwd=c(3,3,3,3,3),bty = "n",x.intersp=0.05)