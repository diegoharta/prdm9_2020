setwd("~/Documents/Projects/PZIFE/Prdm9Nicolas/dataFromCluster/TR_2019_02_18/")

col_vector = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf','#377eb8','#4daf4a','#984ea3','#e41a1c','#ff7f00','#f781bf','#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf','#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf')


m <- rbind(c(1,2),c(3,4))
layout(m,heights = c(2.5,2.5))

par(mar = c(5,5, 0.5, 0.5))
Carray=c(2)
numsims=1
inicio="std_output_"
final=".txtnnew"

xmin=-1
xmax=5
yemin=c(0,0,0,1.8,0,.0011,0.8)
yemax=c(.5,20,10,5,6,.5,3.2)

alphaArray=c(2)
rhoArray=c(0.522879,1.52288,2.52288,3.52288,4.52288)
CarrayN=c(0,1,2,3,4)
UarrayN=c(2,3,4,5,6)

alphaArray=c(2)
rhoArray=c(1.52288,2.52288,3.52288,4.52288)
CarrayN=c(0,1,2,3)
UarrayN=c(2,3,4,5)

alphaArray=c(0,1,2,3,4,5)
rhoArray=c(1,2,3,4,5)
Carray=c(-1,0,1,2,3)
Uarray=c(1,2,3,4,5)



count=1
distrib=c()
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="MeanRec",xlab="log rho")
for(x in 1:length(Carray))
{
  #plot(-100,-100,xlim=c(0,21),ylim=c(0,1),xlab="MeanRec")
  for(k in 1:length(rhoArray)){
   #alpha=alphaArray[k]
    rho=rhoArray[k]
    C=CarrayN[k]
    U=UarrayN[k]
    
    for(i in 1:numsims){
      label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
      (label)
      a=read.table(label,header=TRUE)
      distrib[i]=a$meanrecomb
      # points(i,a$meanrec)
    }
    points(rho,mean(distrib),col=col_vector[k])
    text(rho,rho/10,paste(C,U,rho,alpha,sep=" "))
    
    
  }
}
# 
count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="MeanClassDiv")
for(x in 1:length(Carray))
{
  #plot(-100,-100,xlim=c(0,21),ylim=c(yemin[count],yemax[count])0,1),xlab="MeanRec")
  for(k in 1:length(rhoArray)){
   #alpha=alphaArray[k]
    rho=rhoArray[k]
    C=CarrayN[k]
    U=UarrayN[k]
    
    for(i in 1:numsims){
      label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
      a=read.table(label,header=TRUE)
      distrib[i]=a$cl_div
      # points(i,a$meanrec)
    }
    points(rho,mean(distrib),col=col_vector[k])
  }
}

count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="Znf_div")
for(x in 1:length(Carray))
{
  for(k in 1:length(rhoArray)){
   #alpha=alphaArray[k]
    rho=rhoArray[k]
    C=CarrayN[k]
    U=UarrayN[k]
    
    for(i in 1:numsims){
      label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
      a=read.table(label,header=TRUE)
      distrib[i]=a$zf_div
      # points(i,a$meanrec)
    }
    points(rho,mean(distrib),col=col_vector[k])
  }
}

plot.new()
legend("top",c(paste("C",CarrayN[1],sep=""), paste("C",CarrayN[2],sep=""),paste("C",CarrayN[3],sep=""),
               paste("C",CarrayN[4],sep=""),paste("C",CarrayN[5],sep="")),ncol = 2, 
       col=c(col_vector[1],col_vector[2],col_vector[3],col_vector[4],col_vector[5]),
       #pch=c(tipo[1],tipo[2],tipo[3],NA,NA,tipo[5],tipo[5],tipo[5]),
       lty=c(1,1,1,1,1),#pt.cex=c(1,1,1,2,2,tamany[1],tamany[2],tamany[3]),
       lwd=c(3,3,3,3,3),bty = "n",x.intersp=0.05)



#This second part takes file sin which N has been specifically modified. 

setwd("~/Documents/Projects/PZIFE/Prdm9Nicolas/dataFromCluster/TR_2019_02_19_b/")

Carray=c(2)
numsims=1
inicio="std_output_"
final=".txt"

numsims=2
xmin=-1
xmax=5
yemin=c(0,0,0,1.8,0,.0011,0.8)
yemax=c(1,20,10,5,6,.5,3.2)

alphaArray=c(0.30103, 1, 1.30103, 2, 2.30103, 3)
alphaArray=c(2,2,2,2,2,2)
#alphaArray=c(0.30103,0.30103,0.30103,0.30103,0.30103,0.30103)
rhoArray=c(0.823909, 1.52288,1.82391, 2.52288, 2.82391, 3.52288)
rhoArray=c(2.82391,2.82391, 2.82391, 2.82391, 2.82391, 2.82391)
Narray=c(100, 500, 1000, 5000, 10000, 50000)
C=1
U=1 


count=1
distrib=c()
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="MeanRec",xlab="log rho")
for(x in 1:length(Carray))
{
  #plot(-100,-100,xlim=c(0,21),ylim=c(0,1),xlab="MeanRec")
  for(k in 1:length(rhoArray)){
    alpha=alphaArray[k]
    rho=rhoArray[k]
    N=Narray[k]
    #C=CarrayN[k]
    #U=UarrayN[k]
    
    for(i in 1:numsims){
      label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",N,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
          a=read.table(label,header=TRUE)
      distrib[i]=a$meanrecomb
      # points(i,a$meanrec)
      points(k-1,mean(distrib),col=col_vector[k])
    }
  
    text(k-1,k/10,paste(C,U,rho,alpha,sep=" "))
    
    
  }
}

count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="MeanClassDiv")
for(x in 1:length(Carray))
{
  #plot(-100,-100,xlim=c(0,21),ylim=c(yemin[count],yemax[count])0,1),xlab="MeanRec")
  for(k in 1:length(rhoArray)){
    alpha=alphaArray[k]
    rho=rhoArray[k]
    N=Narray[k]
    #C=CarrayN[k]
    #U=UarrayN[k]
    
    for(i in 1:numsims){
      label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",N,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
      a=read.table(label,header=TRUE)
      distrib[i]=a$cl_div
      points(k-1,mean(distrib),col=col_vector[k])
      
    }
   
    
  }
}

count=count+1
plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab="Znf_div")
for(x in 1:length(Carray))
{
  for(k in 1:length(rhoArray)){
    alpha=alphaArray[k]
    rho=rhoArray[k]
    N=Narray[k]
    #C=CarrayN[k]
    #U=UarrayN[k]
    
    for(i in 1:numsims){
      label=paste(inicio,"prueba1_cluster_prdm9_N1000_t1M_r",i,"_N",N,"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
      a=read.table(label,header=TRUE)
      distrib[i]=a$zf_div
      points(k-1,mean(distrib),col=col_vector[k])
      
    }
   
   
  }
}

plot.new()
legend("top",c(paste("N",Narray[1],sep=""), paste("N",Narray[2],sep=""),paste("N",Narray[3],sep=""),
               paste("N",Narray[4],sep=""),paste("N",Narray[5],sep=""), paste("N",Narray[6],sep="")),ncol = 2, 
       col=c(col_vector[1],col_vector[2],col_vector[3],col_vector[4],col_vector[5], col_vector[6]),
       #pch=c(tipo[1],tipo[2],tipo[3],NA,NA,tipo[5],tipo[5],tipo[5]),
       lty=c(1,1,1,1,1,1),#pt.cex=c(1,1,1,2,2,tamany[1],tamany[2],tamany[3]),
       lwd=c(3,3,3,3,3,3),bty = "n",x.intersp=0.05)




