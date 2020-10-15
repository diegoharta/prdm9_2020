

library(RColorBrewer)
rainbowcols <- palette( brewer.pal(n=9,name="Set1"))

#rainbowcols <- rainbow(8, s=0.7)
#rainbowcols <- heat.colors(8, alpha=1)

label="prueba-056-3-A20-P20-long-N5000"

label = "pruebaKmin5_e1m2_plusminus"
label = "pruebaGeomDistrib"
label = "prueba_g9e4m10d12"
label = "prueba_e4m10d115p2"
label = "prueba_e4m10d12j5_pruebaHist"


setwd(paste("~/Documents/Projects/PZIFE/C_scripts_and_data/",label,sep=""))

a=read.table(paste("znfCountStatistics_",label,".dat",sep=""))


plot(a[,1],type="l",ylim=c(0,20),xlim=c(0,500))
lines(a[,1]+sqrt(a[,2]),col=3)
lines(a[,1]-sqrt(a[,2]),col=3)


a=read.table(paste("znfDivStatistics_",label,".dat",sep=""))

plot(0,0,xlim=c(0,500),ylim=c(0,1))
lines(a[,1]/4,col=rainbowcols[1])
for(i in 2:13){
  lines(a[,i]/4,col=rainbowcols[i])
}
i=7
#lines(a[,i]/4,col=rainbowcols[6])
i=10
#lines(a[,i]/4,col=rainbowcols[7])
i=13
#lines(a[,i]/4,col=rainbowcols[8])
title(sub=paste("znfDivStatistics_",label,sep=""))
legend("topright", legend=c("neighbour 1","neighbour 2","neighbour 3","neighbour 4","neighbour 5","neighbour 7","neighbour 10","neighbour 13"), 
       col=c(rainbowcols[1],rainbowcols[2],rainbowcols[3],rainbowcols[4],rainbowcols[5],rainbowcols[6],rainbowcols[7],rainbowcols[8]),
       lty=1,bty="n")




a=read.table(paste("histogramOfSizeOfZnfArray_",label,".dat",sep=""))
x=mat.or.vec(1,20)
y=mat.or.vec(1,20)
for(j in 1:20){x[j]=j}
plot(0,0,xlim=c(0,20),ylim=c(0,.3))
for(i in 1:9){
  #for(k in 1:20){
  #  points(k,a[(i-1)*5+1,k],col=i,pch=i)
  #}
  for(k in 1:19){
    y[k]=a[(i-1)*5+1,k]
  }
  lines(x,y,col=i)
}
title(sub=paste("Histogram Znf array size",label,sep=""))
legend("topright", legend=c("1","11","21","31","41","51","61","71","81"), 
       col=c(1,2,3,4,5,6,7,8,9),lty=1,
       bty="n")

bplus=0.000012
bminus=0.00001
alpha=bplus/bminus
kmin=5
kmax =20
suma=0
for(k in kmin:kmax){
  suma=suma+alpha^(k-kmin)/k
}
pikmin=1/(kmin*suma)
cte=pikmin*kmin/(alpha^kmin)
pik=mat.or.vec(1,kmax)
for(k in kmin:kmax){
  pik[k]=cte*(alpha^k)/k
}
#plot(0,0,xlim=c(0,20),ylim=c(0,.5))
for(k in 1:20){
  points(k,pik[k],col=i,pch=i)
}


## Generate PDFs

pdf(paste("znfCountStatistics_",label,".pdf",sep=""), width=8, height= 4)

a=read.table(paste("znfCountStatistics_",label,".dat",sep=""))
plot(a[,1],type="l",ylim=c(0,15),xlim=c(0,500))
lines(a[,1]+sqrt(a[,2]),col=3)
lines(a[,1]-sqrt(a[,2]),col=3)
dev.off()

pdf(paste("znfDivStatistics_",label,".pdf",sep=""), width=8, height= 4)
a=read.table(paste("znfDivStatistics_",label,".dat",sep=""))
plot(0,0,xlim=c(0,250),ylim=c(0,.2))
lines(a[,1]/4,col=rainbowcols[1])
for(i in 2:5){
  lines(a[,i]/4,col=rainbowcols[i])
}
i=7
lines(a[,i]/4,col=rainbowcols[6])
i=10
lines(a[,i]/4,col=rainbowcols[7])
i=13
lines(a[,i]/4,col=rainbowcols[8])
title(sub=paste("znfDivStatistics_",label,sep=""))
legend("topright", legend=c("neighbour 1","neighbour 2","neighbour 3","neighbour 4","neighbour 5","neighbour 7","neighbour 10","neighbour 13"), 
       col=c(rainbowcols[1],rainbowcols[2],rainbowcols[3],rainbowcols[4],rainbowcols[5],rainbowcols[6],rainbowcols[7],rainbowcols[8]),
       lty=1,bty="n")
dev.off()



pdf(paste("histZnfArraySize_",label,".pdf",sep=""), width=8, height= 4)

a=read.table(paste("histogramOfSizeOfZnfArray_",label,".dat",sep=""))
x=mat.or.vec(1,20)
y=mat.or.vec(1,20)
for(j in 1:20){x[j]=j}
plot(0,0,xlim=c(0,20),ylim=c(0,.3))
for(i in 1:9){
  #for(k in 1:20){
  #  points(k,a[(i-1)*5+1,k],col=i,pch=i)
  #}
  for(k in 1:19){
    y[k]=a[(i-1)*5+1,k]
  }
  lines(x,y,col=i)
}
title(sub=paste("Histogram Znf array size",label,sep=""))
legend("topright", legend=c("1","11","21","31","41","51","61","71","81"), 
       col=c(1,2,3,4,5,6,7,8,9),lty=1,
       bty="n")

bplus=0.000012
bminus=0.00001
alpha=bplus/bminus
kmin=5
kmax =20
suma=0
for(k in kmin:kmax){
  suma=suma+alpha^(k-kmin)/k
}
pikmin=1/(kmin*suma)
cte=pikmin*kmin/(alpha^kmin)
pik=mat.or.vec(1,kmax)
for(k in kmin:kmax){
  pik[k]=cte*(alpha^k)/k
}
#plot(0,0,xlim=c(0,20),ylim=c(0,.5))
for(k in 1:20){
  points(k,pik[k],col=i,pch=i)
}
dev.off()