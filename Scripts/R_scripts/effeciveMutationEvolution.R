label="prueba_1.2"

setwd(paste("~/Documents/Projects/PZIFE/C_scripts_and_data/",label,sep=""))

pdf(paste("effectiveMutationEvolution_",label,".pdf",sep=""), width=8, height= 4)

a=read.table(paste("auxx",label,".dat",sep=""))
a[,1]
x=mat.or.vec(1,1000)
y=mat.or.vec(1,1000)
for(j in 1:1000){x[j]=j}
plot(0,0,xlim=c(0,1000),ylim=c(0,1))
for(i in 1:2){
  #for(k in 1:20){
  #  points(k,a[(i-1)*5+1,k],col=i,pch=i)
  #}
#  for(k in 1:19){
 #   y[k]=a[(i-1)*5+1,k]
  #}
  lines(x,a[,i],col=i)
}
title(sub=paste("effective mutation rates ",label,sep=""))
legend("topright", legend=c("ZnF mutation","Point mutation"), 
       col=c(1,2),lty=1,
       bty="n")
dev.off()
