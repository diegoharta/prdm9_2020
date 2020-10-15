label="prueba_1.52"

setwd(paste("~/Documents/Projects/PZIFE/C_scripts_and_data/",label,sep=""))

#pdf(paste("activeMotifsFreq_",label,".pdf",sep=""), width=8, height= 4)

a=read.table(paste("activeMotifsFreq_",label,".dat",sep=""))
a[2,]
length(a)
count=1
plot(0,0,xlim=c(.5,1),ylim=c(0,20))

for(i in 1:nrow(a)){
  if(i %% 100 == 1){
    lines(density(as.numeric(a[i,])),col=count)
    count= count +1
  }
}


av = mat.or.vec(1,nrow(a))
RowM <- rowMeans(a[, -ncol(a)])
plot(RowM)
prom=mean(RowM[1000:2000])
abline(h=prom,col=2,lwd=2)
text(1000,.996,prom)
#title(sub=paste("effective mutation rates ",label,sep=""))
#legend("topright", legend=c("ZnF mutation","Point mutation"), 
 #      col=c(1,2),lty=1,
  #     bty="n")
#dev.off()


limit = 300
init=100
#PRINT ALLELE FREQUENCIES
pdf(paste("alleleFreqs_",label,"_ZOOM2.pdf",sep=""), width=8, height= 4)
a=read.table(paste("alleleFreqs_",label,".dat",sep=""))
plot(0,0,ylim=c(0,1),xlim=c(init,nrow(a)))
for(i in 1:limit){
  lines(a[,i],col=i)
}
dev.off()

#PRINT MOTIF FREQUENCIES
pdf(paste("motifFreqs_",label,"_zoom2.pdf",sep=""), width=8, height= 4)
b=read.table(paste("motifFreqs_",label,".dat",sep=""))
plot(0,0,ylim=c(0,1),xlim=c(init,nrow(b)))
for(i in 1:limit){
  points(b[,i],col=i,cex=0.2)
  #lines(b[,i],col=i)
  
}
dev.off()


plot(0,0,ylim=c(0,1),xlim=c(0,100))
for(i in 1:1100){
  #points(b[,i],col=i,cex=0.2)
  lines(b[,i],col=i)
  
}

1 %% 100

a=read.table(paste("auxx",label,".dat",sep=""))
b=read.table(paste("aux",label,".dat",sep=""))


a[1,]
length(a)
count=0
plot(0,0,xlim=c(-1,1),ylim=c(0,10))
for(i in 1:nrow(a)){
  if(i %% 10 == 1){
    #lines(density(as.numeric(a[i,])),col=count)
    lines(density(as.numeric(b[i,])),col=count,lty=2)
    count= count +1
  }
}
i=50
lines(density(as.numeric(a[i,])),col=count)
lines(density(as.numeric(b[i,])),col=count,lty=2)


a=read.table(paste("auxxx",label,".dat",sep=""))
plot(a[,1],a[,2],xlim=c(0,1),ylim=c(0,1))
