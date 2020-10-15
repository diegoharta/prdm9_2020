label="prueba_1.52x10"
label="prueba_1.81-N1000-K20-c0-b1-C1-p1-u3-X2"

setwd(paste("~/Documents/Projects/PZIFE/C_scripts_and_data/",label,sep=""))

#pdf(paste("activeMotifsFreq_",label,".pdf",sep=""), width=8, height= 4)

a=read.table(paste("activeMotifsFreq_",label,".dat",sep=""))
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
prom=mean(RowM[3000:5000])
abline(h=prom,col=2,lwd=2)
text(1000,.996,prom)
#title(sub=paste("effective mutation rates ",label,sep=""))
#legend("topright", legend=c("ZnF mutation","Point mutation"), 
#      col=c(1,2),lty=1,


f=read.table(paste("maxFrequency_",label,".dat",sep=""))




limit = 400
init=0
selected = mat.or.vec(limit,1)
colMax <- function(data) sapply(data, max, na.rm = TRUE)

#PRINT ALLELE FREQUENCIES
a=read.table(paste("alleleFreqs_",label,".dat",sep=""))

prom=colMeans(a)
maxim = colMax(a)
plot(0,0,ylim=c(0,1),xlim=c(init,nrow(a)))
count=1
for(i in 1:limit){
  #if(prom[i] > 0.001){
   # selected[count]=i #guarda las trayectorias que pasan el threshold
    lines(a[,i],col=i)
  #  count=count+1
  #}
}
#plot(0,0,ylim=c(0,1),xlim=c(init,nrow(a)))
#for(i in 1:limit){
#  if(maxim[i] > 0.1){
#    selected[count]=i #guarda las trayectorias que pasan el threshold
#    lines(a[,i],col=i)
#    count=count+1
#  }
#}


#PRINT MOTIF FREQUENCIES 
b=read.table(paste("motifFreqs_",label,".dat",sep=""))
#plot(0,0,ylim=c(0,1),xlim=c(init,nrow(b)))
#for(i in 1:(count-1)){
  for(i in 1:limit){
    
    #lines(b[,selected[i]],col=selected[i],cex=0.2)
  points(b[,i],col=i,cex=0.2)
}

anyos=read.table(paste("interestingYears_",label,".dat",sep=""))
anyos=anyos[,1]
length(anyos)
anyos[1]
x=seq(1,nrow(anyos),1)
for(i in 1:length(anyos)){
  abline(v=as.numeric(anyos[i]-2000))
}

#PRINT PRDM9 Diversity as Latrille et al 2017
#c=read.table(paste("prdmDiversity_",label,".dat",sep=""))
#x=seq(1,nrow(a),1)
#plot(x,c,log="y",type="l",ylab="PRDM9 diversity")

#PRINT RECOMBINATION ACTIVITY AS LATRILLE ET AL 2017
#d=read.table(paste("recombinationActivity_",label,".dat",sep=""))
#plot(x,d,type="l",ylab="Recombination activity")
#prom=mean(as.numeric(d[(ncol(d)/2):ncol(d)]))
#abline(h=prom,col="red")

#PRINT ALLELE FREQUENCIES TO PDF
pdf(paste("alleleFreqs_",label,"_ZOOM2.pdf",sep=""), width=8, height= 4)
#a=read.table(paste("alleleFreqs_",label,".dat",sep=""))
#maxim = colMax(a)
plot(0,0,ylim=c(0,1),xlim=c(init,nrow(a)),ylab="Recombination activity")
#count=1
for(i in 1:limit){
 # if(maxim[i] > 0.2){
  #  selected[count]=i #guarda las trayectorias que pasan el threshold
    lines(a[,i],col=i)
   # count=count+1
#  }
}
dev.off()

#PRINT MOTIF FREQUENCIES TO PDF
pdf(paste("motifFreqs_",label,"_zoom2.pdf",sep=""), width=8, height= 4)
#b=read.table(paste("motifFreqs_",label,".dat",sep=""))
plot(0,0,ylim=c(0,1),xlim=c(init,nrow(b)))
for(i in 1:limit){
  
  #points(b[,selected[i]],col=selected[i],cex=0.2)
  points(b[,i],col=i,cex=0.2)
  
}
dev.off()


#pdf(paste("activeMotifsFreq_",label,".pdf",sep=""), width=8, height= 4)
#plot(RowM)
#prom=mean(RowM[3000:5000])
#abline(h=prom,col=2,lwd=2)
#text(1000,.996,prom)  #     bty="n")
#dev.off()


