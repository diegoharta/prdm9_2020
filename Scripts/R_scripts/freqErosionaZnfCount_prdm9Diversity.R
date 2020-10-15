label="prueba_1.61a2"


setwd(paste("~/Documents/Projects/PZIFE/C_scripts_and_data/",label,sep=""))

profile=read.table(paste("profile_",label,".dat",sep=""),header=TRUE)
summaryStats=mat.or.vec(2,10)

m <- rbind(c(1, 1,1), c(2, 2,2),c(3,3,3))
layout(m)
par(mar = c(0.5, 5, 0.5, 0.5))

a=read.table(paste("alleleFreqs_",label,".dat",sep=""))
c=read.table(paste("prdmDiversity_",label,".dat",sep=""))

#Prints mean fitness of active Prdm9 alleles. 
z=read.table(paste("activeMotifsFreq_",label,".dat",sep=""))
av = mat.or.vec(1,nrow(z))
RowM <- rowMeans(z[, -ncol(z)])
plot(RowM,ylab="Mean activity of PRDM9 active motifs",xaxt="n",xlab="")
prom=mean(RowM[(ncol(c)/2):ncol(c)])
abline(h=prom,col=2,lwd=2)
text(ncol(c)/3,.996,prom)
#title(sub=paste("effective mutation rates ",label,sep=""))
#legend("topright", legend=c("ZnF mutation","Point mutation"), 
#      col=c(1,2),lty=1,

#PRINT PRDM9 Diversity as Latrille et al 2017
#c=read.table(paste("prdmDiversity_",label,".dat",sep=""))
x=seq(1,nrow(a),1)
plot(x,c,log="y",type="l",ylab="PRDM9 diversity",xaxt="n",xlab="")
prom=mean(as.numeric(c[(ncol(c)/2):ncol(c)]))
abline(h=prom,col="red")
relDiversity=(as.numeric(c[(ncol(c)/2):ncol(c)]))
densDiv=density(relDiversity)
#plot(densDiv)

par(mar = c(5, 5, 0.5, 0.5))
#PRINT RECOMBINATION ACTIVITY AS LATRILLE ET AL 2017
d=read.table(paste("recombinationActivity_",label,".dat",sep=""))
plot(x,d,type="l",ylab="Recombination activity",xlab="Generations",sub=paste(label," N=",profile$PopulationSize.N.," Theta=",profile$Theta,
              " C=",profile$GeneConversionRate," E=",profile$ErosionRate," Alpha=",profile$Alpha,
              " bPlus=",profile$BirthRate," bMinus=",profile$DeathRate,sep=""))
prom=mean(as.numeric(d[(ncol(d)/2):ncol(d)]))
abline(h=prom,col="red")


f=read.table(paste("maxFrequency_",label,".dat",sep=""))
limit=length(f)

init=0
selected = mat.or.vec(limit,1)

m <- rbind(c(1, 1,1), c(2, 2,2),c(3,3,3))
layout(m)
par(mar = c(0.5, 5, 0.5, 0.5))

#PRINT ALLELE FREQUENCIES
#a=read.table(paste("alleleFreqs_",label,".dat",sep=""))
plot(-100,-100,ylim=c(0,1),xlim=c(init,nrow(a)),ylab="Frequency of PRDM alleles",xaxt="n",xlab="")
count=1
for(i in 1:limit){
  if(f[i] > 0.01){
     selected[count]=i #guarda las trayectorias que pasan el threshold
     lines(a[,i],col=i)
     count=count+1
  }
}

#PRINT MOTIF FREQUENCIES 
b=read.table(paste("motifFreqs_",label,".dat",sep=""))
plot(0,0,ylim=c(0.2,1),xlim=c(init,nrow(b)),ylab="Activity of Motif",xaxt="n",xlab="")
for(i in 1:(count-1)){
  points(b[,selected[i]],col=selected[i],cex=0.2)
  #points(b[,i],col=i,cex=0.2)
}


#PRINT ARRAY FINGER SIZE ASSOCIATED TO EACH ALLELE
znf=read.table(paste("alleleZnfArraySize_",label,".dat",sep=""))
par(mar = c(5, 5, 0.5, 0.5))
plot(0,0,ylim=c(5,20),xlim=c(init,nrow(znf)),ylab="Mean ZnF-array length",xlab="Generations",sub=paste(label," N=",profile$N," Theta=",
                                                                profile$Theta," C=",profile$GeneConversionRate," E=",profile$ErosionRate," Alpha=",profile$Alpha,
                                                                " bPlus=",profile$birthRate," bMinus=",profile$deathRate,sep=""))
for(i in 1:(count-1)){
  points(znf[,selected[i]],col=selected[i],cex=0.2)
  #points(b[,i],col=i,cex=0.2)
}

avznf=read.table(paste("znfCountStatistics_",label,".dat",sep=""))
x=seq(-1999,length(avznf)-2000,1)
lines(x,avznf)



m <- rbind(c(1, 1,1), c(2, 2,2),c(3,3,3))
layout(m)
par(mar = c(0.5, 5, 0.5, 0.5))

#Print histogram and density plot of znf count
relAvZnf=as.numeric(avznf[(length(avznf)/2):(length(avznf))])
h = hist(relAvZnf)
h$density = h$counts/sum(h$counts)
plot(h,freq=FALSE)
plot(density(relAvZnf,adjust=4))
densZnf=density(relAvZnf,adjust=3)
plot(densZnf)
#lines(x,trend,add=TRUE)


# 
# bplus=0.00002
# bminus=0.00001
# alpha=bplus/bminus
# kmin=5
# kmax =20
# suma=0
# for(k in kmin:kmax){
#   suma=suma+alpha^(k-kmin)/k
# }
# pikmin=1/(kmin*suma)
# cte=pikmin*kmin/(alpha^kmin)
# pik=mat.or.vec(1,kmax)
# for(k in kmin:kmax){
#   pik[k]=cte*(alpha^k)/k
# }
# plot(0,0,xlim=c(0,20),ylim=c(0,.5))
# for(k in 1:20){
#   points(k,pik[k],col=i,pch=20)
# }
# sum(pik)
# text(10,.4,bplus)
# text(10,.3,bminus)
# 
# 
# 
# kMin<-5
# kMax<-20
# xz=seq(kMin,kMax,1)
# suma=0
# for(i in kMin:kMax){
#   suma=suma+((alphaChange**(i-kMin))/i)
# }
# piKmin=1/(kMin*suma)
# Cte= piKmin*kMin*(1/(alphaChange**kMin))
# trend <- function(k){
#   Cte*(alphaChange**k)/k
# }
# 
# suma=0
# for(i in kMin:kMax){
#   suma=suma+trends(i)
# }
# sum(trends)
# trends=trend(xz)
# plot(x,trends)
# curve(x,trend,add=T)


