#This file prints the znf array lengths of the new alleles formed.
# It compares de new alleles' array length with the average array length of the population at the moment it arises
# We can therefore see if the difference is positive, that the new allele has a shorter array than the average.
# The three plots aux, auxx and auxxx are because we separate do this test dependent on the type of mutation that has given rise to
#  the new allele (znf mutation, point mutation, gene conversion mutation).

label="prueba_1.82a-N1000-K20-c0-b1-C3-p2-u3-X3-d5"
label="prueba_1.92-N1000-K20-s1-C1a-p1-u2-X3-d1a-100.-Z3-j6-a"

setwd(paste("~/Documents/Projects/PZIFE/C_scripts_and_data/",label,sep=""))

profile=read.table(paste("profile_",label,".dat",sep=""),header=TRUE)

ymax=8
ymin=-8
ytext=6
m <- rbind(c(1, 1,1), c(2,2,2),c(3,3,3))
layout(m)
print(m)
par(mar = c(0.5, 5, 0.5, 0.5))

a=read.table(paste("aux",label,".dat",sep=""))
head(a)
x<-seq(1,nrow(a),1)
difs1=a[,1]-a[,2]
sum(difs1)
suma=0
count=0
for(j in (nrow(a)/2):nrow(a)){
  if(difs1[j]!=a[j,1]){
    suma=suma+difs1[j]
    count=count+1
  }
}
suma=suma/count
print(suma)
plot(x,difs1,col=1,ylim=c(ymin,ymax))
abline(h=0,col=3)
abline(h=suma,col=2)
text(nrow(a)/2,ytext,suma)
text(nrow(a)/3,ytext,"Znf mutation")

par(mar = c(0.5, 5, 0.5, 0.5))
b=read.table(paste("auxx",label,".dat",sep=""))
x<-seq(1,nrow(b),1)
difs1=b[,1]-b[,2]
sum(difs1)
suma=0
count=0
for(j in (nrow(b)/2):nrow(b)){
  if(difs1[j]!=b[j,1]){
    suma=suma+difs1[j]
    count=count+1
  }
}
suma=suma/count
print(suma)
plot(x,difs1,col=1,ylim=c(ymin,ymax))
abline(h=0,col=3)
abline(h=suma,col=2)
text(nrow(b)/2,ytext,suma)
text(nrow(b)/3,ytext,"Point mutation")

par(mar = c(5, 5, 0.5, 0.5))
a=read.table(paste("auxxx",label,".dat",sep=""))
head(a)
x<-seq(1,nrow(a),1)
difs1=a[,1]-a[,2]
sum(difs1)
suma=0
count=0
for(j in (nrow(a)/2):nrow(a)){
  if(difs1[j]!=a[j,1]){
    suma=suma+difs1[j]
    count=count+1
  }
}
suma=suma/count
print(suma)
plot(x,difs1,col=1,ylim=c(ymin,ymax),sub = label)
abline(h=0,col=3)
abline(h=suma,col=2)
text(nrow(a)/2,ytext,suma)
text(nrow(a)/3,ytext,"Gene conversion")

#PRINT ALONG TIME STARTING FROM BURNIN

a=read.table(paste("aux",label,".dat",sep=""))
x<-seq(1,nrow(a),1)
difs1=a[,1]-a[,2]
sum(difs1)
numEvents=nrow(a)
suma=0
count=0
for(j in (nrow(a)/2):nrow(a)){
  if(difs1[j]!=a[j,1]){
    suma=suma+difs1[j]
    count=count+1
  }
}
suma=suma/count
print(suma)
plot(-1000,-1000,ylim=c(ymin,ymax),xlim=c(0,profile$Generations-profile$BurnIn),xaxt="n",xlab="",ylab="Diff in array length")

ids=0
j=1
while(ids < profile$BurnIn){
  j=j+1 
  ids=a[j,4]
}
a=a[j:nrow(a),]
x=a[,4]-profile$BurnIn
difs1=difs1[j:length(difs1)]
length(x)
length(difs1)

points(x,difs1,col=1)
abline(h=suma,col=2)
text(profile$Generations/3,ytext,suma)
text(profile$Generations/4,ytext,"Znf mutation")
text(profile$Generations/2,ytext,paste("Events=",numEvents,sep=""))

b=read.table(paste("auxx",label,".dat",sep=""))
numEvents=nrow(b)
x<-seq(1,nrow(b),1)
difs1=b[,1]-b[,2]
sum(difs1)
suma=0
count=0
for(j in (nrow(b)/2):nrow(b)){
  if(difs1[j]!=b[j,1]){
    suma=suma+difs1[j]
    count=count+1
  }
}
suma=suma/count
plot(-1000,-1000,ylim=c(ymin,ymax),xlim=c(0,profile$Generations-profile$BurnIn),xaxt="n",xlab="",ylab="Diff in array length")

ids=0
j=1
while(ids < profile$BurnIn){
  j=j+1 
  ids=b[j,4]
}
b=b[j:nrow(b),]
x=b[,4]-profile$BurnIn
difs1=difs1[j:length(difs1)]
length(x)
length(difs1)

points(x,difs1,col=1)
abline(h=suma,col=2)
text(profile$Generations/3,ytext,suma)
text(profile$Generations/4,ytext,"Point mutation")
text(profile$Generations/2,ytext,paste("Events=",numEvents,sep=""))

a=read.table(paste("auxxx",label,".dat",sep=""))
numEvents=nrow(a)
head(a)
x<-seq(1,nrow(a),1)
difs1=a[,1]-a[,2]
sum(difs1)
suma=0
count=0
for(j in (nrow(a)/2):nrow(a)){
  if(difs1[j]!=a[j,1]){
    suma=suma+difs1[j]
    count=count+1
  }
}
suma=suma/count
plot(-1000,-1000,ylim=c(ymin,ymax),xlim=c(0,profile$Generations-profile$BurnIn),xlab="Generations after burn-in",ylab="Diff in array length",sub=label)

ids=0
j=1
while(ids < profile$BurnIn){
  j=j+1 
  ids=a[j,4]
}
a=a[j:nrow(a),]
x=a[,4]-profile$BurnIn
difs1=difs1[j:length(difs1)]
length(x)
length(difs1)

points(x,difs1,col=1)
abline(h=suma,col=2)
text(profile$Generations/3,ytext,suma)
text(profile$Generations/4,ytext,"Gene conversion")
text(profile$Generations/2,ytext,paste("Events=",numEvents,sep=""))







