
a=read.table("profile_prueba5.dat")
a[1,]
b=density(as.numeric(a[1,]),adjust=2)
plot(b,col=1,xlim=c(0,20))

for(i in 2:10){
  b=density(as.numeric(a[i,]),adjust = 2)
  lines(b,col=i,add=TRUE)
}

b=density(as.numeric(a[,1]),adjust=2)
plot(b,col=1,xlim=c(3,20))
abline(v=mean(a[,1]),add=TRUE)



a=read.table("div_prueba-045-3-d.dat")
plot(0,0,xlim=c(0,1000),ylim=c(0,4))
lines(a[,1])
for(i in 2:7){
  lines(a[,i],col=i,add=TRUE)
}


a=read.table("countPrint_prueba5.dat")
plot(0,0,xlim=c(0,100),ylim=c(0,40))
lines(a[,1])
for(i in 2:20){
  lines(a[,i],col=i,add=TRUE)
}



a=read.table("profile_prueba5.dat")
b=density(as.numeric(a[1,]))
plot(a,col=1,xlim=c(0,20))


