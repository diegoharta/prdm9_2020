
m <- rbind(c(1, 1,2,2), c(3,3, 4,4),c(5,5,6,6))
layout(m)
par(mar = c(3, 5, 0.5, 0.5))


bplusArr=c(0.000005, 0.000008, 0.00001, 0.0000115, 0.000015, 0.00002)
for(i in 1:6){
  
  bplus=bplusArr[i]
#bplus=0.00002
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
plot(0,0,xlim=c(5,20),ylim=c(0,.6))
for(k in 1:20){
 points(k,pik[k],col=i,pch=20)
}
sum(pik)
text(10,.4,paste("bplus = ",bplus,sep=""))
text(10,.3,paste("bminus = ",bminus,sep=""))
}
