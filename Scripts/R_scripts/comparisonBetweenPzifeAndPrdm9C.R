setwd("~/Documents/Projects/PZIFE/C_scripts_and_data/data2FromCluster/TR_2019_02_04/")
a=read.table("comparisonData_C0.dat",header=TRUE)


m <- rbind(c(1,1,2,2), c(3,3,4,4),c(5,5,6,6))
layout(m,heights = c(2,2,2))

par(mar = c(0.5,5, 0.5, 0.5))
plot(a$meanrec,xaxt="n")
datos1=a[1:5,]$meanrec
mean(datos1)
datos2=a[6:10,]$meanrec
abline(h=mean(datos1),col=2)
abline(h=mean(datos2),col=3)
boxplot(datos1,datos2)
     
par(mar = c(0.5,5, 0.5, 0.5))
plot(a$meanclassdiv,xaxt="n")
datos1=a[1:5,]$meanclassdiv
mean(datos1)
sd(datos1)
datos2=a[6:10,]$meanclassdiv
sd(datos2)
abline(h=mean(datos1),col=2)
abline(h=mean(datos2),col=3)
boxplot(datos1,datos2)

par(mar = c(0.5,5, 0.5, 0.5))
plot(a$tau2,xaxt="n")
datos1=a[1:5,]$tau2
mean(datos1)
datos2=a[6:10,]$tau2
abline(h=mean(datos1),col=2)
abline(h=mean(datos2),col=3)
boxplot(datos1,datos2)

par(mar = c(0.5,5, 0.5, 0.5))
plot(a$tau2div,xaxt="n")
datos1=a[1:5,]$tau2div
mean(datos1)
datos2=a[6:10,]$tau2div
abline(h=mean(datos1),col=2)
abline(h=mean(datos2),col=3)
boxplot(datos1,datos2)

par(mar = c(0.5,5, 0.5, 0.5))
plot(a$tau,xaxt="n")
datos1=a[1:5,]$tau
mean(datos1)
datos2=a[6:10,]$tau
abline(h=mean(datos1),col=2)
abline(h=mean(datos2),col=3)
boxplot(datos1,datos2)

par(mar = c(0.5,5, 0.5, 0.5))
plot(a$taudiv,xaxt="n")
datos1=a[1:5,]$taudiv
mean(datos1)
datos2=a[6:10,]$taudiv
abline(h=mean(datos1),col=2)
abline(h=mean(datos2),col=3)
boxplot(datos1,datos2)
