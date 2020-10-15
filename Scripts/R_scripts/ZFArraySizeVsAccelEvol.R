#This script takes is to visualize the data from Baker and Przeworski 2017 showing accelerated evolution (measured as the fraction 
# of diversity present at DNA binding sites within each Znf array) if the prdm9 gene has its complete structure but not if it has a 
# truncated structure

setwd(paste("~/Documents/Projects/PZIFE/BakerEtAlData/",sep=""))

a=read.table(paste("NumZfsVsProportionAADiversityinbindingSites_noOnes.dat",sep=""),header = TRUE)

xlimTop=max(a$NumberZFs)
ylimTop=max(a$ProportionAAdiv)

plot(0,0,xlim=c(0,xlimTop),ylim=c(0,ylimTop),xlab="ZF array size",ylab="AA diversity at DNA binding sites")

#for(i in nrow(a)){
points(a$NumberZFs[a$PRDM9structure=="KRAB-SET"],a$ProportionAAdiv[a$PRDM9structure=="KRAB-SET"],col=2)
points(a$NumberZFs[a$PRDM9structure=="KRAB-SSXRD-SET"],a$ProportionAAdiv[a$PRDM9structure=="KRAB-SSXRD-SET"],col=3)
points(a$NumberZFs[a$PRDM9structure=="SET"],a$ProportionAAdiv[a$PRDM9structure=="SET"],col=4)
points(a$NumberZFs[a$PRDM9structure=="SSXRD-SET"],a$ProportionAAdiv[a$PRDM9structure=="SSXRD-SET"],col=5)

trend=lm(a$ProportionAAdiv[a$PRDM9structure=="KRAB-SSXRD-SET"]~a$NumberZFs[a$PRDM9structure=="KRAB-SSXRD-SET"])
trend$coefficients[2]
abline(a=trend$coefficients[1], b=trend$coefficients[2],col=3)

trend=lm(a$ProportionAAdiv[a$PRDM9structure=="SET"]~a$NumberZFs[a$PRDM9structure=="SET"])
trend$coefficients[2]
abline(a=trend$coefficients[1], b=trend$coefficients[2],col=4)




legend("topright",c("KRAB-SET","KRAB-SSXRD-SET","SET","SSXRD-SET"),
       ncol = 1, col=c(2,3,4,5),pch=1,cex=1)

source("~/Documents/Projects/CommonFunctions/Common_functions.R")
library(vioplot);

plot(c(1:10), c(1:10), axes=F, xlab="", ylab="", xlim=c(-5,0), ylim=c(0,30), col=NA, cex.main=2)
#mtext("Region", side = 1, line = 5, cex=2)
mtext(expression(paste("Difference in ",pi," between CN+ and CNr",sep="")), side = 2, line = 6.2 , cex=0.6)
#Violin(a$NumberZFs[a$PRDM9structure=="KRAB-SET"], .0001, arr_color[3], "black", -4, 1, "F", "F")
#points(-4, median(difsaaCEU), col="black",bg="black", pch=20, cex=2)
Violin(a$NumberZFs[a$PRDM9structure=="KRAB-SSXRD-SET"], .0001, arr_color[5], "black", -3, 1, "F", "F")
#points(-3, median(difsmmCEU), col="black",bg="black", pch=20, cex=2)
Violin(a$NumberZFs[a$PRDM9structure=="SET"], .0001, arr_color[2], "black", -2, 1, "F", "F")
points(-2, median(a$NumberZFs[a$PRDM9structure=="SET"]), col="black",bg="black", pch=20, cex=2)
Violin(a$NumberZFs[a$PRDM9structure=="SSXRD-SET"], .0001, arr_color[2], "black", -1, 1, "F", "F")
points(-1, median(a$NumberZFs[a$PRDM9structure=="SSXRD-SET"]), col="black",bg="black", pch=20, cex=2)

plot(density(a$NumberZFs[a$PRDM9structure=="KRAB-SSXRD-SET"]),col=3,xlim=c(-1,40),ylim=c(0,.3))
lines(density(a$NumberZFs[a$PRDM9structure=="SET"]),col=4)
lines(density(a$NumberZFs[a$PRDM9structure=="SSXRD-SET"]),col=5)


abline(h=0,col=arr_color[6], lty=1, lwd=1)
axis(1, at = c(-4,-3,-2), labels=NA, las=1, cex.axis=.8, tck=-0.02)
axis(1, at = c(-4,-3,-2), labels=c("5' region","CNV region","3' region"), las=1, cex.axis=1.8, tck=0, lty=0, line=1.2)
axis(2, at = seq(-.001,.0045, 0.001), labels=NA, las=1, cex.axis=1, tck=-0.02)
axis(2, at = seq(-.001,.0045, 0.001), las=1, cex.axis=1.5, tck=0, lty=0, line=0.5)
par(xpd=TRUE)
#legend(-3.5, -0.0035, c("5' region","CNV region","3' region"), pch=c(15,15,15), pt.cex=3, text.col="black", col=c(arr_color[3],arr_color[5],arr_color[2]), xjust=0.5, bty = "n", cex=2)     
#legend(-2.3, -0.0035, c("CEU","CHB","YRI"), pch=c(1,2,3), pt.cex=3, text.col="black", col=c(arr_color[1]), xjust=0.5, bty = "n", cex=2)     
#polygon(c(-4,-2,-2,-4), c(-0.0045, -0.0045, -0.0035, -0.0035))
par(xpd=FALSE)

