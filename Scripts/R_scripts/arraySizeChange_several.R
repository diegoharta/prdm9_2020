

m <- rbind(c(1,2),c(3,4),c(5,5))
     #      ,c(3,4),c(5,6),c(7,7))
layout(m,heights = c(2,2,0.5))
#layout(m)
par(mar = c(2,2, 0.5, 0.5))


arr_color=c("slateblue2","tomato4","darkorchid3","violetred2","orangered")

labelArray=c(
  
"prueba_1.87-N100-K20-s1-c0-b1-C0a-p1-u2-X2-d3-longC-Z4-j6",
  "prueba_1.87-N100-K20-s1-c0-b1-C0.1a-p1-u2-X2-d3-longC-Z4-j6",
 "prueba_1.87-N100-K20-s1-c0-b1-C1a-p1-u2-X2-d3-longC-Z4-j6",
"prueba_1.87-N100-K20-s1-c0-b1-C10a-p1-u2-X2-d3-longC-Z4-j6",
 "prueba_1.87-N100-K20-s1-c0-b1-C0a-p0-u2-X2-d3-longE-Z4-j6-b",
"prueba_1.87-N100-K20-s1-c0-b1-C1a-p0-u2-X2-d3-longE-Z4-j6-b",
"prueba_1.87-N100-K20-s1-c0-b1-C10a-p0-u2-X2-d3-longE-Z4-j6",
"prueba_1.87-N100-K20-s1-c0-b1-C0a-p1-u2-X2-d3-longC-Z3-j6",
"prueba_1.87-N100-K20-s1-c0-b1-C1a-p1-u2-X2-d3-longC-Z3-j6",
"prueba_1.87-N100-K20-s1-c0-b1-C10a-p1-u2-X2-d3-longC-Z3-j6"
 
)



labelArray=c(
  
  "prueba_1.88-N100-K20-s1-c0-b1-C0a-p0-u2-X2-d3-longD-Z4-j6",
  "prueba_1.88-N100-K20-s1-c0-b1-C10a-p0-u2-X2-d3-longD-Z4-j6",
  "prueba_1.88-N100-K20-s1-c0-b1-C0a-p1-u2-X2-d3-longD-Z4-j6",
  "prueba_1.88-N100-K20-s1-c0-b1-C10a-p1-u2-X2-d3-longD-Z4-j6"
  
)

labelArray=c(
  
  "prueba_1.88-N100-K20-s1-c0-b1-C0a-p1-u2-X2-d3-longD-Z3-j6",
  "prueba_1.88-N100-K20-s1-c0-b1-C10a-p1-u2-X2-d3-longD-Z3-j6",
  "prueba_1.88-N200-K20-s1-c0-b1-C0a-p1-u2-X2-d3-longD-Z3-j6",
  "prueba_1.88-200-K20-s1-c0-b1-C10a-p1-u2-X2-d3-longD-Z3-j6"
  
)


labelArray=c(
  #No selection / selection && No conversion / Conversion
  "prueba_1.88-N100-K20-s1-c0-b1-C0a-p0-u2-X2-d3-longD-Z3-j6",
  "prueba_1.88-N100-K20-s1-c0-b1-C10a-p0-u2-X2-d3-longD-Z3-j6",
  "prueba_1.88-N100-K20-s1-c0-b1-C0a-p1-u2-X2-d3-longD-Z3-j6",
  "prueba_1.88-N100-K20-s1-c0-b1-C10a-p1-u2-X2-d3-longD-Z3-j6"
  
)




labelArray=c(

  "prueba_1.89-N100-K20-s1-c0-b1-C10a-p1-u2-X2-d3-longA-Z4-j6"
  
)

labelArray=c(
  #C range 
  "prueba_1.89-N100-K20-s1-c0-b1-C0a-p1-u2-X2-d3-longD-Z3-j6",
  "prueba_1.89-N100-K20-s1-c0-b1-C0.1a-p1-u2-X2-d3-longD-Z3-j6",
  "prueba_1.89-N100-K20-s1-c0-b1-C1a-p1-u2-X2-d3-longD-Z3-j6",
  "prueba_1.89-N100-K20-s1-c0-b1-C10a-p1-u2-X2-d3-longD-Z3-j6"
  
)

labelArray=c(
  "prueba_1.88-N100-K20-s1-c0-b1-C10a-p1-u2-X2-d3-longD-Z3-j6-doubleCXudx",
  "prueba_1.88-N100-K20-s1-c0-b1-C10a-p1-u2-X2-d3-longD-Z3-j6-halfCXudx",
  "prueba_1.88-N100-K20-s1-c0-b1-C10a-p1-u2-X2-d3-longD-Z3-j6"
)




labelArray=c(
  #RelZnf3 / RelZnf4 && No conversion / Conversion
  "prueba_1.87-N100-K20-s1-c0-b1-C0a-p1-u2-X2-d3-longC-Z3-j6",
  "prueba_1.87-N100-K20-s1-c0-b1-C10a-p1-u2-X2-d3-longC-Z3-j6",
  "prueba_1.87-N100-K20-s1-c0-b1-C0a-p1-u2-X2-d3-longC-Z4-j6",
  "prueba_1.87-N100-K20-s1-c0-b1-C10a-p1-u2-X2-d3-longC-Z4-j6"
  
)

labelArray=c(
  "prueba_1.88-N100-K20-s1-c0-b1-C10a-p1-u2-X2-d3-longD-Z3-j6",
 # "prueba_1.88-N100-K20-s1-c0-b1-C10a-p1-u3-X2-d3-longD-Z3-j6",
  "prueba_1.88-N100-K20-s1-c0-b1-C10a-p1-u4-X2-d3-longD-Z3-j6"
)


labelArray=c(
  "prueba_1.88-N100-K20-s1-c0-b1-C10a-p1-u2-X2-d3-longD-Z4-j6", 
  "prueba_1.88-N100-K20-s1-c0-b1-C10a-p1-u3-X2-d3-longD-Z4-j6", 
  "prueba_1.88-N100-K20-s1-c0-b1-C10a-p1-u4-X2-d3-longD-Z4-j6"
)

labelArray=c(
  "prueba_1.90-N100-K20-s1-c0-b1-C10a-p1-u2-X2-d3-longD-Z4-j6",
  
  "prueba_1.90-N100-K20-s1-c0-b1-C1a-p1-u3-X2-d3-longD-Z4-j6",
  
  "prueba_1.90-N100-K20-s1-c0-b1-C0.1a-p1-u4-X2-d3-longD-Z4-j6",
  
  "prueba_1.90-N100-K20-s1-c0-b1-C0.01a-p1-u5-X2-d3-longD-Z4-j6"
  )



for(i in 1:length(labelArray)){

  label=labelArray[i]
setwd(paste("~/Documents/Projects/PZIFE/C_scripts_and_data/",label,sep=""))

profile=read.table(paste("profile_",label,".dat",sep=""),header=TRUE)


setwd(paste("~/Documents/Projects/PZIFE/C_scripts_and_data/",label,sep=""))

profile=read.table(paste("profile_",label,".dat",sep=""),header=TRUE)


kmin=6
kmax=20
a=read.table(paste("arraySizeChange_",label,".dat",sep=""))
equis=seq(kmin,kmax-1,1)
sube=a[1,]
baja=a[2,]
minimo=min(min(sube),min(baja))
maximo=max(max(sube),max(baja))
tamanyo=1.4
plot(equis,sube,col=arr_color[1],pch=15,cex=tamanyo, ylim = c(minimo,maximo+.04*maximo))
points(equis,baja,col=arr_color[2],pch=15,cex=tamanyo)
text(12,maximo+.03*maximo,label)
text(15,maximo,paste("C = ",profile$GeneConversionRate,sep = ""))

subeeff=a[5,]
bajaeff=a[6,]
#geneconveff=a[5,]
#pointeff=a[6,]
#minimo=min(min(subeeff),min(bajaeff),min(geneconveff),min(pointeff))
#maximo=max(max(subeeff),max(bajaeff),max(geneconveff),max(pointeff))
minimo=min(min(subeeff),min(bajaeff))
maximo=max(max(subeeff),max(bajaeff))

plot(equis,subeeff,col=arr_color[1],pch=19,cex=tamanyo,ylim = c(minimo,maximo+.2*maximo))
points(equis,bajaeff,col=arr_color[2],pch=19,cex=tamanyo)


#points(equis,geneconveff,col=arr_color[3],pch=19,cex=tamanyo)
#points(equis,pointeff,col=arr_color[4],pch=19,cex=tamanyo)


text(12,maximo+.1*maximo,label)
text(12,maximo,paste("C = ",profile$GeneConversionRate,sep = ""))


summaryStats=mat.or.vec(2,10)

a=read.table(paste("histogramOfSizeOfZnfArray_",label,".dat",sep=""))
novA=a[,3:length(a)]
kmin=6
kmax =20

histot=hist(as.numeric(unlist(novA)),xlim=c(kmin-.5,kmax+0.5),ylim=c(0,.22),freq=FALSE,breaks=c(4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5,13.5,14.5,15.5,16.5,17.5,18.5,19.5,20.5), main="")
Prop=profile$SampleSize*2*profile$Runs*(profile$Generations-profile$BurnIn)/20
Prop=1
alpha=(profile$BirthRate)/profile$DeathRate

suma=0
for(k in kmin:kmax){
  suma=suma+alpha^(k-kmin)/k
}
pikmin=1/(kmin*suma)
cte=Prop*pikmin*kmin/(alpha^kmin)
pik=mat.or.vec(1,kmax)
for(k in kmin:kmax){
  pik[k]=cte*(alpha^k)/k
}

#plot(0,0,xlim=c(5,20),ylim=c(0,.6))
for(k in kmin:kmax){
  points(k,pik[k],col=2,pch=20)
}
sum(pik)
#text(10,.15*Prop,paste("bplus = ",profile$BirthRate,sep=""))
#text(10,.14*Prop,paste("bminus = ",profile$DeathRate,sep=""))
#text(10,.13*Prop,paste("alpha = ",profile$Alpha,sep=""))
tamanyo=0.7

text(15,.17*Prop,label,cex=tamanyo)
text(15,.16*Prop,paste("N = ",profile$PopulationSize,sep=""),cex=tamanyo)
#text(10,.15*Prop,paste("bplus = ",profile$BirthRate,sep=""),cex=tamanyo)
#text(10,.14*Prop,paste("bminus = ",profile$DeathRate,sep=""),cex=tamanyo)
text(15,.15*Prop,paste("alpha = ",profile$Alpha,sep=""),cex=tamanyo)
text(15,.14*Prop,paste("C = ",profile$GeneConversionRate,sep=""),cex=tamanyo)
#text(10,.11*Prop,paste("time = ",profile$Generations,sep=""),cex=tamanyo)
#text(10,.10*Prop,paste("runs = ",profile$Runs,sep=""),cex=tamanyo)

#PRDM9 DIVERSITY
c=read.table(paste("prdmDiversity_",label,".dat",sep=""))
prom=mean(as.numeric(c[1,(ncol(c)/2):ncol(c)]))
text(15,.11*Prop,paste("Dvsty = ",prom,sep=""),cex=tamanyo)

#PRINT RECOMBINATION ACTIVITY AS LATRILLE ET AL 2017
d=read.table(paste("recombinationActivity_",label,".dat",sep=""))
prom=mean(as.numeric(d[1,(ncol(d)/2):ncol(d)]))
text(15,.10*Prop,paste("Actvty = ",prom,sep=""),cex=tamanyo)

#PRINT selection coefficient as Latrille et al 2017
e=read.table(paste("selectionCoefficient_",label,".dat",sep=""),header=TRUE)
prom=mean(as.numeric(e[(nrow(e)/2):nrow(e),2]))
#text(15,.13*Prop,prom,cex=tamanyo)
selValue=4*profile$PopulationSize*prom
text(15,.12*Prop,paste("4Ns = ",selValue,sep=""),cex=tamanyo)



bStats=read.table(paste("generalStatistics_",label,".dat",sep=""))
for(k in 1:8){
  gralStats[i,k]=unlist(as.numeric(bStats[k]))
}
mutRate[i]=gralStats[i,7]/((profile$Generations-profile$BurnIn))
text(15,.09*Prop,bquote(paste(u," = ",.(mutRate[i]),sep="")),cex=tamanyo)
epsilonRatio[i]=profile$ErosionRate/mutRate[i]
text(15,.08*Prop,bquote(paste(epsilon," = ",.(epsilonRatio[i]),sep="")),cex=tamanyo)

####################
#propi=20
#relBaja=mat.or.vec(kmax-kmin-1,1)
#relSube=mat.or.vec(kmax-kmin-1,1)
#relGeneConv=mat.or.vec(kmax-kmin-1,1)
#relPoint=mat.or.vec(kmax-kmin-1,1)
#for(j in 1:(kmax-kmin-1)){
#  relBaja[j]=bajaeff[j]/(histot$counts[j+1])*propi
#  relSube[j]=subeeff[j]/(histot$counts[j+1])*propi
#  relGeneConv[j]=geneconveff[j]/(histot$counts[j+1])
#  relPoint[j]=pointeff[j]/(histot$counts[j+1])*propi
#}

#plot(equis2,relGeneConv,col=arr_color[3],pch=19)

#points(equis2,relSube,col=arr_color[1],pch=19)
#points(equis2,relBaja,col=arr_color[2],pch=19)
#points(equis2,relPoint,col=arr_color[4],pch=19)


diffs1=mat.or.vec(kmax-kmin-1,1)
diffs2=mat.or.vec(kmax-kmin-1,1)
diffs3=mat.or.vec(kmax-kmin-1,1)
for(j in 1:(kmax-kmin-1)){
  restaeffs=bajaeff[j]-subeeff[j+1]
  sumaeffs=baja[j]+sube[j+1]
  diffs1[j]=bajaeff[j]/(histot$counts[j+2])
  diffs2[j]=subeeff[j+1]/(histot$counts[j+2])
  diffs3[j]=(bajaeff[j]-subeeff[j+1])/(histot$counts[j+2])
  diffs3[j]=restaeffs/sumaeffs

}

plot(equis2,diffs1,col=arr_color[3],ylim=c(0,0.5))

points(equis2,diffs2,col=arr_color[4])
points(equis2,diffs3,col=arr_color[5],pch=19)
abline(h=0,col=2)


plot.new()

length(equis)
legend("bottom",c("from x to x+1","from x+1 to x","all","effective"),ncol = 4, col=c(arr_color[1],arr_color[2],1,1),
       pch=c(NA,NA,15,19),lty=c(1,1,NA,NA),lwd=c(2,2,NA,NA))


}
