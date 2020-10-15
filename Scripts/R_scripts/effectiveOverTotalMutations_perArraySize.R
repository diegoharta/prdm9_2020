

m <- rbind(c(1,2),c(3,4),c(5,5))
#      ,c(3,4),c(5,6),c(7,7))
layout(m,heights = c(2,2,0.5))
#layout(m)
par(mar = c(2,2, 0.5, 0.5))

cbbPalette <- c("#000000", "#009E73", "#e79f00", "#9ad0f3", "#0072B2", "#D55E00", 
                "#CC79A7", "#F0E442")

arr_color=c("tomato4","slateblue2","darkorchid3","violetred2","orangered","green")

arr_color <- c("#000000", "#009E73", "#e79f00", "#9ad0f3", "#0072B2", "#D55E00", 
                "#CC79A7", "#F0E442")

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
 "prueba_1.90-N100-K20-s1-c0-b1-C1a-p1-u2-X2-d3-longD-Z2-j6",
  #"prueba_1.90-N100-K20-s1-c0-b1-C1a-p1-u2-X2-d3-longD-Z3-j6",
  "prueba_1.90-N100-K20-s1-c0-b1-C1a-p1-u2-X2-d3-longD-Z4-j6",
 "prueba_1.92-N1000-K20-s1-C1a-p1-u2-X3-d1a-100.-Z3-j6-a"
  
)

kmin=6
kmax=20

efficiencyRateSube=mat.or.vec(length(labelArray),kmax-kmin-1)
efficiencyRateBaja=mat.or.vec(length(labelArray),kmax-kmin-1)
efficiencyRateGenConv=mat.or.vec(length(labelArray),kmax-kmin-1)
efficiencyRatePoint=mat.or.vec(length(labelArray),kmax-kmin-1)

for(i in 1:length(labelArray)){
  
  label=labelArray[i]
  setwd(paste("~/Documents/Projects/PZIFE/C_scripts_and_data/",label,sep=""))
  
  profile=read.table(paste("profile_",label,".dat",sep=""),header=TRUE)
  
  
  setwd(paste("~/Documents/Projects/PZIFE/C_scripts_and_data/",label,sep=""))
  
  profile=read.table(paste("profile_",label,".dat",sep=""),header=TRUE)
  
  

  a=read.table(paste("arraySizeChange_",label,".dat",sep=""))
  equis=seq(kmin,kmax-1,1)
  sube=a[1,]
  baja=a[2,]
  genconv=a[3,]
  point=a[4,]
  subeeff=a[5,]
  bajaeff=a[6,]
  genconveff=a[7,]
  pointeff=a[8,]
  
  length(sube)
  equis=seq(kmin,kmax-1,1)
  
  #propi=20
  relSube=mat.or.vec(kmax-kmin,1)
  relBaja=mat.or.vec(kmax-kmin,1)
 
  relGeneConv=mat.or.vec(kmax-kmin,1)
  relGeneConv2=mat.or.vec(kmax-kmin,1)
  relPoint=mat.or.vec(kmax-kmin,1)
  for(j in 1:(kmax-kmin)){
    relSube[j]=subeeff[j]/sube[j]
    relBaja[j]=bajaeff[j]/baja[j]
    relGeneConv[j]=genconveff[j]/genconv[j]
    relPoint[j]=pointeff[j]/point[j]
  }
  
  
  diffs1=mat.or.vec(kmax-kmin,1)
  
  for(j in 1:(kmax-kmin-1)){
    restaeffs=bajaeff[j]-subeeff[j+1]
    sumaeffs=bajaeff[j]+subeeff[j+1]
   
    diffs1[j]=restaeffs/sumaeffs
    
    
  }
  diffs[kmax-kmin]=0
  
  
  minimo=min(min(sube),min(baja))
  maximo=.7
 
  plot(equis,relSube,col=arr_color[1],pch=19,cex=tamanyo,ylim=c(0,maximo))
  points(equis,relBaja,col=arr_color[2],pch=19,cex=tamanyo)
  points(equis,relGeneConv,col=arr_color[3],pch=19)
  points(equis,relPoint,col=arr_color[4],pch=19)
  points(equis,diffs1,col=arr_color[5],pch=17)
  
  
  
  text(13,maximo-.1*maximo,label)
  text(15,maximo-.15*maximo,paste("C = ",profile$GeneConversionRate,sep = ""))
  text(15,maximo-.2*maximo,paste("Z = ",profile$RelevantZnf,sep = ""))
  
}
  
  plot.new()
  
  length(equis)
  legend("bottom",c("RelSube (from x to x+1)","RelBaja (from x+1 to x)","RelGeneConv","RelPoint","Diff SubeBaja"),ncol = 1, 
         col=c(arr_color[1],arr_color[2],arr_color[3],arr_color[4],arr_color[5]),pch=c(19,19,19,19,17))
  
  


  
  