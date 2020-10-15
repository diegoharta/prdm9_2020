#This script is mean to generate a pdf file showing the speed with which znf array sizes change
#it generates one plot for every simulation which can be of several runs
#it takes data from the file histogramOfSizeOfZnfArray

simID="N1000_t400_s10m"
paths="~/Documents/Projects/PZIFE/C_scripts_and_data/dataFromCluster/TrialRun_2018_09_14/s10m"
arrayP=c(1)
arrayE=c(6,5,4)
arrayD=c(2,1,0)
arrayU=c(5,4,3,2)

setwd(paths)
label="prueba2"

# PRINT PDF
pdf(paste("evolutionOfZnfArraySize_",label,".pdf",sep=""), width=8, height= 4)

m <- rbind(c(1,2,3,4,5), c(6,7,8,9,10))
layout(m,heights = c(2,2))
par(mar = c(5,5, 2, 0.5))  



arr_color=c("#009E73", "#e79f00", "#0072B2", "#9ad0f3", "#D55E00", 
            "#CC79A7", "#F0E442","#000000")

tamany=c(1.2,1.5,1.8,2)


tipo=c(19,17,15,18,1)



m <- rbind(c(1, 1,1), c(2,2,2))
layout(m,heights = c(2,2))
par(mar = c(0.5,5, 0.5, 0.5))


ymax=60
count=0
plot(-100,-100,ylim=c(0,ymax),xlim=c(xmin,xmax),ylab="Size or array",xlab="",xaxt="n",las=2)
for(ii in 1:length(arrayP)){
  count=0
  # for(j in 1:length(arrayU)){
  for(kk in 1:length(arrayD)){
    for(ll in 1:length(arrayU)){ 
      countE=0
      for(oo in 1:length(arrayE)){
        
        p=arrayP[ii]
        X=arrayE[oo]
        D=arrayD[kk]
        C=D
        U=arrayU[ll]
        #U=C+2
        
        
        # if(count == 1 && i==1) {text(1.5,xmax-1,label,cex=1)}
        # er=("std_error.dat")
        # val=file.info(er)$size
        # if(is.na(val) == TRUE){
        #   fileName <- "std_output.txt"
        #   conn <- file(fileName,open="r")
        #   linn <-readLines(conn)
        #   eco=(linn[length(linn)])
        #   close(conn)
        #   substr(eco, 1,5)
        #   if(substr(eco,1,5)=="alpha"){
        
        
        
        label=paste("prueba_pzife_1.97_",simID,"_p",p,"_X",X,"_D",D,"_C",C,"_U",U,sep="")
        setwd(paste(paths,"/",label,sep=""))
        labelcito=paste("p",p,"_X",X,"_D",D,"_C",C,"_U",U,sep="")
        
        a=read.table(paste("histogramOfSizeOfZnfArray_",label,".dat",sep=""),header=FALSE)
        profile=read.table(paste("profile_",label,".dat",sep=""),header=TRUE)
        novA=a[,3:length(a)]
       
        
        avPorGenA=rowMeans(novA)
        maxis=100
        timeToZero=mat.or.vec(1,profile$Runs)
        numObs=(profile$Generations-profile$BurnIn)/profile$Interval
        for(ss in 1:profile$Runs){
          avPorGenANew=avPorGenA[((ss-1)*numObs+1):(ss*numObs)]
          atcors=(acf(avPorGenANew,lag.max=maxis,plot=FALSE))
          rr=0
          repeat{
            rr=rr+1
            timeToZero[ss]=rr
            if(atcors$acf[rr]<0 || rr == maxis){
              break
            }
          }
        }
        avsTimes=mean(timeToZero)
        points(((ii-1)*1000+((kk-1)*250)+((ll-1)*60)+(oo-1)*15),avsTimes,col=arr_color[ll],pch=tipo[kk],cex=tamany[oo])
        
        
      
        
      }}}}
# }}
dev.off()  



