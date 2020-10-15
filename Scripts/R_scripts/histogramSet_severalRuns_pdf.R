

simID="N1000_t400_s10m"
paths="~/Documents/Projects/PZIFE/C_scripts_and_data/dataFromCluster/TrialRun_2018_09_14/s10m"
arrayP=c(1)
arrayE=c(6,5,4)
arrayD=c(2,1,0)
arrayU=c(5,4,3,2)

setwd(paths)
label="prueba2"

# PRINT PDF
pdf(paste("sizeOfArrayStats_",label,".pdf",sep=""), width=8, height= 9)

m <- rbind(c(1,2,3), c(4,5,6),c(7,8,9),c(10,11,12))
layout(m,heights = c(2,2,2,2))
par(mar = c(5,5, 2, 0.5))  


ymax=2
count=0
# plot(-100,-100,ylim=c(0,ymax),xlim=c(xmin,xmax),ylab="Size or array",xlab="",xaxt="n",las=2)
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
        longA=(profile$Generations-profile$BurnIn)/profile$Interval
        
        ymax=21
        xmin=0
        xmax=800
        # plot(-100,-100,ylim=c(0,ymax),xlim=c(xmin,xmax),ylab="AA Div. at Binding sites",xlab="",xaxt="n",las=2)
        # for(xx in 1:nrow(a)){
        #   histo=hist(as.numeric(novA[xx,]),breaks=c(5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5,13.5,14.5,15.5,16.5,17.5,18.5,19.5,20.5),
        #                                            plot=FALSE)
        #   for(yy in 1:20){
        #     points(xx,histo$mids[yy],cex=(histo$density[yy]*10))
        #   }
        #  
        # }
        # 
        
        
        
        kmin=6
        kmax=20
        histot=hist(as.numeric(unlist(novA)),xlim=c(kmin-.5,kmax+0.5),ylim=c(0,.22),freq=FALSE,
                    breaks=c(4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5,13.5,14.5,15.5,16.5,17.5,18.5,19.5,20.5),
                    main=labelcito)
        
        alpha=(profile$BirthRate)/profile$DeathRate
        sum(histot$density)
        Prop=profile$SampleSize*2*profile$Runs*(profile$Generations-profile$BurnIn)/20
        #Prop=1.1
        Prop=1
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
        
      }}}}
# }}
dev.off()  