### This file is intended to print the relationship between potentiality and activity


# PRINT PDF
label="N400_t100_s1u"
paths="~/Documents/Projects/PZIFE/C_scripts_and_data/dataFromCluster/TrialRun_2018_10_08/s1u"
setwd(paths)
#pdf(paste("newAllelePotentiality_",label,".pdf",sep=""), width=8, height= 9)

simID=paste("prueba_pzife_1.98_",label,sep="")
arrayP=c(2)
arrayE=c(2)
arrayD=c(0)
arrayU=c(2)

ymaxfreq=1
ymaxPot=0.005

init=0
inicio=21
final=30

arr_color=c("#009E73", "#e79f00", "#0072B2", "#9ad0f3", "#D55E00", 
            "#CC79A7", "#F0E442","#000000")

tamany=c(1.2,1.5,1.8,2)
tamany=c(1,1,1,1,1)

m <- rbind(c(1,1,2,2),c(3,3,4,4))
#,c(6,6,7,7))
layout(m,heights = c(4,4))
par(mar = c(0.5,5, 0.5, 0.5))
#,xaxs="i")

tipo=c(22,23,24,25,1)
tipo2=c(15,16,17,18)
tipo3=c(7,8,9,10)

puntos=c()
varian=c()
meanE=c()
countE=c()
i=1

par(mar = c(5,5, 0.5, 0.5),xaxs="i")
ymax=1
countP=0

contador=0
for(ii in 1:length(arrayP)){
  countP=0
  # for(j in 1:length(arrayU)){
  for(kk in 1:length(arrayD)){
    for(ll in 1:length(arrayU)){ 
      countE=0
      for(oo in 1:length(arrayE)){
        contador=contador+1
        p=arrayP[ii]
        X=arrayE[oo]
        D=arrayD[kk]
        C=D
        U=arrayU[ll]
        #U=C+2
        label=paste(simID,"_p",p,"_X",X,"_D",D,"_C",C,"_U",U,sep="")
        setwd(paste(paths,"/",label,sep=""))
        
        
        #if(count == 1 && i==1) {text(1.5,xmax-1,label,cex=1)}
        er=("std_error.dat")
        val=file.info(er)$size
        if(is.na(val) == TRUE){
          fileName <- "std_output.txt"
          conn <- file(fileName,open="r")
          linn <-readLines(conn)
          eco=(linn[length(linn)])
          close(conn)
          substr(eco, 1,5)
          if(substr(eco,1,5)=="alpha"){
            
        #     newAllele=read.table(paste("novelAllelePotentiality_",label,".dat",sep=""))
        #     
        #     newActivity=read.table(paste("novelAlleleActivity_",label,".dat",sep=""))
        #     
        #     promAct=rowMeans(newActivity)
        #     promPot=rowMeans(newAllele)
        #     avAct=mean(promAct)
        #     avPot=mean(promPot)
        #     
        #     nrow(newAllele)
        #     plot(100,100,xlim=c(0.8,1.05),ylim=c(0.00005,.1),log="y",xlab="Activity",ylab="Potentiality")
        #     for(jj in 1:50){
        #       for(kk in 1:20){
        #         
        #         points(newActivity[5000+jj*100,kk],newAllele[jj,kk],col=jj)
        #         #points(promAct[jj],promPot[jj],col=arr_color[oo])
        #         
        #       }
        #       
        #     }
        #     abline(v=avAct,col=arr_color[oo])
        #     abline(h=avPot,col=arr_color[oo])
        #     
        #     
        #     
        #   }
        # }
            
            #Read files
            a=read.table(paste("alleleFreqs_",label,".dat",sep=""))
            b=read.table(paste("motifActivity_",label,".dat",sep=""))
            c=read.table(paste("alleleZnfArraySize_",label,".dat",sep=""))
            d=read.table(paste("referenceOfActiveAlleles_",label,".dat",sep=""))
            e=read.table(paste("allelePotentiality_",label,".dat",sep=""))

            profile=read.table(paste("profile_",label,".dat",sep=""),header=TRUE)


            uq_elem=c()
            for(i in 1:ncol(d))
            {
              uq_elem=c(unique(d[,i]), uq_elem)
              uq_elem=unique(uq_elem)
            }
            sortUq=sort(uq_elem)


            colMax <- function(X) apply(X, 2, max)
            selected=mat.or.vec(ncol(a),1)

            # Create newA, newB and newC from reference allele list in d

            newA=mat.or.vec(nrow(a),length(sortUq))
            newB=mat.or.vec(nrow(a),length(sortUq))
            newC=mat.or.vec(nrow(a),length(sortUq))
            newE=mat.or.vec(nrow(a),length(sortUq))
            for(i in 2:length(sortUq)){
              for(k in 1:nrow(a)){
                #for(j in 1:ncol(a)){

                #
                j=1
                #repeat{
                while(d[k,j]!=0 && d[k,j]<=sortUq[i] && j<=(length(sortUq)-1)){
                  if(d[k,j]==sortUq[i]){
                    newA[k,i-1]=a[k,j]
                    newB[k,i-1]=b[k,j]
                    newC[k,i-1]=c[k,j]
                    newE[k,i-1]=e[k,j]/2/profile$PopulationSize/a[k,j]
                  }
                  j=j+1
                  #if(d[k,j]==0){break}
                  #if(d[k,j]>=sortUq[i]){
                  #  break
                  #}
                }
                #}
              }
            }

            
            #PRINTS ALLELE FREQUENCIES
            maxim = colMax(newA)
            init=0
            par(mar = c(0.55,5, 0.5, 0.5),xaxs="i")
            plot(-100,-100,ylim=c(0,ymaxfreq),xlim=c(init,nrow(a)),ylab="Allele Frequencies",xaxt="n",xlab="")
            count=1
            for(i in 1:length(maxim)){
              if(maxim[i] > 0.1){
               
                selected[count]=i #guarda las trayectorias que pasan el threshold
                lines(newA[,i],col=count)
                count=count+1
              }
            }
            
            #PRINTS MOTIF ACTIVITY
            par(mar = c(0.5,5, 0.5, 0.5))
            plot(100,1000,ylim=c(0,1),xlim=c(init,nrow(b)),ylab="Motif Activity", xaxt="n",xlab="")
            for(i in 1:(count-1)){
              for(j in 1:nrow(newB)){
                points(j,newB[j,selected[i]],col=i,cex=0.4)
                #points(newB[,i],col=i,cex=0.2)
              }
            }
            for(ij in 1:nrow(newB)){
              meanAct=0
              cuentas=0
              for(ji in 1:ncol(newB)){
                if(newB[ij,ji]!=0){
                  meanAct=meanAct+newB[ij,ji]
                  cuentas=cuentas+1
                }
              }
              points(ij,meanAct/cuentas,col=1,pch=4)
            }
          
            # par(mar = c(5,5, 0.5, 0.5))
            # plot(100,1000,ylim=c(0.001,0.04),xlim=c(init,nrow(e)),ylab="Allele Potentiality", xlab="Generations")
            # for(i in 1:(count-1)){
            # #  for(j in 1:nrow(newE)){
            #    # correccion=/2/profile$PopulationSize/newA[j,selected[i]]
            #     #points(j,newE[j,selected[i]],col=i,cex=0.4)
            #     #points(newB[,i],col=i,cex=0.2)
            #     points(newE[,selected[i]],col=i)
            #     
            #  # }
            # }
            
            vectornewE=c()
            vecotrnewEquis=c()
            par(mar = c(5,5, 0.5, 0.5))
            plot(100,1000,ylim=c(0.0001,ymaxfreq),xlim=c(0.4,1.01),ylab="Allele frequencies",xlab="Activity")
            for(i in 1:(count-1)){
              
              meanE=0
              countnewE=0
              for(j in 1:nrow(newE)){
                # correccion=newE[j,selected[i]]/2/profile$PopulationSize/newA[j,selected[i]]
                points(newB[j,selected[i]],newA[j,selected[i]],col=i,cex=.4)
                if(newE[j,selected[i]]!=0){
                  meanE=meanE+newE[j,selected[i]]
                  countnewE=countnewE+1
                  
                }            
                #points(newB[,i],col=i,cex=0.2)
              }
             # lines(newB[,selected[i]],newA[,selected[i]],col=i,cex=1)
             points(newB[,selected[i]],newE[,selected[i]],col=i,cex=1) 
              # abline(h=(meanE/countnewE),col=i)
              
            }
            
            par(mar = c(5,5, 0.5, 0.5))
            plot(100,1000,ylim=c(0,0.2),xlim=c(0,1.01),ylab="Allele Potentiality",xlab="Activity")
            for(i in 1:(count-1)){
              vectornewE=c()
              vectornewEquis=c()
              meanE=0
              countnewE=0
             for(j in 1:nrow(newE)){
               # correccion=newE[j,selected[i]]/2/profile$PopulationSize/newA[j,selected[i]]
                points(newB[j,selected[i]],newE[j,selected[i]],col=i,cex=1)
                if(newE[j,selected[i]]!=0){
                    meanE=meanE+newE[j,selected[i]]
                    countnewE=countnewE+1
                    vectornewEquis[countnewE]=newB[j,selected[i]]
                    vectornewE[countnewE]=newE[j,selected[i]]
                }       
             
             }
             # if(countnewE>1){
              #  abline(lm(vectornewE ~ vectornewEquis),col=i)
                #points(newB[,i],col=i,cex=0.2)
           #  }
           #  points(newB[,selected[i]],newE[,selected[i]],col=i,cex=1) 
            # abline(h=(meanE/countnewE),col=i)
             
            }
            
            
          }}  
        countE=countE+1
        countP=countP+1
        
      }
      
    }
    
  }
}
abline(v=230,col=arr_color[6])
abline(v=480,col=arr_color[6])
abline(v=730,col=arr_color[6])





#axis(1, at=c(500,1500,2500,3500), labels=c("0.1","0.01","0.1","1"))
title(sub = label)


#dev.off()

# 
# 
# #dev.off()
# #This file intends to show the results from 1 PZIFE simulation
# #It shows the relevant statistics that characterize the evolutionary scenario under which the Red-Queen is developing
# #It generates one pdf file 
# 
# 
# for(zz in 1:length(arrayP)){
#   count=0
#   # for(j in 1:length(arrayU)){
#   for(yy in 1:length(arrayD)){
#     for(xx in 1:length(arrayU)){ 
#       countE=0
#       for(ww in 1:length(arrayE)){
#         
#         p=arrayP[zz]
#         X=arrayE[ww]
#         D=arrayD[yy]
#         C=D
#         U=arrayU[xx]
#         #U=C+2
#         label=paste(simID,"_p",p,"_X",X,"_D",D,"_C",C,"_U",U,sep="")
#         setwd(paste(paths,"/",label,sep=""))
#         
#         
#         
#         
#         m <- rbind(c(1, 1,1), c(2,2,2),c(3,3,3),c(4,4,4),c(5,5,5))
#         layout(m,heights = c(2,1.5,1.5,1.5,2.5))
#         
#         par(mar = c(0.5,5, 0.5, 0.5))
#         
#         #Read files
#         a=read.table(paste("alleleFreqs_",label,".dat",sep=""))
#         b=read.table(paste("motifActivity_",label,".dat",sep=""))
#         c=read.table(paste("alleleZnfArraySize_",label,".dat",sep=""))
#         d=read.table(paste("referenceOfActiveAlleles_",label,".dat",sep=""))
#         e=read.table(paste("allelePotentiality_",label,".dat",sep=""))
#         
#         profile=read.table(paste("profile_",label,".dat",sep=""),header=TRUE)
#         
#         
#         uq_elem=c()
#         for(i in 1:ncol(d))
#         {
#           uq_elem=c(unique(d[,i]), uq_elem)
#           uq_elem=unique(uq_elem)
#         }
#         sortUq=sort(uq_elem)
#         
#         
#         colMax <- function(X) apply(X, 2, max)
#         selected=mat.or.vec(ncol(a),1)
#         
#         # Create newA, newB and newC from reference allele list in d
#         
#         newA=mat.or.vec(nrow(a),length(sortUq))
#         newB=mat.or.vec(nrow(a),length(sortUq))
#         newC=mat.or.vec(nrow(a),length(sortUq))
#         newE=mat.or.vec(nrow(a),length(sortUq))
#         for(i in 2:length(sortUq)){
#           for(k in 1:nrow(a)){
#             #for(j in 1:ncol(a)){
#             
#             #
#             j=1
#             #repeat{
#             while(d[k,j]!=0 && d[k,j]<=sortUq[i] && j<=(length(sortUq)-1)){ 
#               if(d[k,j]==sortUq[i]){ 
#                 newA[k,i-1]=a[k,j]
#                 newB[k,i-1]=b[k,j]
#                 newC[k,i-1]=c[k,j]
#                 newE[k,i-1]=c[k,j]
#               }
#               j=j+1
#               #if(d[k,j]==0){break}
#               #if(d[k,j]>=sortUq[i]){
#               #  break
#               #}
#             }
#             #}
#           }
#         }
#         newA[,1]
#         
#         #PRINTS ALLELE FREQUENCIES
#         maxim = colMax(newA)
#         init=0
#         par(mar = c(0.5,5, 0.5, 0.5),xaxs="i")
#         plot(-100,-100,ylim=c(0,1),xlim=c(init,nrow(a)),ylab="Allele Frequencies",xaxt="n",xlab="")
#         count=1
#         for(i in 1:length(maxim)){
#           if(maxim[i] > 0.1){
#             selected[count]=i #guarda las trayectorias que pasan el threshold
#             lines(newA[,i],col=count)
#             count=count+1
#           }
#         }
#         
#         #PRINTS MOTIF ACTIVITY
#         par(mar = c(0.5,5, 0.5, 0.5))
#         plot(100,1000,ylim=c(0.0000001,1),xlim=c(init,nrow(b)),ylab="Motif Activity", xaxt="n",xlab="",log="y")
#         for(i in 1:(count-1)){
#           for(j in 1:nrow(newB)){
#             points(j,newB[j,selected[i]],col=i,cex=0.4)
#             #points(newB[,i],col=i,cex=0.2)
#           }
#         }
#         
#         
#         
#         
#         
#       }}}}
# 
# dev.off()