setwd("~/Documents/Projects/PZIFE/Prdm9Nicolas/dataFromCluster/TR_2019_03_07/")

col_vector = c('#e41a1c','#377eb8','#4daf4a',"black",'#984ea3','#ff7f00','#a65628','#f781bf','#377eb8','#4daf4a','#984ea3','#e41a1c','#ff7f00','#f781bf','#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf','#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf')
pdf("prdm9_RDZs_Nulls.pdf",height = 6,width=8)


tamany=c(0.7,1.2,1.8,2)
tipo=c(22,23,24,25,1)
tipo=c(0,5,2,6)

Propor=1
xmin=-5000.2
xmax=100
yemin=c(0,1,1,1,0.01,0)
yemax=c(1,55,25,2200,100,120)

alphaArray=c(1,2,3,"Null")
rhoArray=c(2)
Carray=c(-1,0,1,2,"Null")
Uarray=c(1,2,3,4)
Narray=c(100,500,1000,5000,10000,50000)

ylabs=c("MeanRecRate","AllelicClassDiv","ZnDiv","4Ns","Real / theor GC rate")
geneConvRate=c("g0.5","g0.33b","g0.25b","g0.143","g0")

equislabel="log alpha"

#Varying C and U and rho

mm <- rbind(c(1,1,1,2,2,2,5),c(3,3,3,4,4,4,6))#,c(7,7))
layout(mm,heights = c(2.5,2.5))#,1.5))
m=1
par(mar = c(5,5, 0.5, 0))
numsims=1
inicio="std_output_prueba1_cluster_prdm9_N1000__r"
final=".txt"

pointType=c(1,5,8,11,14,17)


#equislabel=expression(paste("C & U & ",rho,sep=""))
equislabel="C"
countP=1
count=1


ylabs=c("Mean recombination activity","Allelic class diversity","Zinc finger diversity","4Ns","effGeneConv","effPoint")
columnInterest=c(5,6,7,11)#,29,26)
count=0
for(l in 1:length(columnInterest)){
  count=count+1
  if(count==3||count==2||count==4){
    plot(1000,1000,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab=ylabs[count],xlab=equislabel,log="y",xaxt="n")
  }else{
    plot(1000,1000,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab=ylabs[count],xlab=equislabel,xaxt="n")
  }
  axis(1, at=c(-500,-1500,-2500,-3500,-4500), labels=c(40,4,0.4,0.04,0))
  #plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab=ylabs[count],xlab=equislabel)
  jj=1
  #for(jj in 1:length(alphgeneConvRate)){
  # inicio=paste("std_output_prueba2_cluster_prdm9_N1000_",geneConvRate[jj],"_r",sep="")
  for(j in 1:length(Carray)){
    for(k in 1:length(Uarray)){
      for(l in 1:length(alphaArray)){
        alpha=alphaArray[l]
        # alpha="Null"
        U=Uarray[k]
        rho=rhoArray[1]
        C=Carray[j]
        
        for(i in 1:numsims){
          label=paste(inicio,i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
          (label)
          a=read.table(label,header=TRUE)
          distrib[i]=a[[columnInterest[count]]]
          if(columnInterest[count]==29){
            distrib[i]=a[[30]]/2000/1000000#a[[29]]
          }
          if(columnInterest[count]==26){
            distrib[i]=a[[27]]/2000/1000000#a[[26]]
          }
          #points(-(j*100+k*30+l)/4-jj/10+1.5,distrib[i],col=col_vector[jj],pch=k)
          points(((j-1)*1000+((k-1)*200)+((l-1)*30)+(jj-1)*30)*-Propor,distrib[i],col=col_vector[l],pch=tipo[k],cex=tamany[1])
        }
      }
    }
  }
}


plot.new()
par(mar = c(0,0, 0, 0))
legend("bottomleft",c(expression(paste(alpha," = 0",sep="")),expression(paste(alpha," = 0.001",sep="")),expression(paste(alpha," = 0.01",sep="")),
               expression(paste(alpha," = 0.1",sep="")),"U = 0.0004","U = 0.004","U = 0.04","U = 0.4"),
               ncol = 1, col=c(col_vector[4],col_vector[3],col_vector[2],col_vector[1],"black","black","black","black"),
               pch=c(20,20,20,20,tipo[4],tipo[3],tipo[2],tipo[1]),
       bty = "n",x.intersp=2,cex=.6)
# 
dev.off()
# 
# 
# 
# 
# count=count+1
# plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab=ylabs[count],xlab=equislabel)
# for(jj in 1:length(geneConvRate)){
#   inicio=paste("std_output_prueba2_cluster_prdm9_N1000_",geneConvRate[jj],"_r",sep="")
#   for(j in 1:length(Carray)){
#     for(k in 1:length(Uarray)){
#       for(l in 1:length(rhoArray)){
#         alpha=alphaArray[1]
#         U=Uarray[k]
#         rho=rhoArray[l]
#         C=Carray[j]
#         
#         for(i in 1:numsims){
#           label=paste(inicio,i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
#           (label)
#           a=read.table(label,header=TRUE)
#           distrib[i]=a$cl_div
#           #points(-(j*100+k*30+l)/4-jj/10+1.5,distrib[i],col=col_vector[jj],pch=k)
#           points(((j-1)*1000+((k-1)*400)+((l-1)*200)+(jj-1)*30)*-Propor,distrib[i],col=col_vector[jj],pch=tipo[k],cex=tamany[l])
#         }
#       }
#     }
#   }
# }
# 
# count=count+1
# plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab=ylabs[count],xlab=equislabel)
# for(jj in 1:length(geneConvRate)){
#   inicio=paste("std_output_prueba2_cluster_prdm9_N1000_",geneConvRate[jj],"_r",sep="")
#   for(j in 1:length(Carray)){
#     for(k in 1:length(Uarray)){
#       for(l in 1:length(rhoArray)){
#         alpha=alphaArray[1]
#         U=Uarray[k]
#         rho=rhoArray[l]
#         C=Carray[j]
#         
#         for(i in 1:numsims){
#           label=paste(inicio,i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
#           (label)
#           a=read.table(label,header=TRUE)
#           distrib[i]=a$zf_div
#           #points(-(j*100+k*30+l)/4-jj/10+1.5,distrib[i],col=col_vector[jj],pch=k)
#           points(((j-1)*1000+((k-1)*400)+((l-1)*200)+(jj-1)*30)*-Propor,distrib[i],col=col_vector[jj],pch=tipo[k],cex=tamany[l])
#         }
#       }
#     }
#   }
# }
# 
# count=count+1
# plot(100,10000,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab=ylabs[count],xlab=equislabel)
# for(jj in 1:length(geneConvRate)){
#   inicio=paste("std_output_prueba2_cluster_prdm9_N1000_",geneConvRate[jj],"_r",sep="")
#   for(j in 1:length(Carray)){
#     for(k in 1:length(Uarray)){
#       for(l in 1:length(rhoArray)){
#         alpha=alphaArray[1]
#         U=Uarray[k]
#         rho=rhoArray[l]
#         C=Carray[j]
#         
#         for(i in 1:numsims){
#           label=paste(inicio,i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
#           (label)
#           a=read.table(label,header=TRUE)
#           distrib[i]=a$X4Ns
#           #points(-(j*100+k*30+l)/4-jj/10+1.5,distrib[i],col=col_vector[jj],pch=k)
#           points(((j-1)*1000+((k-1)*400)+((l-1)*200)+(jj-1)*30)*-Propor,distrib[i],col=col_vector[jj],pch=tipo[k],cex=tamany[l])
#         }
#       }
#     }
#   }
# }
# 
# 
# count=count+1
# plot(100,10000,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab=ylabs[count],xlab=equislabel)
# for(jj in 1:length(geneConvRate)){
#   inicio=paste("std_output_prueba2_cluster_prdm9_N1000_",geneConvRate[jj],"_r",sep="")
#   for(j in 1:length(Carray)){
#     for(k in 1:length(Uarray)){
#       for(l in 1:length(rhoArray)){
#         alpha=alphaArray[1]
#         U=Uarray[k]
#         rho=rhoArray[l]
#         C=Carray[j]
#         
#         for(i in 1:numsims){
#           label=paste(inicio,i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
#           (label)
#           a=read.table(label,header=TRUE)
#           distrib[i]=a$realConvRate/a$theorConvRate
#           #points(-(j*100+k*30+l)/4-jj/10+1.5,distrib[i],col=col_vector[jj],pch=k)
#           points(((j-1)*1000+((k-1)*400)+((l-1)*200)+(jj-1)*30)*-Propor,distrib[i],col=col_vector[jj],pch=tipo[k],cex=tamany[l])
#         }
#       }
#     }
#   }
# }
# 
# #dev.off()
