
# This plot is intended to show a "three dimensional" plot
# For every C and U pair of values, show R, allelic class, allele and zinc finger diversity

setwd("~/Documents/Projects/PZIFE/Prdm9Nicolas/tests/test2/")

# mm <- rbind(c(1,2,3),c(4,5,6),c(7,8,9))#,c(10,11,12))
# layout(mm,heights = c(2.5,2.5,2.5))#,2.5))
# 
# 
# col_vector = c('#e41a1c','#377eb8','#4daf4a',"black",'#984ea3','#ff7f00','#a65628','#f781bf','#377eb8','#4daf4a','#984ea3','#e41a1c','#ff7f00','#f781bf','#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf','#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf')
# #pdf("prdm9_allRate.pdf",height = 5, width=6)
# 
# 
# a=read.table("testCU_data.txt",header=TRUE)
# stats=c(6,31,7,26,27,29,30)
# labs=c("ACdiv","AlDiv","ZfDiv","totAlPoint","totACPoint","totAlGeneConv","totACGeneConv")
# 
# for(j in 1:length(stats)){
#   plot(400,100,ylim=c(.00004,400),xlim=c(0.00004,0.4),log="xy")
#   text(0.005,400,labs[j])
#   for(i in 1:nrow(a)){
#     equis=a[i,1]
#     ye=a[i,2]+.0004
#     zeta=a[i,stats[j]]
#     #maxim=max(a[,stats[j]])
#    
#     points(equis,ye,pch=19,cex=zeta/max(a[,stats[j]])*5)  
#   }
# }



col_vector = c('#e41a1c','#377eb8','#4daf4a',"black",'#984ea3','#ff7f00','#a65628','#f781bf','#377eb8','#4daf4a','#984ea3','#e41a1c','#ff7f00','#f781bf','#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf','#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf')
pdf("prdm9_allRate.pdf",height = 4, width=8)

mm <- rbind(c(1,2,3))#,c(4,5,6),c(7,8,9))#,c(10,11,12))
layout(mm,heights = c(2.5))#,2.5,2.5))#,2.5))


a=read.table("testCU_data.txt",header=TRUE)
stats=c(6,31,7)
labs=c("ACdiv","AlDiv","ZfDiv","totAlPoint","totACPoint","totAlGeneConv","totACGeneConv")

for(j in 1:length(stats)){
  #plot(NULL,ylim=c(.00004,400),xlim=c(0.00004,0.4),log="xy",xaxt="n",yaxt="n",xlab='',ylab='')
  if(j==1){
    par(mar = c(5,5, 3, 0.5))
    plot(NULL,ylim=c(.00004,400),xlim=c(0.00004,0.4),log="xy",xaxt="n",yaxt="n",xlab='',ylab='')
    axis(2, at=c(.0004,.004,.04,.4,4,40), labels=c(0,0.004,0.04,0.4, 4, 40))
    axis(1, at=c(.0004,.004,.04), labels=c(0.0004,0.004,0.04))
    title(main = "Allelic class diversity", ylab="Gene conversion rate (C)",xlab="Point mutation rate (U)")
  }
  if(j==2){
    par(mar = c(5,5, 3, 0.5))
    plot(NULL,ylim=c(.00004,400),xlim=c(0.00004,0.4),log="xy",xaxt="n",yaxt="n",xlab='',ylab='')
    #axis(2, at=c(.0004,.004,.04,.4,4,40), labels=c(0,0.004,0.04,0.4, 4, 40))
    axis(1, at=c(.0004,.004,.04), labels=c(0.0004,0.004,0.04))
    title(main = "Allele diversity", xlab="Point mutation rate (U)")
  }
  if(j==3){
    par(mar = c(5,5, 3, 0.5))
    plot(NULL,ylim=c(.00004,400),xlim=c(0.00004,0.4),log="xy",xaxt="n",yaxt="n",xlab='',ylab='')
    #axis(2, at=c(.0004,.004,.04,.4,4,40), labels=c(0,0.004,0.04,0.4, 4, 40))
    axis(1, at=c(.0004,.004,.04), labels=c(0.0004,0.004,0.04))
    title(main = "Zinc finger diversity", xlab="Point mutation rate (U)")
  }
 # text(0.005,400,labs[j])
  for(i in 1:nrow(a)){
    equis=a[i,1]
    ye=a[i,2]+.0004
    zeta=a[i,stats[j]]
    #maxim=max(a[,stats[j]])
    
    points(equis,ye,pch=19,cex=zeta/max(a[,stats[j]])*5)  
  }
}

dev.off()



# 
# 
# if(stats[j]==26){
#   zeta=(a[i,26]+a[i,29])/2/1000/1000000
#   maxim=max(zeta)
# }
# else if(stats[j]==27){
#   zeta=(a[i,27]+a[i,30])/2/1000/1000000
# }
# 
# 
# plot(100,100,ylim=c(.00004,400),xlim=c(0.00004,0.4),log="xy")
# for(i in 1:nrow(a)){
#   equis=a[i,1]
#   ye=a[i,2]+.0004
#   zeta=a[i,31]
#   points(equis,ye,pch=19,cex=zeta)  
# }
# plot(100,100,ylim=c(.00004,400),xlim=c(0.00004,0.4),log="xy")
# for(i in 1:nrow(a)){
#   equis=a[i,1]
#   ye=a[i,2]+.0004
#   zeta=a[i,7]
#   points(equis,ye,pch=19,cex=zeta)  
# }
# 
# 
# 
# 
# tamany=c(1,1.2,1.8,2,1,1.2,1.8,2)
# tipo=c(22,23,24,25,1,22,23,24,25,1)
# 
# Propor=1
# xmin=-5000.2
# xmax=100
# yemin=c(1e-7,1e-7,1e-7,1e-9,1e-3,1e-9,1e-9,1e-9,1e-9, 0.01)
# yemax=c(.9,1,1,1,.5,1,1e-3,1e-3,1e-3,40,42,100,.0004,1,1,1)
# 
# yemin=c(.7,1e-5,1e-3,1e-5,2e-3,1e-7,1e-9,1e-9,1e-9, 0.01)
# yemax=c(.85,1,1,1,.25,.5,1e-3,1e-3,1e-3,40,42,100,.0004,1,1,1)
# 
# # yemin=c(1e-7,1e-7,.6,1e-9,1e-9,.6,1e-9,1e-9,.1, 0.01)
# # yemax=c(.9,1,1,1,1,1,1,1,1,40,42,100,.0004,1,1,1)
# 
# 
# alphaArray=c(2,3,"Null")
# rhoArray=c(3)
# Carray=c(-1,0,1,2,3,"Null")
# Uarray=c(2,3,4)
# Narray=c(100,500,1000,5000,10000,50000)
# 
# ylabs=c("MeanRecRate","AllelicClassDiv","AlleleDiv","ZnDiv","4Ns")#,"Real / theor GC rate")
# geneConvRate=c("g0.5","g0.33b","g0.25b","g0.143","g0")
# 
# equislabel="log alpha"
# 
# #Varying C and U and rho
# 
# mm <- rbind(c(1,2,3),c(4,5,6))#,c(7,8,9))#,c(10,11,12))
# layout(mm,heights = c(2.5,2.5,2.5))#,2.5))#,2.5))
# 
# mm <- rbind(c(1,2),c(3,4),c(5,6))#,c(7,8,9))#,c(10,11,12))
# layout(mm,heights = c(2.5,2.5,2.5))#,2.5))
# 
# # 
# #  mm <- rbind(c(1,2))
# #  layout(mm,heights = c(2.5))
# # # 
# 
# m=1
# par(mar = c(5,5, 0.5, 0.5))
# numsims=1
# inicio="std_output_prueba1_cluster_prdm9_N1000__r"
# final=".txt"
# 
# pointType=c(1,5,8,11,14,17)
# 
# equislabel=expression(paste("C & U & ",alpha,sep=""))#,"/",rho,"=cte",sep=""))
# countP=1
# count=1
# effPoint=c()
# effGeneConv=c()
# 
# ylabs=c("totalPoint","totalGeneConv","geneConvProp","effAllelePoint","effAlleleGeneConv","geneConvProp","effAllelicClassPoint","effAllelicClassGeneConv","geneConvProp","ratio1","ratio2","ratio1","ratio2")
# ylabs=c("totalPoint","totalGeneConv","sum","effAllelePoint","effAlleleGeneConv","sum","effAllelicClassPoint","effAllelicClassGeneConv","sum","ratio1","ratio2","ratio1","ratio2")
# ylabs=c("alleleEff/tot Point","alleleEff/tot GeneConv","allelicClass/allele point","allelicClass/Allele geneconv","allelicClass/tot point","allelicClass/tot geneconv","effAllelicClassPoint","effAllelicClassGeneConv","sum","ratio1","ratio2","ratio1","ratio2")
# 
# 
# 
# columnInterest=c(25,28,34,26,29,35,27,30,36)
# columnInterest=c(25,28,31,26,29,32,27,30,33)
# columnInterest=c(25,28,37,26,29,38,27,30)
# columnInterest=c(37,38,39,40,41,42)
# count=0
# 
# for(ll in 1:length(columnInterest)){
#   counter=0
#   count=count+1
#   # if(count==5||count==1||count==2){#
#   #   #||count==4){
#   plot(1000,1000,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab=ylabs[count],xlab=equislabel,log="y")
#   # }else{
#   #   plot(1000,1000,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab=ylabs[count],xlab=equislabel)
#   # }
#   # 
#   #plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab=ylabs[count],xlab=equislabel)
#   jj=1
#   #for(jj in 1:length(alphgeneConvRate)){
#   # inicio=paste("std_output_prueba2_cluster_prdm9_N1000_",geneConvRate[jj],"_r",sep="")
#   for(j in 1:length(Carray)){
#     for(k in 1:length(Uarray)){
#       for(l in 1:length(alphaArray)){
#         alpha=alphaArray[l]
#         # alpha="Null"
#         U=Uarray[k]
#         rho=rhoArray[1]
#         C=Carray[j]
#         counter=counter+1
#         
#         # if(count==4){
#         #   if(l==4){points(((j-1)*1000+((k-1)*200)+((l-1)*30)+(jj-1)*30)*-Propor,effPoint[counter],col=col_vector[l],pch=18,cex=tamany[1])
#         #   }else{points(((j-1)*1000+((k-1)*200)+((l-1)*30)+(jj-1)*30)*-Propor,6*(effPoint[counter]+effGeneConv[counter]),col=col_vector[l],pch=19,cex=tamany[1])
#         #   }
#         # }
#         
#         for(i in 1:numsims){
#           label=paste(inicio,i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
#           (label)
#           a=read.table(label,header=TRUE)
#           
#           if(columnInterest[count]==31){
#             distrib=(a[[25]]+a[[28]])/2/1000/1000000
#             
#           }
#           else if(columnInterest[count]==32){
#             distrib=(a[[26]]+a[[29]])/2/1000/1000000
#             
#           }
#           else if(columnInterest[count]==33){
#             distrib=(a[[27]]+a[[30]])/2/1000/1000000
#             
#           }
#           
#           
#           else if(columnInterest[count]==34){
#             distrib=a[[28]]/(a[[25]]+a[[28]])#/2/1000/1000000/
#             
#           }
#           else if(columnInterest[count]==35){
#             distrib=a[[29]]/(a[[26]]+a[[29]])#/2/1000/1000000/
#             
#           }
#           else if(columnInterest[count]==36){
#             distrib=a[[30]]/(a[[27]]+a[[30]])#/2/1000/1000000/
#             
#           }
#           
#           else if(columnInterest[count]==37){
#             distrib=a[[26]]/a[[25]]#/2/1000/1000000/
#             
#           }
#           else if(columnInterest[count]==38){
#             distrib=a[[29]]/a[[28]]#/2/1000/1000000/
#             
#           }
#           
#           else if(columnInterest[count]==39){
#             distrib=a[[27]]/a[[26]]#/2/1000/1000000/
#             
#           }
#           else if(columnInterest[count]==40){
#             distrib=a[[30]]/a[[29]]#/2/1000/1000000/
#             
#           }
#           
#           else if(columnInterest[count]==41){
#             distrib=a[[27]]/a[[25]]#/2/1000/1000000/
#             
#           }
#           else if(columnInterest[count]==42){
#             distrib=a[[30]]/a[[28]]#/2/1000/1000000/
#             
#           }
#           else{
#             distrib=a[[columnInterest[count]]]/2/1000/1000000
#           }
#           # 
#           
#           #  if(columnInterest[count]==6){
#           #distrib=1-(1/a[[6]])
#           #   distrib=a[[6]]
#           #  }
#           # #points(-(j*100+k*30+l)/4-jj/10+1.5,distrib[i],col=col_vector[jj],pch=k)
#           points(((j-1)*1000+((k-1)*200)+((l-1)*30)+(jj-1)*30)*-Propor,distrib,col=col_vector[l],pch=tipo[k],cex=tamany[1])
#         }
#       }
#     }
#   }
# }
# # equis=seq(-1,-80,-1)
# # plot(equis,effPoint+effGeneConv,col=col_vector[1:4])
# 
# # 
# # col_vector = c('#e41a1c','#377eb8','#4daf4a','#984ea3',"black",'#ff7f00','#a65628','#f781bf','#377eb8','#4daf4a','#984ea3','#e41a1c','#ff7f00','#f781bf','#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf','#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf')
# # 
# # mm <- rbind(c(1))
# #  layout(mm,heights = c(2.5))
# # equis=seq(1,16,1)
# # for(ll in 1:5){
# #   #equis=seq(-80,-1,5)
# #   inicio=(ll-1)*16+1
# #   final=(ll)*16
# #   sub=effPoint[inicio:final]+effGeneConv[inicio:final]
# #   if(ll==1){plot(equis,sub,xlim=c(0,17),ylim=c(1e-4,1e2),log="y",col=col_vector[ll])}
# #   else{points(equis,sub,col=col_vector[ll])}
# # }
# # length(equis)
# # 
# # 
# 
# # plot.new()
# # par(mar = c(0,0, 0, 0))
# # legend("top",c(expression(paste(alpha,"=0",sep="")),expression(paste(alpha,"=0.01",sep="")),"U=4*10^(-log(C/4)-3)", "U=4*10^(-log(C/4)-2)"),
# #        ncol = 2, 
# #        col=c("black","black",col_vector[1],col_vector[2]),pch=c(0,1,20,20),
# #        bty = "n",x.intersp=0.5)
# # 
# # dev.off()
# # 
# # 
# # 
# # 
# # count=count+1
# # plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab=ylabs[count],xlab=equislabel)
# # for(jj in 1:length(geneConvRate)){
# #   inicio=paste("std_output_prueba2_cluster_prdm9_N1000_",geneConvRate[jj],"_r",sep="")
# #   for(j in 1:length(Carray)){
# #     for(k in 1:length(Uarray)){
# #       for(l in 1:length(rhoArray)){
# #         alpha=alphaArray[1]
# #         U=Uarray[k]
# #         rho=rhoArray[l]
# #         C=Carray[j]
# #         
# #         for(i in 1:numsims){
# #           label=paste(inicio,i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
# #           (label)
# #           a=read.table(label,header=TRUE)
# #           distrib[i]=a$cl_div
# #           #points(-(j*100+k*30+l)/4-jj/10+1.5,distrib[i],col=col_vector[jj],pch=k)
# #           points(((j-1)*1000+((k-1)*400)+((l-1)*200)+(jj-1)*30)*-Propor,distrib[i],col=col_vector[jj],pch=tipo[k],cex=tamany[l])
# #         }
# #       }
# #     }
# #   }
# # }
# # 
# # count=count+1
# # plot(100,100,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab=ylabs[count],xlab=equislabel)
# # for(jj in 1:length(geneConvRate)){
# #   inicio=paste("std_output_prueba2_cluster_prdm9_N1000_",geneConvRate[jj],"_r",sep="")
# #   for(j in 1:length(Carray)){
# #     for(k in 1:length(Uarray)){
# #       for(l in 1:length(rhoArray)){
# #         alpha=alphaArray[1]
# #         U=Uarray[k]
# #         rho=rhoArray[l]
# #         C=Carray[j]
# #         
# #         for(i in 1:numsims){
# #           label=paste(inicio,i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
# #           (label)
# #           a=read.table(label,header=TRUE)
# #           distrib[i]=a$zf_div
# #           #points(-(j*100+k*30+l)/4-jj/10+1.5,distrib[i],col=col_vector[jj],pch=k)
# #           points(((j-1)*1000+((k-1)*400)+((l-1)*200)+(jj-1)*30)*-Propor,distrib[i],col=col_vector[jj],pch=tipo[k],cex=tamany[l])
# #         }
# #       }
# #     }
# #   }
# # }
# # 
# # count=count+1
# # plot(100,10000,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab=ylabs[count],xlab=equislabel)
# # for(jj in 1:length(geneConvRate)){
# #   inicio=paste("std_output_prueba2_cluster_prdm9_N1000_",geneConvRate[jj],"_r",sep="")
# #   for(j in 1:length(Carray)){
# #     for(k in 1:length(Uarray)){
# #       for(l in 1:length(rhoArray)){
# #         alpha=alphaArray[1]
# #         U=Uarray[k]
# #         rho=rhoArray[l]
# #         C=Carray[j]
# #         
# #         for(i in 1:numsims){
# #           label=paste(inicio,i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
# #           (label)
# #           a=read.table(label,header=TRUE)
# #           distrib[i]=a$X4Ns
# #           #points(-(j*100+k*30+l)/4-jj/10+1.5,distrib[i],col=col_vector[jj],pch=k)
# #           points(((j-1)*1000+((k-1)*400)+((l-1)*200)+(jj-1)*30)*-Propor,distrib[i],col=col_vector[jj],pch=tipo[k],cex=tamany[l])
# #         }
# #       }
# #     }
# #   }
# # }
# # 
# # 
# # count=count+1
# # plot(100,10000,xlim=c(xmin,xmax),ylim=c(yemin[count],yemax[count]),ylab=ylabs[count],xlab=equislabel)
# # for(jj in 1:length(geneConvRate)){
# #   inicio=paste("std_output_prueba2_cluster_prdm9_N1000_",geneConvRate[jj],"_r",sep="")
# #   for(j in 1:length(Carray)){
# #     for(k in 1:length(Uarray)){
# #       for(l in 1:length(rhoArray)){
# #         alpha=alphaArray[1]
# #         U=Uarray[k]
# #         rho=rhoArray[l]
# #         C=Carray[j]
# #         
# #         for(i in 1:numsims){
# #           label=paste(inicio,i,"_N",Narray[3],"_p",alpha,"_X",rho,"_DNull_C",C,"_U",U,final,sep="")
# #           (label)
# #           a=read.table(label,header=TRUE)
# #           distrib[i]=a$realConvRate/a$theorConvRate
# #           #points(-(j*100+k*30+l)/4-jj/10+1.5,distrib[i],col=col_vector[jj],pch=k)
# #           points(((j-1)*1000+((k-1)*400)+((l-1)*200)+(jj-1)*30)*-Propor,distrib[i],col=col_vector[jj],pch=tipo[k],cex=tamany[l])
# #         }
# #       }
# #     }
# #   }
# # }
# # 
# # #dev.off()
