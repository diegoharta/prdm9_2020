
setwd("~/Documents/Academico/Proyectos/prdm9_2020/")

library(viridis)
library(viridisLite)

col_vector = viridis(20)


# library(RColorBrewer)
# n <- 1000
# qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
# col_vect = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
# col_vector = sample(col_vect, n,replace = TRUE)

#col_vector = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#b15928')

#col_vector = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf','#377eb8','#4daf4a','#984ea3','#e41a1c','#ff7f00','#f781bf','#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf','#e41a1c',
#                '#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf','#377eb8','#4daf4a','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf')

# col_vector = c('#000000','#1f78b4','#e31a1c','#ff7f00', '#33a02c','#6a3d9a')
# n <- 50
# colfunc <- colorRampPalette(col_vector)
# colors <- colfunc(n)
# resortedcolors <- colors[c(seq(1,n,10), seq(2,n,10), seq(3,n,10), seq(4,n,10), seq(5,n,10), seq(6,n,10), seq(7,n,10), seq(8,n,10), seq(9,n,10), seq(10,n,10))]
# col_vector <- resortedcolors

col_vector_mbv = c('red3', 'darkorange2','gold', 'forestgreen', 'seagreen2', 'royalblue3', 'darkmagenta')
n <- 50
colfunc <- colorRampPalette(col_vector_mbv)
colors_mbv <- colfunc(n)
col_vector <- colors_mbv
resortedcolors_mbv <- colors_mbv[c(seq(1,n,10), seq(6,n,10), seq(2,n,10), seq(7,n,10), seq(3,n,10), seq(8,n,10), seq(4,n,10), seq(9,n,10), seq(5,n,10), seq(10,n,10))]
col_vector<- resortedcolors_mbv

#col_vector = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#b15928')


sim=2
#p=2
#X=3
C=0
U=4
starTime=944
endTime=1400

#label=paste("prueba1_cluster_prdm9_N1000_t1M_r",sim,"_p2_X3_C",C,"_U3.",sep="")
#label2=paste("prueba1_cluster_prdm9_N1000_t1M_r",sim,"_N1000_p",p,"_X",X,"_DNull_C",C,"_U",U,".",sep="")
#label2="pruebaGeneConv2_iniAt1_no0G_znfInarray_H."
label=paste("prueba1_cluster_prdm9_N1000__r",sim,"_N1000_p2_X3_DNull_C",C,"_U",U,".",sep="")

#setwd(paste("~/Documents/Projects/PZIFE/Prdm9Nicolas/dataFromCluster/TR_2019_02_19_b/TR_2019_02_19_b_trace/",sep=""))
#setwd(paste("~/Documents/Projects/PZIFE/Prdm9Nicolas/tests/test2/",sep=""))
#setwd(paste("~/Documents/Projects/PZIFE/Prdm9Nicolas/dataFromCluster/TR_2019_03_05/",sep=""))

pdf("Manuscript/Images/Images_new/quasispecies.pdf",width=10,height = 8)


m <- rbind(c(1,1,1), c(2,2,2),c(3,3,3),c(4,4,4))
layout(m,heights = c(3,3,2,2))

par(mar = c(0.5,5, 0.5, 0.5))

nrow(a)
#a_All=read.table(paste("Data/Quasispecies/",label,"allelic_class_freq_history",sep=""))
a=a_All[starTime:endTime,]
#d_All=read.table(paste("Data/Quasispecies/",label,"allelic_class_history",sep=""))
d=d_All[starTime:endTime,]
#act_All=read.table(paste("Data/Quasispecies/",label,"alelic_class_act_history",sep=""))
act=act_All[starTime:endTime,]

#a_zf_All=read.table(paste("Data/Quasispecies/",label,"zf_freq_history",sep=""))
a_zf=a_zf_All[starTime:endTime,]
#d_zf_All=read.table(paste("Data/Quasispecies/",label,"zf_history",sep=""))
d_zf=d_zf_All[starTime:endTime,]

#profile=read.table(paste("std_output_",label2,"txt",sep=""),header=TRUE)


uq_elem=c()
for(i in 1:ncol(d))
{
  uq_elem=c(unique(d[,i]), uq_elem)
  uq_elem=unique(uq_elem)
}
sortUq=sort(uq_elem)
sortUq=sortUq[2:length(sortUq)]

colMax <- function(X) apply(X, 2, max)
selected=mat.or.vec(ncol(a),1)
selected2=mat.or.vec(1000,1)


# Creates sort unique vector of zinc fingers
uq_elem_zf=c()
for(i in 1:ncol(d_zf))
{
  uq_elem_zf=c(unique(d_zf[,i]), uq_elem_zf)
  uq_elem_zf=unique(uq_elem_zf)
}
sortUq_zf=sort(uq_elem_zf)
sortUq_zf=sortUq_zf[2:length(sortUq_zf)]

#grep(substr(textoA,1,3),sortUq_zf)
#LETTERS[4]
# Create newA, newB and newC from reference allele list in d

newA=mat.or.vec(nrow(a),length(sortUq))
newB=mat.or.vec(nrow(act),length(sortUq))
for(i in 1:length(sortUq)){
  for(k in 1:nrow(a)){
    j=1
    while(d[k,j]!=0 && d[k,j]<=sortUq[i] && j<=(length(sortUq))){ 
      #while(d[k,j]<=sortUq[i] && j<=(length(sortUq)-1)){ 
      
      if(d[k,j]==sortUq[i]){ 
        newA[k,i]=a[k,j]
        newB[k,i]=act[k,j]
      }
      j=j+1
    }
  }
}


umbral=.3
#PRINTS ALLELIC CLASS FREQUENCIES
maxim = colMax(newA)
init=0
par(mar = c(0.5,5, 0.5, 0.5),xaxs="i")
plot(-100,-100,ylim=c(0,1),xlim=c(init,nrow(a)),ylab="Allele frequencies",xaxt="n",xlab="",cex.lab=1.4)
count=1
abline(h=1,col="darkgrey",lty=3)
for(i in 1:length(maxim)){
  if(maxim[i] > umbral){
    selected[count]=i #guarda las trayectorias que pasan el threshold
    lines(newA[,i],col=col_vector[count])#col_vector[count])
    count=count+1
  }
}


#PRINTS ALLELIC CLASS NUMBER AT TIME OF maximum

correctLetter=5 #adjust to define start letter
umbralPrint=0.3 #set equal to umbral to show all elements
countInvasions=1
countPosition=0
verticalPosition=0
classInvasionArray=c()
zfList =c()
for(jj in 1:(length(sortUq))){
  if(maxim[jj]>umbral){
    countPosition=countPosition+1
    if(maxim[jj]>umbralPrint){
      verticalPosition=verticalPosition+1
      textoA=sortUq[jj]
      while(nchar(textoA)<9){
        textoA <- paste("0", textoA,sep="")
      }  
      textoB <- paste(LETTERS[grep(substr(textoA,1,3),sortUq_zf)-correctLetter],LETTERS[grep(substr(textoA,4,6),sortUq_zf)-correctLetter],
                      LETTERS[grep(substr(textoA,7,9),sortUq_zf)-correctLetter],sep="")
      #LETTERS[4]
      #textoB=paste(substr(textoA,1,3), "-", substr(textoA,4,6), "-", substr(textoA,7,9), sep="")
      lugar=which(newA[,jj]==maxim[jj])
      text(lugar,maxim[jj]+.1,textoB,cex=1.2,col=col_vector[countPosition],font=2)
      # text(lugar,1.5,textoB,cex=1.2,col=1)
      
      if(lugar != 0){
        classInvasionArray[countInvasions]=lugar
        countInvasions=countInvasions+1
      }
    }
  }
}

#PRINTS MOTIF ACTIVITY
par(mar = c(0.5,5, 0.5, 0.5))
plot(100,1000,ylim=c(0,1),xlim=c(init,nrow(act)),ylab="Recombination activities", xaxt="n",xlab="",cex.lab=1.4)
for(i in 1:(count-1)){
  for(j in 1:nrow(newB)){
    if(newB[j,selected[i]]!=0){
      points(j,newB[j,selected[i]],col=col_vector[i],cex=0.8,pch=20)
      #points(newB[,i],col=i,cex=0.2)
    }
  }
}



#PRINTS ZINC FINGER FREQUENCIES

selected=mat.or.vec(ncol(a_zf),1)
selected2=mat.or.vec(1000,1)

# Create newA, newB and newC from reference allele list in d
newA_Znf=mat.or.vec(nrow(a_zf),length(sortUq_zf))
for(i in 1:length(sortUq_zf)){
  for(k in 1:nrow(a_zf)){
    j=1
    while(d_zf[k,j]!=0 && d_zf[k,j]<=sortUq_zf[i] && j<=(length(sortUq_zf))){ 
      #while(d_zf[k,j]<=sortUq_zf[i] && j<=(length(sortUq_zf))){ 
      
      if(d_zf[k,j]==sortUq_zf[i]){ 
        newA_Znf[k,i]=a_zf[k,j]
      }
      j=j+1
    }
  }
}

umbral=0.1
colorExtension=5
#PRINTS ZNF FREQUENCIES
maxim_zf = colMax(newA_Znf)
init=0
par(mar = c(0.5,5, 0.5, 0.5),xaxs="i")
plot(-100,-100,ylim=c(0,1),xlim=c(init,nrow(a_zf)),ylab="Zinc finger frequencies",xlab="",cex.lab=1.4,xaxt="n")
count=1
abline(h=1,col="darkgrey",lty=3)
for(i in 1:length(maxim_zf)){
  if(maxim_zf[i] > umbral){
    selected[count]=i #guarda las trayectorias que pasan el threshold
    lines(newA_Znf[,i],col=col_vector_mbv[count],lwd=2)
    count=count+1
  }
}


#PRINTS Znf NUMBER AT TIME OF OCCURRENCE

# #To determine where to place the label corresponding to each allelic class
# times=mat.or.vec(1,ncol(newA_Znf))
# counts=mat.or.vec(1,ncol(newA_Znf))
# moment=mat.or.vec(1,ncol(newA_Znf))
# sumaCols=mat.or.vec(1,ncol(newA_Znf))
# 
# 
# for(i in 1:(ncol(newA_Znf))){
#   for(j in 1:nrow(newA_Znf)){
#     if(newA_Znf[j,i]!=0){
#       times[i]=times[i]+j
#       moment[i]=moment[i]+j*newA_Znf[j,i]
#       counts[i]=counts[i]+1
#       sumaCols[i]=sumaCols[i]+newA_Znf[j,i]
#     }
#   }
#   times[i]=times[i]/counts[i]
#   moment[i]=moment[i]/sumaCols[i]
# }


countInvasions=1
countPosition=0
classInvasionArray=c()
for(jj in 1:(length(sortUq_zf))){
  if(maxim_zf[jj]>umbral){
    countPosition=countPosition+1
    textoA=sortUq_zf[jj]
    while(nchar(textoA)<3){
      textoA <- paste("0", textoA,sep="")
    }  
    textoAB <- paste(LETTERS[grep(textoA,sortUq_zf)-correctLetter]," = ", textoA,sep="")
    lugar=which(newA_Znf[,jj]==maxim_zf[jj])
    #text(lugar,2-.1*((countPosition)%%9),textoA,cex=1.2,col=col_vector[countPosition*colorExtension])
    text(lugar,maxim_zf[jj]+.1,textoAB,cex=1.2,col=col_vector_mbv[countPosition],font = 2)
    
    # text(lugar,1.5,textoB,cex=1.2,col=1)
    
    if(lugar != 0){
      classInvasionArray[countInvasions]=lugar
      countInvasions=countInvasions+1
    }
  }
}

#The number of zinc finger trajectories is count-1
xPoints=c()
xPoints[1]<- 19
xPoints[2]<- 147
xPoints[3]<- 252
xPoints[4]<-331
xPoints[5]<- 396
xPoints[6]<- 409
#newA_Znf[,selected[6]]

par(mar = c(3,5, 0.5, 0.5),xaxs="i")
plot(-100,-100,ylim=c(0,1),xlim=c(init,nrow(a_zf)),yaxt="n",xaxt="n",xlab="",ylab="",bty="n")

#EF
polygon(c(0,0,xPoints[2], xPoints[2]), c(1,.9, .9,1), col=rgb(1, 0, 0,0.5), border=NA)

#BEF
polygon(c(xPoints[1], xPoints[1],xPoints[6], xPoints[6]), c(.85,.75,.75,.85), col=rgb(0,1, 0,0.5), border=NA)

#BF
polygon(c(xPoints[2],xPoints[2],xPoints[4], xPoints[4]), c(1,.9, .9,1), col=rgb(0, 0, 1,0.5), border=NA)

#CB
polygon(c(xPoints[3],xPoints[3],xPoints[6], xPoints[6]), c(.70,.60, .60,.70), col=rgb(1, 0, 1,0.5), border=NA)

#CJ
polygon(c(xPoints[5],xPoints[5],nrow(a_zf), nrow(a_zf)), c(1,.9, .9,1), col=rgb(1, 1, 0,0.5), border=NA)

# polygon(c(xPoints[3],xPoints[3],xPoints[4], xPoints[4]), c(.95,1, 1,.95), col=rgb(0, 1, 0,0.5), border=NA)
# polygon(c(xPoints[3],xPoints[3],xPoints[4], xPoints[4]), c(0,.2, .2,0), col=rgb(0, 0, 1,0.5), border=NA)
# polygon(c(xPoints[3],xPoints[3],xPoints[4], xPoints[4]), c(.2,.7, .7,.2), col=rgb(1, 1, 0,0.5), border=NA)
# polygon(c(300,300,310,310), c(.7,.95, .95,.70), col=rgb(0, 1, 1,0.5), border=NA)
# 
# polygon(c(xPoints[4],xPoints[4],xPoints[5], xPoints[5]), c(.95,1, 1,.95), col=rgb(0, 1, 0,0.5), border=NA)
# polygon(c(xPoints[4],xPoints[4],xPoints[5], xPoints[5]), c(0,.95, .95,0), col=rgb(1, 1, 0,0.5), border=NA)
# 
# polygon(c(xPoints[5],xPoints[5],xPoints[6], xPoints[6]), c(.95,1, 1,.95), col=rgb(0, 1, 0,0.5), border=NA)
# polygon(c(xPoints[5],xPoints[5],xPoints[6], xPoints[6]), c(.9,.95,.95,.9), col=rgb(1, 1, 0,0.5), border=NA)
# polygon(c(xPoints[5],xPoints[5],xPoints[6], xPoints[6]), c(0,.9, .9,0), col=rgb(0, 1, 1,0.5), border=NA)
# 
# polygon(c(xPoints[6],xPoints[6],nrow(a_zf), nrow(a_zf)), c(0,1, 1,0), col=rgb(0, 1, 1,0.5), border=NA)

dev.off()