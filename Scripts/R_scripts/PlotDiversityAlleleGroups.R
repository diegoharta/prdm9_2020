#!/usr/bin/Rscript
library(extrafont)
#loadfonts()

AsignColorNumber <- function(numcolors, value, rangeNES){
	colpos <- -1
	for (step in c(0:numcolors)){
		if(value-rangeNES[1]>=(rangeNES[2]-rangeNES[1])/(numcolors-1)*(step-1) & value-rangeNES[1]<(rangeNES[2]-rangeNES[1])/(numcolors-1)*step){
			colpos <- step
		}
	}
	if(colpos==-1){print(paste("ERRRORRRRR! color fora del rang (ampliar rangeNES)", value))}
	return(colpos)
}





######
###### MAIN
######

range <- c(0, 1000)
numcolors <- 100
colfunc <- colorRampPalette(c("white", "greenyellow", "forestgreen", "darkgreen"))
colors <- colfunc(numcolors)
sidei <- 0.13
sidej <- 0.1

label="prueba6_1.999_N200_DNullC2aU3_p2X3c"

label="prueba6_1.999_N200_DNullC0.04aU3_p2X3k110c"
label="prueba7_2.01_N200_DNullC4aU3_p2X3c"
label="prueba6_1.999_N200_DNullC0.04aU3_p2X3c"
label="prueba7_2.011_N200_DNullC0.4aU3_p2X3l"

label="prueba5_pzife_2.011_N200_p2_X3_DNull_C2_U3"

#setwd(paste("~/Documents/Projects/PZIFE/C_scripts_and_data/",label,sep=""))
setwd(paste("~/Documents/Projects/PZIFE/C_scripts_and_data/data2FromCluster/TR_2019_01_30/",label,sep=""))

label="prueba32_pzife_2.011_N200_p2_X3_DNull_C0_U3"

#setwd(paste("~/Documents/Projects/PZIFE/C_scripts_and_data/",label,sep=""))
setwd(paste("~/Documents/Projects/PZIFE/C_scripts_and_data/data2FromCluster/TR_2019_02_04/",label,sep=""))

#setwd(paste("~/Documents/Projects/PZIFE/C_scripts_and_data/",label,sep=""))
#Read files
fileToOpen=paste("znfDiversitySpectrum_",label,".dat",sep="")
profile=read.table(paste("profile_",label,".dat",sep=""),header = TRUE)
DF <- read.table(fileToOpen, sep = "\t", header=TRUE) 
#AllelicClassNumber	Year	BindingMotif	Frequency	Activity	ZnfArraySize	Potentiality	ParentId	ParentACNumber ...

#pdf("PlotDiversityAlleleGroups_3.pdf", family = "Abyssinica SIL", colormodel="cmyk", width = 25, height = 10)
pdf(paste("PlotDiversityAlleleGroups_",label,".pdf",sep=""), colormodel="cmyk", width = 25, height = 10)

layout(matrix(c(1,2,3,4,5,6),nrow=2,ncol=3,byrow=T), widths=c(3,3,3), heights=c(2,2), TRUE)

#for(year in unique(DF$Year)){
#for(year in unique(DF$Year)[1:4]){
#for(year in c(12000,14200, 16000, 16300,18200, 19000)){
#for(year in c(9000,9300, 9600, 9900,10200, 10500, 10800,11100,11400,11700,12000,12300,12600,13200,13500)){
#for(year in c(2000,3200,4400,5600,6800,8000,9200,10400,11600,12800,14000)){
for(year in c(100)){
  
	print(paste("Year =", year))
  DF
	DATA <- DF[DF$Year==year,c(13:length(DF[1,])-3)]
	(DATA$Znf_4)
	DATA <- DATA*1000
	DATA <- as.data.frame(apply(DATA, 2, as.integer))
	v <- as.vector(colSums(DATA)>0)
	DATA <- DATA[, v[1:length(v)-1]]
	print(DATA)
	BindingMotifDATA <- DF$BindingMotif[DF$Year==year]
	print(BindingMotifDATA)
	FrequencyDATA <- DF$Frequency[DF$Year==year]
	print(FrequencyDATA)
	ZAidDATA <- colnames(DATA)
	print(ZAidDATA)

	veci<- seq(0, length(DATA[1,])*sidei, sidei)
	veci<- veci+sidei*5
	vecj <- seq(0, length(DATA[,1])*sidej, sidej)
	par(mar=c(3,3,3,3))
	plot(c(0:10), c(0:10), axes=F, col=F, xlab="", ylab="", xlim=c(0,max(veci[length(veci)]+sidei*2, vecj[length(vecj)])), ylim=c(0,max(veci[length(veci)]+sidei*2, vecj[length(vecj)])))
	text(veci[1]-sidei/3, vecj[length(DATA[,1])+1]+.2, (year-profile$BurnIn)/profile$Interval, cex=2)
	for(j in c(1:length(DATA[,1]))){ ## For each allele class
		y <- vecj[length(DATA[,1])-j+1]
		while(nchar(BindingMotifDATA[j])<9){
			BindingMotifDATA[j] <- paste("0", BindingMotifDATA[j],sep="")
		}
		text(veci[1]-sidei/3, y+sidej/2, paste(substr(BindingMotifDATA[j],1,3), "-", substr(BindingMotifDATA[j],4,6), "-", substr(BindingMotifDATA[j],7,9), sep=""), cex=1.2, adj=c(1,0.5), font=2)
		polygon(c(veci[length(veci)]+sidei/6,veci[length(veci)]+sidei*2.1,veci[length(veci)]+sidei*2.1,veci[length(veci)]+sidei/6), c(y,y,y+sidej,y+sidej), border=NA, col=colors[AsignColorNumber(numcolors, as.integer(FrequencyDATA[j]*1000), range)])
		text(veci[length(veci)]+sidei/3, y+sidej/2, FrequencyDATA[j], cex=1.2, adj=c(0,0.5), font=2)
		for(i in c(1:length(DATA[1,]))){ ## For each zinc finger allele
			colnum <- AsignColorNumber(numcolors, DATA[j,i], range)
			x <- veci[i]
			polygon(c(x,x+sidei,x+sidei,x), c(y,y,y+sidej,y+sidej), col=colors[colnum])
			text(x+sidei/2, y+sidej/2, DATA[j,i]/1000, adj=c(.5,.5), cex=.7)
#			polygon(c(veci[i],veci[i]+sidei,veci[i]+sidei,veci[i]), c(vecj[j],vecj[j],vecj[j]+sidej,vecj[j]+sidej), col=colors[colnum])
#			text(veci[i]+sidei/2, vecj[j]+sidej/2, DATA[j,i]/1000, adj=c(.5,.5), cex=.7)
			if(j==1){
				text(x+sidei/2, y+sidej, substr(ZAidDATA[i], 5, 7), adj=c(0,0), cex=1.2, pos=3, font=2)
			}
		}
	}
}


dev.off()


#pdf("PlotDiversityAlleleGroups.pdf", colormodel="cmyk", width = 15, height = 15)
#layout(matrix(c(1,2,3,4),nrow=2,ncol=2,byrow=T), widths=c(1,1), heights=c(1,1), TRUE)
##for(year in unique(DF$Year)){
#for(year in unique(DF$Year)[1]){
#	print(paste("Year =", year))
#	DATA <- DF[DF$Year==year,c(10:length(DF[1,]))]
#	DATA <- DATA*1000
#	DATA <- as.data.frame(apply(DATA, 2, as.integer))
#	v <- as.vector(colSums(DATA)>0)
#	DATA <- DATA[, v[1:length(v)-1]]
#	print(head(DATA))
#	labelsDATA <- DF[DF$Year==year,1]
#	print(labelsDATA)
#	veci<- seq(0, length(DATA[1,])*sidei, sidei)
#	veci<- veci+2
#	vecj <- seq(0, length(DATA[,1])*sidej, sidej)
#	par(mar=c(3,3,3,3))
#	plot(c(0:10), c(0:10), axes=T, col=F, xlab="", ylab="", xlim=c(0,max(veci[length(veci)]+sidei, vecj[length(vecj)])), ylim=c(0,max(veci[length(veci)]+sidei, vecj[length(vecj)])))
#	for(i in c(1:length(DATA[1,]))){ ## for each allele class
#		text(veci[1]-0.5, vecj[i]+sidei/2, labelsDATA[i], cex=1, pos=2)
#		for(j in c(1:length(DATA[,1]))){ ## For each zinc finger allele
#			colnum <- AsignColorNumber(numcolors, DATA[j,i], range)
#			polygon(c(veci[i],veci[i]+sidei,veci[i]+sidei,veci[i]), c(vecj[j],vecj[j],vecj[j]+sidej,vecj[j]+sidej), col=colors[colnum])
#			text(veci[i]+sidei/2, vecj[j]+sidej/2, DATA[j,i]/1000, adj=c(.5,.5), cex=.5)
#		}
#	}
#}





