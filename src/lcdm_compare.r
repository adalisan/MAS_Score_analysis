library(zoo)

#input command line arguments
args=commandArgs()
FILENAME=args[6]
REGION=args[7]
SUBREGION=args[8]
DATA_DIR="/cis/project/sydney/data_analysis_MAS.20110324/"
SUBJECT_DIR="/cis/project/sydney/data_fslcdm/MAS.20110324_Antsy/"

#name of output file
pdf( file = paste(DATA_DIR,SUBREGION,"/",SUBREGION,"_lcdmplot_dens10.pdf",sep=""), height = 10, width = 16)

#calculating data number and number of histgrams to make given there are a max of 10 datasets per histogram
DATANUMB=nrow(read.table(FILENAME))
HISTNUMB=ceiling(DATANUMB/10)
SKIP<-c(seq(0,HISTNUMB*10,10))

longtail=data.frame(IDS=NA)
belowsurface=data.frame(IDS=NA)
largevol=data.frame(IDS=NA)
smallvol=data.frame(IDS=NA)

for (n in 1:HISTNUMB) {
    READPATH<-c()
    names<-read.table(FILENAME,nrows=10,skip=SKIP[n]) #file that contains file names or path names of the subjects
    ROWS=nrow(na.omit(names))
    col= c('red','blue','orange','green','gray','pink',70,5,'yellow','brown')
    xmin<-c()
    yval<-c()
    thk95<-c()
    thk99<-c()
    vol95<-c()
    vol99<-c()
    ICV<-c()
    surfacearea<-data.frame(surfaceArea.mm2.=NA)
    for (i in 1:ROWS){
	print(names[[1]][i])
    	fn1=paste(SUBJECT_DIR,names[[1]][i],"/FSLCDM_v2.3/",REGION,"/",SUBREGION,"_1_manseg_quartile_pialmsk_AntsyGrey.txt",sep="") 
	fn2=paste(SUBJECT_DIR,names[[1]][i],"/FSLCDM_v2.3/",REGION,"/",SUBREGION,"_1_quartile_pialmsk_AntsyGrey.txt",sep="")
    	if ( file.exists(fn1) ) { 
#	   print(fn1)
	   xdata=scan(fn1) }
	else xdata=scan(fn2)
#	if (REGION == 'rh_cingulate' || REGION == 'lh_cingulate' )
#	   xdata=xdata[xdata>-1.75]
#	else xdata=xdata[xdata>-2]
	xdata=xdata[xdata>-2]
	xdata=xdata[xdata<8]
#	print(min(xdata))
	if (i==1) 
           hist(xdata, 100, freq=FALSE, col="white", border="white", xlim=c(-2,8), ylim=c(0,.45),main=SUBREGION,xlab="Distance.mm.",ylab="Probability Density")
    	   par(new=T);
#	hist(xdata,breaks=max(xdata)-min(xdata))		
	dens1=density(xdata)


#	ydatazoo<-as.zoo(dens1$y)
#	ydatamin<-rollapply(ydatazoo, 3, function(x) which.min(x)==2)
#	indexmin<-index(ydatamin)[coredata(ydatamin)]
#	xminvalues=dens1$x[indexmin]
#	yminvalues=dens1$y[indexmin]
#	indextemp=which(xminvalues<0)
#	index=xminvalues[max(indextemp)]
#	print(index)
#	print(yminvalues[max(indextemp)])

#	xmin[i]=xminvalues[max(indextemp)]
#	yval[i]=yminvalues[max(indextemp)]

#    	lines(dens1$x,dens1$y*length(xdata), lwd=2, col=col[i])
    	lines(dens1, lwd=2, col=col[i])
	stats=read.csv(paste(SUBJECT_DIR,names[[1]][i],"/FSLCDM_v2.3/stats/lcdm.csv",sep=""))   
	stats2=read.table(paste(SUBJECT_DIR,names[[1]][i],"/stats/aseg.stats",sep=""),skip=20,nrows=1,sep=",",comment.char="")   	
	print(stats2)

	hemi=unlist(strsplit(REGION,""))[1]
	roi=unlist(strsplit(SUBREGION,"_"))[2]
	region_stats=stats[(stats$hemisphere==hemi & stats$ROI==roi),]

	surfacearea=rbind(surfacearea,region_stats$surfaceArea.mm2.)

	thk95[i]=quantile(xdata,.95)
	thk99[i]=quantile(xdata,.99)
	vol95[i]=length(xdata[xdata<thk95[i]])
	vol99[i]=length(xdata[xdata<thk99[i]])
	ICV[i]=stats2$V4
    }
    print(names[1])
    legend(3.75, .35, paste(c('ID  ',t(sapply(strsplit(as.character(names$V1),"_"), "[[", 1))),c(' thk.mm.',round(thk95,3)),c(' vol.mm3.',vol95),c(' surfArea.mm2.',t(surfacearea$surfaceArea.mm2.)[1:length(names$V1)+1]),c('ICV',round(ICV)),sep="\t\t"), cex=1,  col= c('white','red','blue','orange','green','gray','pink',70,5,'yellow','brown'),lwd=2, bty="n");
    stats=data.frame(IDS=names$V1,thk95.mm.=thk95,thk99.mm.=thk99,vol95.mm3.=vol95,vol99.mm3.=vol99,surfArea.mm2.=surfacearea$surfaceArea.mm2.[2:(length(names$V1)+1)],ICV=ICV)
    if (n==1) 
       lcdmstats=stats
    else 
       lcdmstats<-rbind(lcdmstats,stats)
}
write.table(lcdmstats, file=paste(DATA_DIR,"/",SUBREGION,"/",SUBREGION,"_lcdmstats.csv",sep=""), row.names=F)
#write.table(longtail, file=paste(DATA_DIR,"/",SUBREGION,"/longtail.txt",sep=""), row.names=F)
#write.table(belowsurface, file=paste(DATA_DIR,"/",SUBREGION,"/belowsurface.txt",sep=""), row.names=F)
#write.table(largevol, file=paste(DATA_DIR,"/",SUBREGION,"/largevol.txt",sep=""), row.names=F)
#write.table(smallvol, file=paste(DATA_DIR,"/",SUBREGION,"/smallvol.txt",sep=""), row.names=F)
dev.off()
