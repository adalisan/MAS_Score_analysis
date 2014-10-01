#library(zoo)

#input command line arguments
args=commandArgs()
FILENAME=args[6]
REGION=args[7]
SUBREGION=args[8]
DATA_DIR="/cis/project/sydney/data_analysis_all/"
SUBJECT_DIR_1="/cis/project/sydney/data_fslcdm/MAS.20100921_Antsy/"
SUBJECT_DIR_2="/cis/project/sydney/data_fslcdm/MAS.20110324_Antsy/"

#name of output file
pdf( file = paste(DATA_DIR,SUBREGION,"/",SUBREGION,"_lcdmplot_dens.pdf",sep=""), height = 10, width = 16)

#calculating data number and number of histgrams to make given there are a max of 10 datasets per histogram
DATANUMB=nrow(read.table(FILENAME))
HISTNUMB=ceiling(DATANUMB/10)
SKIP<-c(seq(0,HISTNUMB*10,10))

longtail=data.frame(IDS=NA)
belowsurface=data.frame(IDS=NA)
largevol=data.frame(IDS=NA)
smallvol=data.frame(IDS=NA)
j=1
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
    vol95_norm<-c()
    vol99_norm<-c()
    surfacearea<-data.frame(surfaceArea.mm2.=NA)
    for (i in 1:ROWS){
	print(names[[1]][i])
    	fn1=paste(SUBJECT_DIR_1,names[[1]][i],"/FSLCDM_v2.3/",REGION,"/",SUBREGION,"_1_manseg_quartile_pialmsk_AntsyGrey.txt",sep="") 
	fn2=paste(SUBJECT_DIR_1,names[[1]][i],"/FSLCDM_v2.3/",REGION,"/",SUBREGION,"_1_quartile_pialmsk_AntsyGrey.txt",sep="")
	fn3=paste(SUBJECT_DIR_2,names[[1]][i],"/FSLCDM_v2.3/",REGION,"/",SUBREGION,"_1_manseg_quartile_pialmsk_AntsyGrey.txt",sep="") 
	fn4=paste(SUBJECT_DIR_2,names[[1]][i],"/FSLCDM_v2.3/",REGION,"/",SUBREGION,"_1_quartile_pialmsk_AntsyGrey.txt",sep="")
	if ( file.exists(fn1) ) { 
	   xdata=scan(fn1) }
	else if ( file.exists(fn2) ) {
	   xdata=scan(fn2) }
	else if ( file.exists(fn3) ) {
	   xdata=scan(fn3) }
	else xdata=scan(fn4)
#	if (SUBREGION == 'rh_antcing' || REGION == 'lh_antcing' || 'rh_postcing' || 'lh_postcing' )
#	   xdata=xdata[xdata>-1.75]
#	print(min(xdata))
	xdata=xdata[xdata>-2]
	xdata=xdata[xdata<8]
	if (j==1) {
           hist(xdata, 100, freq=FALSE, col="white", border="white", xlim=c(-2,8), ylim=c(0,.45),main=SUBREGION,xlab="Distance.mm.",ylab="Probability Density")
    	   par(new=T);
	   j=2 }
	print(j)
#	hist(xdata,breaks=max(xdata)-min(xdata))		
	dens1=density(xdata)
#    	lines(dens1$x,dens1$y*length(xdata), lwd=2, col=col[i])
    	lines(dens1, lwd=2, col=col[i])
#	stats=read.csv(paste(SUBJECT_DIR,names[[1]][i],"/FSLCDM_v2.3/stats/lcdm.csv",sep=""))   
#	stats2=read.table(paste(SUBJECT_DIR,names[[1]][i],"/stats/aseg.stats",sep=""),skip=68)   	
#	print(stats2)

#	hemi=unlist(strsplit(REGION,""))[1]
#	roi=unlist(strsplit(SUBREGION,"_"))[2]
#	region_stats=stats[(stats$hemisphere==hemi & stats$ROI==roi),]
#	if ( hemi=='l' ) {
#	   stats3=stats2[(stats2$V5=="Left-Cerebral-Cortex"),]
#	   }
#	else stats3=stats2[(stats2$V5=="Right-Cerebral-Cortex"),]
##	print(stats3)
#	print(region_stats)
#	totalvolume=stats3$V3
##	print(totalvolume)
#	surfacearea=rbind(surfacearea,region_stats$surfaceArea.mm2.)
#	print(region_stats$surfaceArea.mm2.)
#	thk95[i]=quantile(xdata,.95)
#	thk99[i]=quantile(xdata,.99)
#	vol95[i]=length(xdata[xdata<thk95[i]])
#	vol99[i]=length(xdata[xdata<thk99[i]])
#	vol95_norm[i]=length(xdata[xdata<thk95[i]])/totalvolume
#	vol99_norm[i]=length(xdata[xdata<thk99[i]])/totalvolume
	
    }
 #   legend(5, .35, paste(c('ID',t(names[1])),c(' thk95.mm.',round(thk95,4)),c(' thk99.mm.',round(thk99,4)),c(' vol95.mm3.',vol95),c(' vol99.mm3.',vol99),c(' surfArea.mm2.',t(surfacearea$surfaceArea.mm2.)[1:length(names$V1)+1]),sep=", "), cex=1,  col= c('white','red','blue','orange','green','gray','pink',70,5,'yellow','brown'),lwd=2, bty="n");
}

dev.off()
