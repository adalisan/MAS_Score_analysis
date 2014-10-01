
#input command line arguments
args=commandArgs()
FILENAME=args[6]
HEMI=args[7]
SUBJECT_DIR="/cis/home/jfeng/lcdm_for_Brain1yr_Dec10/work_dir_lcdm/HG/"

pdf( file = paste(SUBJECT_DIR,"/lcdmplot_dens_",HEMI,".pdf",sep=""), height = 10, width = 16)

#calculating data number and number of histgrams to make given there are a max of 10 datasets per histogram
DATANUMB=nrow(read.table(FILENAME))
HISTNUMB=ceiling(DATANUMB/10)
SKIP<-c(seq(0,HISTNUMB*10,10))

for (n in 1:HISTNUMB) {
    READPATH<-c()
    names<-read.table(FILENAME,nrows=10,skip=SKIP[n]) #file that contains file names or path names of the subjects
    ROWS=nrow(na.omit(names))
    col= c('red','blue','orange','green','gray','pink',70,5,'yellow','brown')
    thk95<-c()
    thk99<-c()
    vol95<-c()
    vol99<-c()
    for (i in 1:ROWS){
    	fn1=paste(SUBJECT_DIR,'/',names[[1]][i],"_",HEMI,"_HG_AntsyGrey.txt",sep="") 
	fn2=paste(SUBJECT_DIR,'/',names[[1]][i],"_",HEMI,"_HG_AntsyGrey_original.txt",sep="")
	if ( file.exists(fn1) )
	   xdata=scan(fn1)
	else xdata=scan(fn2)
	if (i==1) 
           hist(xdata, 100, freq=FALSE, col="white", border="white", xlim=c(-2,10), ylim=c(0,.4),main=SUBREGION,xlab="Distance.mm.",ylab="Number of Voxels")
    	   par(new=T);
	dens1=density(xdata)
    	lines(dens1, lwd=2, col=col[i])
	thk95[i]=quantile(xdata,.95)
	thk99[i]=quantile(xdata,.99)
	vol95[i]=length(xdata[xdata<thk95[i]])
	vol99[i]=length(xdata[xdata<thk99[i]])
    }
    legend(5, .35, paste(c('ID',t(names[1])),c(' thk95.mm.',round(thk95,4)),c(' thk99.mm.',round(thk99,4)),c(' vol95.mm3.',vol95),c(' vol99.mm3.',vol99),sep=", "), cex=1,  col= c('white','red','blue','orange','green','gray','pink',70,5,'yellow','brown'),lwd=2, bty="n");
    stats=data.frame(IDS=names$V1,thk95.mm.=thk95,thk99.mm.=thk99,vol95.mm3.=vol95,vol99.mm3.=vol99)
    if (n==1) 
       lcdmstats=stats
    else 
       lcdmstats<-rbind(lcdmstats,stats)
}
write.table(lcdmstats, file=paste(SUBJECT_DIR,"_",HEMI,"_lcdmstats.csv",sep=""), row.names=F)
dev.off()
