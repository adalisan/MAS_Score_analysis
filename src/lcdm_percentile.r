
#input command line arguments
args=commandArgs()
FILENAME=args[6]
REGION=args[7]
SUBREGION=args[8]
DATA_DIR="/cis/project/sydney/data_analysis/"
SUBJECT_DIR="/cis/project/sydney/MAS.20100921_Antsy/"
#name of output file

pdf( file = paste(DATA_DIR,SUBREGION,"/thick_percentile.pdf",sep=""), height = 10, width = 16)

#calculating data number and number of histgrams to make given there are a max of 10 datasets per histogram
DATANUMB=nrow(read.table(FILENAME))
HISTNUMB=ceiling(DATANUMB/10)
SKIP<-c(seq(0,HISTNUMB*10,10))

lcdmstats<-c('thk95','thk90','vol95','vol90')
subjids<-c('Subject ID')

for (n in 1:HISTNUMB) {
    READPATH<-c()
    names<-read.table(FILENAME,nrows=10,skip=SKIP[n]) #file that contains file names or path names of the subjects
    subjids<-c(subjids,names[1])
    print(subjids)
    ROWS=nrow(na.omit(names))
    col= c('red','blue','orange','green','gray','pink',70,5,'yellow','brown')
    thk95<-c()
    thk99<-c()
    vol95<-c()
    vol99<-c()
    for (i in 1:ROWS){
    	fn1=paste(SUBJECT_DIR,names[[1]][i],"/FSLCDM_v2.3/",REGION,"/",SUBREGION,"_1_manseg_quartile_pialmsk_AntsyGrey.txt",sep="") 
	fn2=paste(SUBJECT_DIR,names[[1]][i],"/FSLCDM_v2.3/",REGION,"/",SUBREGION,"_1_quartile_pialmsk_AntsyGrey.txt",sep="")
    	if ( file.exists(fn1) )
	   xdata=scan(fn1)
	else xdata=scan(fn2)
	if (i==1) 
           hist(xdata, 100, freq=FALSE, col="white", border="white", xlim=c(-2,12), ylim=c(0,3500),main=SUBREGION,xlab="Distance",ylab="Density")
    	   par(new=T);
	dens1=density(xdata)
#    	lines(dens1, lwd=2, col=col[i])
    	lines(dens1$x,dens1$y*length(xdata), lwd=2, col=col[i])
	thk95[i]=quantile(xdata,.95)
	thk99[i]=quantile(xdata,.99)
	vol95[i]=length(xdata[xdata<thk95[i]])
	vol99[i]=length(xdata[xdata<thk99[i]])
    }
    print(thk95)
    legend(6, 3000, paste(c('ID',t(names[1])),c(' thk95',round(thk95,4)),c(' thk99',round(thk99,4)),c(' vol95',vol95),c(' vol99',vol99),sep=", "), cex=1,  col= c('white','red','blue','orange','green','gray','pink',70,5,'yellow','brown'),lwd=2, bty="n");
    lcdmstats<-rbind(lcdmstats,cbind(thk95,thk99,vol95,vol99))
}
print(subjids)
write.table(cbind(subjids,lcdmstats), file=paste(DATA_DIR,"/",SUBREGION,"/lcdmstats.txt",sep=""), row.names=F, col.names=F)

dev.off()
