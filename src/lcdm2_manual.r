#R script to make histogram plots of lcdm distances when some of the subjects were manually segmented

#input command line arguments
args=commandArgs()
FILENAME=args[6]
REGION=args[7]
SUBREGION=args[8]

#name of output file
pdf( file = paste(SUBREGION,"_manual.pdf",sep=""), height = 10, width = 16)

#calculating data number and number of histgrams to make given there are a max of 10 datasets per histogram
DATANUMB=nrow(read.table(FILENAME))
HISTNUMB=ceiling(DATANUMB/10)
SKIP<-c(seq(0,HISTNUMB*10,10))

#creating each histogram
for (n in 1:HISTNUMB) {
    READPATH<-c()
    names<-read.table(FILENAME,nrows=10,skip=SKIP[n]) #file that contains file names or path names of the subjects
    ROWS=nrow(na.omit(names))
    col= c('red','blue','orange','green','gray','pink',70,5,'yellow','brown')
    percent_above<-c()
    for (i in 1:ROWS){
    	fn1=paste("/cis/project/sydney/MAS.20100921_Antsy/",
		names[[1]][i],"/FSLCDM_v2.3/",REGION,"/",SUBREGION,"_1_manseg_quartile_pialmsk_AntsyGrey.txt",sep="")  #create path of individual files here 
    	fn2=paste("/cis/project/sydney/MAS.20100921_Antsy/",
		names[[1]][i],"/FSLCDM_v2.3/",REGION,"/",SUBREGION,"_1_quartile_pialmsk_AntsyGrey.txt",sep="")  #create path of individual files here. 
    	if ( file.exists(fn1) )
	   fn=fn1
    	else
	   fn=fn2
    	xdata=scan(fn)
    	if (i==1) 
           hist(xdata, 100, freq=FALSE, col="white", border="white", xlim=c(-2,12), ylim=c(0,.4),main=SUBREGION,xlab="Distance",ylab="Density")
    	   par(new=T);
    	lines(density(xdata), lwd=2, col=col[i])
    	percent_above[i]=round(sum(xdata>4)/length(xdata),4)
    }
    legend(5, .4, c(paste(t(names[1]),percent_above)), cex=2,  col= c('red','blue','orange','green','gray','pink',70,5,'yellow','brown'),lwd=2, bty="n");
}

dev.off()
