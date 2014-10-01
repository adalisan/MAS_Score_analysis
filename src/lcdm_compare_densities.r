#R script to make histogram plots of lcdm distances when some of the subjects were manually segmented

#input command line arguments
args=commandArgs()
FILENAME=args[6]
REGION=args[7]
SUBREGION=args[8]

#name of output file
pdf( file = paste(SUBREGION,"_compare_manual.pdf",sep=""), height = 10, width = 16)

#calculating data number and number of histgrams to make given there are a max of 10 datasets per histogram
DATANUMB=nrow(read.table(FILENAME))
HISTNUMB=ceiling(DATANUMB/10)
SKIP<-c(seq(0,HISTNUMB*10,10))

for (n in 1:HISTNUMB) {
    READPATH<-c()
    names<-read.table(FILENAME,nrows=10,skip=SKIP[n]) #file that contains file names or path names of the subjects
    ROWS=nrow(na.omit(names))
    col= c('red','blue','orange','green','gray','pink',70,5,'yellow','brown')
    percent_above1<-c()
    percent_above2<-c()
    for (i in 1:ROWS){
    	fn1=paste("/cis/project/sydney/MAS.20100921_done/",
		names[[1]][i],"/FSLCDM_v2.3/",REGION,"/",SUBREGION,"_orig_0.5_AntsyGrey.txt",sep="")  #create path of individual files here 
    	fn2=paste("/cis/project/sydney/MAS.20100921_done/",
		names[[1]][i],"/FSLCDM_v2.3/",REGION,"/",SUBREGION,"_1_quartile_pialmsk_AntsyGrey.txt",sep="")  #create path of individual files here. 
	fn3=paste("/cis/project/sydney/MAS.20100921_done/",names[[1]][i],"/FSLCDM_v2.3/",REGION,"/",SUBREGION,"_orig_1_AntsyGrey.txt",sep="")
    	fn4=paste("/cis/project/sydney/MAS.20100921_done/",names[[1]][i],"/FSLCDM_v2.3/",REGION,"/",SUBREGION,"_orig_1b_AntsyGrey.txt",sep="")
    	if ( file.exists(fn1) )
	   fn=fn1
    	else
	   fn=fn2
    	xdata1=scan(fn)
	xdata2=scan(fn3)
	xdata3=scan(fn4)
    	if (i==1) 
           hist(xdata1, 100, freq=FALSE, col="white", border="white", xlim=c(-2,12), ylim=c(0,.4),main=SUBREGION,xlab="Distance",ylab="Density")
    	   par(new=T);
    	lines(density(xdata1), lwd=2, col=col[i], lty=2)
    	lines(density(xdata2), lwd=2, col=col[i])
	lines(density(xdata3), lwd=2, col=col[i], lty=3)
	percent_above1[i]=round(sum(xdata1>4)/length(xdata1),4)
	percent_above2[i]=round(sum(xdata2>4)/length(xdata2),4)

    }
    legend(5, .4, c(paste(t(names[1]),percent_above1,percent_above2,sep=",")), cex=2,  col= c('red','blue','orange','green','gray','pink',70,5,'yellow','brown'),lwd=2, bty="n");
}

dev.off()
