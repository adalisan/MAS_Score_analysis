SUBREGION="rh_stg"
SUBJECT="/cis/home/cathey/FSLCDM/0042"

#output file name
pdf( file = paste("FSonly.pdf",sep=""), height = 10, width = 16)

#calculate data number and histrogram number given 10 datasets/histogram max
#DATANUMB=nrow(read.table(FILENAME))
#HISTNUMB=ceiling(DATANUMB/10)
#SKIP<-c(seq(0,HISTNUMB*10,10))

#create data
#for (n in 1:HISTNUMB) {
    READPATH<-c()

 #   names<-read.table(FILENAME,nrows=10,skip=SKIP[n]) #file that contains file names or path names of the subjects
 #   ROWS=nrow(na.omit(names))
    col= c('red','blue','orange','green','gray','pink',70,5,'yellow','brown')

#    for (i in 1:ROWS){
#	print(names[[1]][i])
   	fn1=paste(SUBJECT,"/FS_",SUBREGION,"_1_AntsyGrey.txt",sep="") 
	fn2=paste("/cis/project/sydney/data_fslcdm/MAS.20100921_Antsy/0042_20060207_QC/FSLCDM_v2.3/rh_TL/rh_stg_1_manseg_quartile_pialmsk_AntsyGrey.txt")
    	xdata=scan(fn1)
	xdata=xdata[xdata>-2]
	xdata=xdata[xdata<8]
	xdata2=scan(fn2)
	xdata2=xdata2[xdata2>-2]
	xdata2=xdata2[xdata2<8]

	
#	if (i==1) 
           hist(xdata, 100, freq=FALSE, col="white", border="white", xlim=c(-2,8), ylim=c(0,.45),main=SUBREGION,xlab="Distance.mm.",ylab="Probability Density")
    	   par(new=T);	
	dens1=density(xdata)
	dens2=density(xdata2)
	lines(dens1, lwd=2, col=col[2])
	lines(dens2, lwd=2, col=col[3])
#    }
#}

dev.off()

