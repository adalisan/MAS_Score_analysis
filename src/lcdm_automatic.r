#R script to make histogram plots of lcdm distances when some of the subjects were manually segmented

#input command line arguments
args=commandArgs()
FILENAME=args[6]
SUBJECT_DIR="/cis/home/jwentz/ShannonData/"
#name of output file
pdf( file = paste(SUBJECT_DIR,"autofixed.pdf",sep=""), height = 10, width = 16)

#calculating data number and number of histgrams to make given there are a max of 10 datasets per histogram
DATANUMB=nrow(read.table(FILENAME))
HISTNUMB=ceiling(DATANUMB/10)
SKIP<-c(seq(0,HISTNUMB*10,10))
MIN=-2
MAX=8

isofit<-xy.coords(read.table("isofitdata.txt"))
sigparams<-read.table("sigmoid_params.txt")

for (n in 1:HISTNUMB) {
    READPATH<-c()
    names<-read.table(FILENAME,nrows=10,skip=SKIP[n]) #file that contains file names or path names of the subjects
    ROWS=nrow(na.omit(names))
    col= c('red','blue','orange','green','gray','pink',70,5,'yellow','brown')
    percent_above1<-c()
    percent_above2<-c()
    for (i in 1:ROWS){
   	fn1=paste(SUBJECT_DIR,names[[1]][i],"_lstg_AntsyGrey.txt",sep="")  #create path of individual files here 
 	fn2=paste(SUBJECT_DIR,names[[1]][i],"_lstg_AntsyGrey_SC.txt",sep="")
	xdata1=scan(fn1)
	xdata2=scan(fn2)
	if (i==1) 
           hist(xdata1, 100, freq=FALSE, col="white", border="white", xlim=c(-2,12), ylim=c(0,.5),main="lstg",xlab="Distance",ylab="Density")
    	   par(new=T);
	dens1=density(xdata1,from=MIN,to=MAX)
	dens2=density(xdata2,from=MIN,to=MAX)
	percent_above1[i]=round(sum(xdata1>4)/length(xdata1),4)
	x=dens1$x
    	gx=dens1$n*dens1$y
	gxe1=gx*(1-isofit$y)
	gxe2=gx*(1-(1/(1+exp(sigparams$V1*x+sigparams$V2))))
	nxe1=length(xdata1[xdata1<MIN])+sum(gxe1*(MAX-MIN)/512)+length(xdata1[xdata1 > MAX])
	nxe2=length(xdata1[xdata1<MIN])+sum(gxe2*(MAX-MIN)/512)+length(xdata1[xdata1 > MAX])
    	gxe1_norm=gxe1/nxe1
	gxe2_norm=gxe2/nxe2
	cutoff1=length(gxe1_norm)	
	while (sum(gxe1_norm[cutoff1:length(gxe1_norm)]/512*(MIN-MAX)) < .05) {
	      cutoff1 = cutoff1-1
	}      
	cutoff2=length(gxe2_norm)	
	while (sum(gxe2_norm[cutoff2:length(gxe2_norm)]/512*(MIN-MAX)) < .05) {
	      cutoff2 = cutoff2-1 
	}    
	print(x[cutoff1])
	print(x[cutoff2])  
	lines(dens2, lwd=3, col=col[i])
	lines(x,gxe1_norm, lwd=2, col=col[i], lty=2)
	lines(x,gxe2_norm, lwd=2, col=col[i], lty=3)
    }
}



dev.off()
