
#input command line arguments
args=commandArgs()
FILENAME=args[6]
REGION=args[7]
SUBREGION=args[8]
DATA_DIR="/cis/project/sydney/data_analysis/"
SUBJECT_DIR="/cis/project/sydney/MAS.20100921_rmtg/"
SUBJECT_DIR_2="/cis/project/sydney/MAS.20100921_Antsy/"
#name of output file

pdf( file = paste(DATA_DIR,SUBREGION,"/fit.pdf",sep=""), height = 10, width = 16)

#calculating data number and number of histgrams to make given there are a max of 10 datasets per histogram
DATANUMB=nrow(read.table(FILENAME))
HISTNUMB=ceiling(DATANUMB/10)
SKIP<-c(seq(0,HISTNUMB*10,10))

regdata<-c()
MIN=-3
MAX=8.5

for (n in 1:HISTNUMB) {
    READPATH<-c()
    names<-read.table(FILENAME,nrows=10,skip=SKIP[n]) #file that contains file names or path names of the subjects
    ROWS=nrow(na.omit(names))
    col= c('red','blue','orange','green','gray','pink',70,5,'yellow','brown')
    percent_above1<-c()
    percent_above2<-c()
    for (i in 1:ROWS){
    	fn1=paste(SUBJECT_DIR,names[[1]][i],"/FSLCDM_v2.3/",SUBREGION,"/",SUBREGION,"_1_manseg_quartile_pialmsk_AntsyGrey.txt",sep="")  #create path of individual files here 
	fn2=paste(SUBJECT_DIR_2,names[[1]][i],"/FSLCDM_v2.3/",REGION,"/",SUBREGION,"_1_manseg_quartile_pialmsk_AntsyGrey.txt",sep="")  #create path of individual files here 
#	fn1=paste(SUBJECT_DIR,names[[1]][i],"/FSLCDM_v2.3/",REGION,"/",SUBREGION,"_1_manseg_quartile_pialmsk_AntsyGrey.txt",sep="")
    	xdata1=scan(fn1)
	xdata2=scan(fn2)
	if (i==1) 
           hist(xdata1, 100, freq=FALSE, col="white", border="white", xlim=c(-2,12), ylim=c(0,3500),main=SUBREGION,xlab="Distance",ylab="Density")
    	   par(new=T);
	dens1=density(xdata1)
	dens2=density(xdata2)
#    	lines(dens1, lwd=2, col=col[i])
#    	lines(dens2, lwd=2, col=col[i], lty=2)
    	lines(dens1$x,dens1$y*length(xdata1), lwd=2, col=col[i], lty=2)
    	lines(dens2$x,dens2$y*length(xdata2), lwd=2, col=col[i])
	percent_above1[i]=round(sum(xdata1>4)/length(xdata1),4)
	percent_above2[i]=round(sum(xdata2>4)/length(xdata2),4)
    }
    legend(5, .4, c(paste(t(names[1]),percent_above1,percent_above2,sep=",")), cex=2,  col= c('red','blue','orange','green','gray','pink',70,5,'yellow','brown'),lwd=2, bty="n");

# fit data using isotonic regression    
    for (i in 1:ROWS){
    	fn2=paste(SUBJECT_DIR,names[[1]][i],"/FSLCDM_v2.3/",SUBREGION,"/",SUBREGION,"_1_manseg_quartile_pialmsk_AntsyGrey.txt",sep="")  #create path of individual files here 
	fn1=paste(SUBJECT_DIR_2,names[[1]][i],"/FSLCDM_v2.3/",REGION,"/",SUBREGION,"_1_manseg_quartile_pialmsk_AntsyGrey.txt",sep="")  #create path of individual files here 
#	fn1=paste(SUBJECT_DIR,names[[1]][i],"/FSLCDM_v2.3/",REGION,"/",SUBREGION,"_1_manseg_quartile_pialmsk_AntsyGrey.txt",sep="")
    	xdata1=scan(fn1)
	xdata2=scan(fn2)
	if (i==1) 
           hist(xdata1, 100, freq=FALSE, col="white", border="white", xlim=c(-2,12), ylim=c(0,1),main=SUBREGION,xlab="Distance",ylab="Density")
    	   par(new=T);
	dens1=density(xdata1,from=MIN,to=MAX)
	dens2=density(xdata2,from=MIN,to=MAX)
	x=dens1$x
	xe=dens2$x
    	gx=length(xdata1)*dens1$y
	print(length(gx))
	gxe=length(xdata2)*dens2$y
	print(length(gxe))
	rx=(gx-gxe)/gx
	lines(x,rx, lwd=2, col=col[i], lty=2)
   	regdata<-rbind(regdata,cbind(x,rx))
    }
}
regdata2=na.omit(regdata)
reg=isoreg(regdata2)
cat("R^2 =", formatC(sum(residuals(reg)^2) / (9*var(regdata2)), dig=2),"\n")
regfitdata=cbind(reg$x[reg$ord],reg$yf)
regfitdata=subset(regfitdata,!duplicated(regfitdata))
lines(regfitdata,lwd=3,col='blue')
legend(5, .4, c(paste(t(names[1]),sep=",")), cex=2,  col= c('red','blue','orange','green','gray','pink',70,5,'yellow','brown'),lwd=2, bty="n");
write.table(regdata2, file=paste(DATA_DIR,"/",SUBREGION,"/regdata.txt",sep=""), row.names=F, col.names=F)    
#fitted_params <- read.table(paste(DATA_DIR,"/",SUBREGION,"/fitted_params.txt",sep=""),sep=" ")
#a=fitted_params$V3
#b=fitted_params$V6
#sim_x=(0:1200)/100
#points(sim_x, 1/(1+exp(a*sim_x+b)), type="l")

write.table(regfitdata, file=paste(DATA_DIR,"/",SUBREGION,"/isofitdata.txt",sep=""), row.names=F, col.names=F)
#write.table(cbind(a,b), file=paste(DATA_DIR,"/",SUBREGION,"/sigmoid_params.txt",sep=""), row.names=F, col.names=F)

dev.off()
