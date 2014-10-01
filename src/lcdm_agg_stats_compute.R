library(zoo)

#input command line arguments+
args=commandArgs()
#FILENAME=args[6]
REGION=args[7]
SUBREGION=args[8]
#DATA_DIR="/cis/project/sydney/data_analysis_all/"
DATA_DIR="/cis/project/sydney/data_analysis_wave2/"
tag_wave_1_1 <- "MAS.20100921_Antsy"
tag_wave_1_2 <- "MAS.20110324_Antsy"
tag_wave_2 <- "wave2"

SUBJECT_DIRs= paste0("/cis/project/sydney/data_fslcdm/",c(tag_wave_1_1,tag_wave_1_2,tag_wave_2),"/")
AUXDATA_DIR = "./data/"


#name of output file
#pdf( file = paste(RESULT_DIR,SUBREGION,"/",SUBREGION,"_lcdmplot_dens10.pdf",sep=""), height = 10, width = 14)

FILENAMEs = paste0(AUXDATA_DIR,"datalist",c(tag_wave_1_1,tag_wave_1_2,tag_wave_2),".txt")

#FILENAMEs = paste0(AUXDATA_DIR,c("subjects_wave1_full.txt", "subjects_wave2_full.txt"))



file.list<- read.table(FILENAMEs)
#calculating data number and number of histgrams to make given there are a max of 10 datasets per histogram
DATANUMB=nrow(file.list)
HISTNUMB= 1 #ceiling(DATANUMB/10)
SKIP<-c(seq(0,HISTNUMB*10,10))



lcdmstats<-c('thk95','thk90','vol95','vol90')
subjids<-c('Subject ID')
for (pat_grp in 1:3){
  SUBJECTDIR=SUBJECT_DIRs[pat_grp]
  for (n in 1:HISTNUMB) {
    READPATH<-c()
    names<-read.table(FILENAMEs,skip=SKIP[n]) #file that contains file names or path names of the subjects
    subjids<-c(subjids,names[[1]])
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
      if ( file.exists(fn1) ){
        xdata=scan(fn1)
      }  else if ( file.exists(fn1) ){
        xdata=scan(fn2)
      }
      else next
      
      
      thk95[i]=quantile(xdata,.95)
      thk99[i]=quantile(xdata,.99)
      vol95[i]=length(xdata[xdata<thk95[i]])
      vol99[i]=length(xdata[xdata<thk99[i]])
    }
    print(thk95)
    
    lcdmstats<-rbind(lcdmstats,cbind(thk95,thk99,vol95,vol99))
  }
}
print(subjids)
write.table(cbind(subjids,lcdmstats), file=paste(DATA_DIR,"/",SUBREGION,"/lcdmstats.txt",sep=""), row.names=F, col.names=F)


# 
# 
# 
# longtail=data.frame(IDS=NA)
# belowsurface=data.frame(IDS=NA)
# largevol=data.frame(IDS=NA)
# smallvol=data.frame(IDS=NA)
# 
# for (n in 1:HISTNUMB) {
#     READPATH<-c()
#     names<-read.table(FILENAME,nrows=10,skip=SKIP[n]) #file that contains file names or path names of the subjects
#     ROWS=nrow(na.omit(names))
#     col= c('red','blue','orange','green','gray','pink',70,5,'yellow','brown')
#     xmin<-c()
#     yval<-c()
#     thk95<-c()
#     thk99<-c()
#     vol95<-c()
#     vol99<-c()
#     ICV<-c()
#     surfacearea<-data.frame(surfaceArea.mm2.=NA)
#     for (i in 1:ROWS){
# 	print(names[[1]][i])
#     	fn1=paste(SUBJECT_DIR_3,names[[1]][i],"/FSLCDM_v2.3/",REGION,"/",SUBREGION,"_1_manseg_quartile_pialmsk_AntsyGrey.txt",sep="") 
# 	fn2=paste(SUBJECT_DIR_3,names[[1]][i],"/FSLCDM_v2.3/",REGION,"/",SUBREGION,"_1_quartile_pialmsk_AntsyGrey.txt",sep="")
# #	fn3=paste(SUBJECT_DIR_2,names[[1]][i],"/FSLCDM_v2.3/",REGION,"/",SUBREGION,"_1_manseg_quartile_pialmsk_AntsyGrey.txt",sep="") 
# #	fn4=paste(SUBJECT_DIR_2,names[[1]][i],"/FSLCDM_v2.3/",REGION,"/",SUBREGION,"_1_quartile_pialmsk_AntsyGrey.txt",sep="")
# 
# 
# 
# 	if ( file.exists(fn1) ) { 
# 	   xdata=scan(fn1) }
# 	else if ( file.exists(fn2) ) {
# 	   xdata=scan(fn2) }
# #	else if ( file.exists(fn3) ) {
# #	   xdata=scan(fn3) }
# #	else xdata=scan(fn4)
# 	xdata=xdata[xdata>-2]
# 	xdata=xdata[xdata<8]
# 	if (i==1) 
#            hist(xdata, 100, freq=FALSE, col="white", border="white", xlim=c(-2,8), ylim=c(0,.45),main=NA,
# 	   	       xlab="Distance (mm)",ylab="Probability Density",cex.lab=1.5,cex.axis=1.5)
#     	   par(new=T);
# 	dens1=density(xdata)
#     	lines(dens1, lwd=2, col=col[i])
#     }
#     legend(4.2, .4, paste(c('ID  ',t(sapply(strsplit(as.character(names$V1),"_"), "[[", 1))),sep=""), cex=2,  col= c('white','red','blue','orange','green','gray','pink',70,5,'yellow','brown'),lwd=2, bty="n");
# 
# #    legend(3.75, .35, paste(c('ID  ',t(sapply(strsplit(as.character(names$V1),"_"), "[[", 1))),c(' thk.mm.',round(thk95,3)),c(' vol.mm3.',vol95),c(' surfArea.mm2.',t(surfacearea$surfaceArea.mm2.)[1:length(names$V1)+1]),c('ICV',round(ICV)),sep="\t\t"), cex=1,  col= c('white','red','blue','orange','green','gray','pink',70,5,'yellow','brown'),lwd=2, bty="n");
# #    stats=data.frame(IDS=names$V1,thk95.mm.=thk95,thk99.mm.=thk99,vol95.mm3.=vol95,vol99.mm3.=vol99,surfArea.mm2.=surfacearea$surfaceArea.mm2.[2:(length(names$V1)+1)],ICV=ICV)
# #    if (n==1) 
# #       lcdmstats=stats
# #    else 
# #       lcdmstats<-rbind(lcdmstats,stats)
# }
# write.table(lcdmstats, file=paste(DATA_DIR,"/",SUBREGION,"/",SUBREGION,"_lcdmstats.csv",sep=""), row.names=F)
# #write.table(longtail, file=paste(DATA_DIR,"/",SUBREGION,"/longtail.txt",sep=""), row.names=F)
# #write.table(belowsurface, file=paste(DATA_DIR,"/",SUBREGION,"/belowsurface.txt",sep=""), row.names=F)
# #write.table(largevol, file=paste(DATA_DIR,"/",SUBREGION,"/largevol.txt",sep=""), row.names=F)
# #write.table(smallvol, file=paste(DATA_DIR,"/",SUBREGION,"/smallvol.txt",sep=""), row.names=F)
# dev.off()
