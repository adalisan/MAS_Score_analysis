#arg6=region
#arg7=number of MCI
#arg8=number of normal

args=commandArgs()

REGION=args[6]
DIAG=args[7]
LCDM <- function(fileNames,condTests,REGION){
#DATA_NUMB<-c(as.integer(args[7]),as.integer(args[8]))

### Pre-Processing ###
library(MASS)
b<-list(); dists<-c(); distsl<-c();
for (i in 1:length(fileNames)){ 
    distsl[i]<-read.table(fileNames[i])
    distsl[[i]]=distsl[[i]][distsl[[i]]>-2] 
    distsl[[i]]=distsl[[i]][distsl[[i]]<8] 
     }
minlength=min(as.numeric(lapply(distsl,length)))
dists=sapply(distsl,sample,minlength) #for tests requiring same number in group

######### Simple Summary Statistics ########

pdf(file=paste("wave1_wave2/summarystat_",REGION,"_",DIAG,".pdf",sep=''))

col=c('black','red')
thk95<-c()
thk99<-c()
vol95<-c()
vol99<-c()
for(i in 1:length(fileNames)){ 
	if (i==1) 
           hist(distsl[[i]], 100, freq=FALSE, col="white", border="white", xlim=c(-4,10), ylim=c(0,.4),main=NA,xlab="Distance (mm)",ylab="Probability Density")
    	   par(new=T);
	dens1=density(distsl[[i]])
    	lines(dens1, lwd=1, col=col[i])
	thk95[i]=quantile(distsl[[i]],.95)
	thk99[i]=quantile(distsl[[i]],.99)
	}
legend(6, .3, paste(c('wave1','wave2')), cex=1,  col= c('black','red','blue'),lwd=2, bty="n");

for(i in 1:length(fileNames)){ 
	cdf1=ecdf(round(distsl[[i]],3))
	if (i==1) 
	   {
	   plot(cdf1, verticals=TRUE, xlim=c(-2,7),do.points=F, lwd=.5, col=col[i], main=NA,xlab="Distance (mm)", ylab="Cumulative Probability Density")
    	   par(new=T);
	   }		
	else { lines(cdf1, lwd=.5, col=col[i]) }
	}
legend(5, .3, paste(c('wave1','wave2'),sep=", "), cex=1,  col= c('black','red','blue'),lwd=2, bty="n");

dev.off()

########## 2-group tests ###########


## Kolmogorov-Smirnov Test (KS Test) ##
if(condTests[2]==TRUE){
b[[1]]<-ks.test(distsl[[2]],distsl[[1]],alternative="t",exact=NULL)
b[[2]]<-ks.test(distsl[[2]],distsl[[1]],alternative="l",exact=NULL)
b[[3]]<-ks.test(distsl[[2]],distsl[[1]],alternative="g",exact=NULL)
}

return(b)
}

file1=paste("/cis/project/sydney/pooled_analysis/pooled_files_wave1wave2/",REGION,"_",DIAG,"_wave1_pooled_antsy.txt",sep="")
file2=paste("/cis/project/sydney/pooled_analysis/pooled_files_wave1wave2/",REGION,"_",DIAG,"_wave2_pooled_antsy.txt",sep="")

fileNames=c(file1,file2)

b <- c("Kolmogorov-Smirnov Test")
condTests<-c()
condTests[1]="Mann Whitney U Test"%in%b
condTests[2]="Kolmogorov-Smirnov Test"%in%b
condTests[3]="Welch's t-test"%in%b
condTests[4]="Kruskal-Wallis Test"%in%b
condTests[5]="ANOVA F-test"%in%b
b<-condTests
out<-LCDM(fileNames,condTests,REGION)


### write results into file ###

OUTFILE=paste("/cis/project/sydney/pooled_analysis/wave1_wave2/result_",REGION,"_",DIAG,".txt",sep='')

#cat("Statistical Summary for ",REGION,"\n",file=OUTFILE)

if(length(out)!=0)
	{
	if(b[2]){
		cat(out[[1]]$p.value,"\t",out[[2]]$p.value,"\t",out[[3]]$p.value,"\n",file=OUTFILE,append=TRUE)
		}
	} else { cat("No test was performed",file=OUTFILE,append=TRUE)}
