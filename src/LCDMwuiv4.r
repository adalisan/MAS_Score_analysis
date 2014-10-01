#arg6=region
#arg7=number of MCI
#arg8=number of normal

args=commandArgs()

REGION=args[6]
LCDM <- function(fileNames,condTests,REGION){

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

pdf(file=paste("test_results_MCI/summarystat_",REGION,".pdf",sep=''))

col=c('red','blue')
thk95<-c()
thk99<-c()
vol95<-c()
vol99<-c()
for(i in 1:length(fileNames)){ 
	if (i==1) 
           hist(distsl[[i]], 100, freq=FALSE, col="white", border="white", xlim=c(-4,10), ylim=c(0,.4),main=REGION,xlab="Distance (mm) ",ylab="Density")
    	   par(new=T);
	dens1=density(distsl[[i]])
    	lines(dens1, lwd=1, col=col[i])
	thk95[i]=quantile(distsl[[i]],.95)
	thk99[i]=quantile(distsl[[i]],.99)
	}
legend(6, .3, paste(c('Type','MCI','normal')), cex=1,  col= c('white','red','blue'),lwd=2, bty="n");

for(i in 1:length(fileNames)){ 
	cdf1=ecdf(distsl[[i]])
	if (i==1) 
	   {
	   plot(cdf1, verticals=TRUE, xlim=c(-2,7),do.points=FALSE, lwd=.5, col=col[i])
    	   par(new=T);
	   }		
	else { lines(cdf1, lwd=.5, col=col[i]) }
	}
legend(5, .3, paste(c('MCI','normal'),sep=", "), cex=1,  col= c('red','blue'),lwd=2, bty="n");

dev.off()

########## 2-group tests ###########
if(ncol(dists)==2){

x<-sample(distsl[[1]],100000)
y<-sample(distsl[[1]],100000)

## Mann-Whitney Test (Wilcoxon Rank Sum) ##
if(condTests[1]==TRUE){
b[[1]]<-wilcox.test(x,y,alternative="t",exact = FALSE,correct = FALSE)
b[[2]]<-wilcox.test(x,y,alternative="g",exact = FALSE,correct = FALSE)
b[[3]]<-wilcox.test(x,y,alternative="l",exact = FALSE,correct = FALSE)
}

## Kolmogorov-Smirnov Test (KS Test) ##
if(condTests[2]==TRUE){
b[[4]]<-ks.test(x,y,alternative="t",exact=NULL)
b[[5]]<-ks.test(x,y,alternative="l",exact=NULL)
b[[6]]<-ks.test(x,y,alternative="g",exact=NULL)
}

## Welch's t Test for Independent Samples ##
if(condTests[3]==TRUE){
b[[7]]<-t.test(x,y,alternative = "t",mu = 0, paired = FALSE, var.equal = FALSE,conf.level = 0.95)
b[[8]]<-t.test(x,y,alternative = "g",mu = 0, paired = FALSE, var.equal = FALSE,conf.level = 0.95)
b[[9]]<-t.test(x,y,alternative = "l",mu = 0, paired = FALSE, var.equal = FALSE,conf.level = 0.95)
}
}

########## Multi-group tests ###########

## Kruskal-Wallis one-way ANOVA (KW Test) ##
if(condTests[4]==TRUE){
	b[[10]]<-kruskal.test(distsl)
}

## ANOVA F-test ##
if(condTests[5]==TRUE){
	g<-rep(1:length(distsl),sapply(distsl,length))
	distsv<-unlist(distsl)
	b[[11]]<-oneway.test(distsv~g,var.equal=FALSE)
}
return(b)
}

file1=paste("pooled_antsy_files/",REGION,"_MCI_wave1_pooled_antsy.txt",sep='')
file2=paste("pooled_antsy_files/",REGION,"_normal_wave1_pooled_antsy.txt",sep='')
fileNames=c(file1,file2)

b <- c("Kolmogorov-Smirnov Test","Welch's t-test","Mann Whitney U Test")#,"Kruskal-Wallis Test","ANOVA F-test")
condTests<-c()
condTests[1]="Mann Whitney U Test"%in%b
condTests[2]="Kolmogorov-Smirnov Test"%in%b
condTests[3]="Welch's t-test"%in%b
condTests[4]="Kruskal-Wallis Test"%in%b
condTests[5]="ANOVA F-test"%in%b
b<-condTests
out<-LCDM(fileNames,condTests,REGION)


### write results into file ###

OUTFILE=paste("/cis/project/sydney/pooled/test_results_MCI/result_",REGION,".txt",sep='')

cat("Statistical Summary for ",REGION,"\n",file=OUTFILE)

if(length(out)!=0)
	{
	if(b[1]){
		cat("p-value for MWU Test (two-sided, greater, less): \t",out[[1]]$p.value,"\t",out[[2]]$p.value,"\t",out[[3]]$p.value,"\n",file=OUTFILE,append=TRUE)
#		cat("p-value for MWU Test (two-sided, greater, less): \t",out[[1]]$statistic,"\t",out[[2]]$statistic,"\t",out[[3]]$statistic,"\n",file=OUTFILE,append=TRUE)
		}

	if(b[2]){
		cat("p-value for KS Test (two-sided, less, greater): \t", out[[4]]$p.value,"\t",out[[5]]$p.value,"\t",out[[6]]$p.value,"\n",file=OUTFILE,append=TRUE)
		cat("D for KS Test (two-sided, less, greater): \t", out[[4]]$statistic,"\t",out[[5]]$statistic,"\t",out[[6]]$statistic,"\n",file=OUTFILE,append=TRUE)
		}
	if(b[3]){
		cat("p-value for Welch's t-test (two-sided, greater, less): \t", out[[7]]$p.value,"\t",out[[8]]$p.value,"\t",out[[9]]$p.value,"\n",file=OUTFILE,append=TRUE)
#		cat("p-value for Welch's t-test (two-sided, greater, less): \t", out[[7]]$statistic,"\t",out[[8]]$statistic,"\t",out[[9]]$statistic,"\n",file=OUTFILE,append=TRUE)
		}
	if(b[4]){
		cat("p-value for KW Test: ", out[[10]]$p.value,"\n",file=OUTFILE,append=TRUE)
		}
	if(b[5]){
		cat("p-value for ANOVA F Test: ", out[[11]]$p.value,"\n",file=OUTFILE,append=TRUE)
		}
	} else { cat("No test was performed",file=OUTFILE,append=TRUE)}