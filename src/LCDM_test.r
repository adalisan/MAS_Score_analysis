args=commandArgs()

REGION=args[6]

LCDM <- function(fileNames,condTests,REGION){

#arg7=number of MCI, arg8=number of normal

DATA_NUMB<-c(as.integer(args[7]),as.integer(args[8]))

#b1=TRUE;b2=TRUE;b3=TRUE;b4=TRUE;b5=TRUE;
#condTests<-c(b1,b2,b3,b4,b5)

### Pre-Processing ###
library(MASS)
b<-list(); dists<-c(); distsl<-c();
for (i in 1:length(fileNames)){ 
    distsl[i]<-read.table(fileNames[i])
    distsl[[i]]=distsl[[i]][distsl[[i]]>-1.75] 
     }
minlength=min(as.numeric(lapply(distsl,length)))
dists=sapply(distsl,sample,minlength) #for tests requiring same number in group

######### Simple Summary Statistics ########

#pdf(file=paste("summarystat_",REGION,".pdf",sep=''))
pdf(file=paste("summarystat_test_",REGION,".pdf",sep=''))

#if(length(fileNames)==2){boxplot(dists[,1],dists[,2])}
#if(length(fileNames)==3){boxplot(dists[,1],dists[,2],dists[,3])}
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
#    	lines(dens1$x,dens1$y*length(distsl[[i]]), lwd=2, col=col[i])
    	lines(dens1, lwd=1, col=col[i])

	thk95[i]=quantile(distsl[[i]],.95)
	thk99[i]=quantile(distsl[[i]],.99)
	print(DATA_NUMB[i])
	vol95[i]=length(distsl[[i]][distsl[[i]]<thk95[i]])/DATA_NUMB[i]
	vol99[i]=length(distsl[[i]][distsl[[i]]<thk99[i]])/DATA_NUMB[i]

	}
legend(6, .3, paste(c('Type','MCI','normal'),c(' thk95.mm',round(thk95,4)),c(' thk99.mm.',round(thk99,4)),c(' vol95.mm3',round(vol95,4)),c(' vol99.mm3',round(vol99,4)),sep=", "), cex=1,  col= c('white','red','blue'),lwd=2, bty="n");
#legend(6, .3, paste(c('MCI','normal'),sep=", "), cex=1,  col= c('red','blue'),lwd=2, bty="n");

for(i in 1:length(fileNames)){ 
	cdf1=ecdf(distsl[[i]])
	if (i==1) 
	   {
	   plot(cdf1, verticals=TRUE, xlim=c(-2,7), lwd=.5, col=col[i])#,do.points=FALSE,)
#           hist(distsl[[i]], 100, freq=FALSE, col="white", border="white", xlim=c(-4,10), ylim=c(0,1),main=REGION,xlab="Distance (mm) ",ylab="Density")
    	   par(new=T);
	   }		
	else { lines(cdf1, lwd=.5, col=col[i]) }
	}
legend(5, .3, paste(c('MCI','normal'),sep=", "), cex=1,  col= c('red','blue'),lwd=2, bty="n");

dev.off()

########## 2-group tests ###########
if(ncol(dists)==2){

## Mann-Whitney Test (Wilcoxon Rank Sum) ##
if(condTests[1]==TRUE){
b[[1]]<-wilcox.test(distsl[[1]],distsl[[2]],alternative="t",exact = FALSE,correct = FALSE)
b[[2]]<-wilcox.test(distsl[[1]],distsl[[2]],alternative="g",exact = FALSE,correct = FALSE)
b[[3]]<-wilcox.test(distsl[[1]],distsl[[2]],alternative="l",exact = FALSE,correct = FALSE)
}

## Kolmogorov-Smirnov Test (KS Test) ##
if(condTests[2]==TRUE){
b[[4]]<-ks.test(distsl[[1]],distsl[[2]],alternative="t",exact=NULL)
b[[5]]<-ks.test(distsl[[1]],distsl[[2]],alternative="l",exact=NULL)
b[[6]]<-ks.test(distsl[[1]],distsl[[2]],alternative="g",exact=NULL)
}

## Welch's t Test for Independent Samples ##
if(condTests[3]==TRUE){
b[[7]]<-t.test(distsl[[1]],distsl[[2]],alternative = "t",mu = 0, paired = FALSE, var.equal = FALSE,conf.level = 0.95)
b[[8]]<-t.test(distsl[[1]],distsl[[2]],alternative = "g",mu = 0, paired = FALSE, var.equal = FALSE,conf.level = 0.95)
b[[9]]<-t.test(distsl[[1]],distsl[[2]],alternative = "l",mu = 0, paired = FALSE, var.equal = FALSE,conf.level = 0.95)
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

file1=paste(REGION,"_normtest1_aMCI_wave1_pooled_antsy.txt",sep='')
file2=paste(REGION,"_normtest2_aMCI_wave1_pooled_antsy.txt",sep='')
fileNames=c(file1,file2)

b <- c("Kolmogorov-Smirnov Test")#,"Kruskal-Wallis Test","ANOVA F-test")
condTests<-c()
condTests[1]="Mann Whitney U Test"%in%b
condTests[2]="Kolmogorov-Smirnov Test"%in%b
condTests[3]="Welch's t-test"%in%b
condTests[4]="Kruskal-Wallis Test"%in%b
condTests[5]="ANOVA F-test"%in%b
b<-condTests
out<-LCDM(fileNames,condTests,REGION)


### write results into file ###

#OUTFILE=paste("/cis/project/sydney/pooled_antsy_files/result_",REGION,".txt",sep='')
OUTFILE=paste("/cis/project/sydney/pooled_antsy_files/test_",REGION,".txt",sep='')
OUTFILE2=paste("/cis/project/sydney/pooled_antsy_files/test_KS_",REGION,".txt",sep='')

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
		cat(out[[4]]$statistic,"\t",out[[5]]$statistic,"\t",out[[6]]$statistic,"\n",file=OUTFILE2,append=TRUE)
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