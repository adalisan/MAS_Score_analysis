


LCDM_analysis <- function(fileNames,condTests,REGION, pairs.to.compare,
                 analysis.tag,group.names){
  ### Pre-Processing 
  require(MASS)
  num.groups <- length(group.names)
  b<-list(); dists<-c(); distsl<-c();
  for (i in 1:length(fileNames)){ 
    distsl[i]<-read.table(fileNames[i])
    distsl[[i]]=distsl[[i]][distsl[[i]]>-2] 
    distsl[[i]]=distsl[[i]][distsl[[i]]<8] 
  }
  minlength=min(as.numeric(lapply(distsl,length)))
  dists=sapply(distsl,sample,minlength) #for tests requiring same number in group
  
  ######### Simple Summary Statistics ########
  
  subdir.name <- paste0("test_results_",analysis.tag )
  if (!(subdir.name %in% list.dirs()))  (dir.create(subdir.name))
  pdf(file=paste(subdir.name,"/summarystat_",REGION,".pdf",sep=''))
  
  col= rainbow(num.groups)
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
  legend(6, .3, paste(group.names), cex=1,  col= col,lwd=2, bty="n");
  
  for(i in 1:length(fileNames)){ 
    cdf1=ecdf(round(distsl[[i]],3))
    if (i==1) 
    {
      plot(cdf1, verticals=TRUE, xlim=c(-2,7),do.points=F, lwd=.5, col=col[i], main=NA,xlab="Distance (mm)", ylab="Cumulative Probability Density")
      par(new=T);
    }		
    else { lines(cdf1, lwd=.5, col=col[i]) }
  }
  legend(5, .3, paste(group.names,sep=", "), cex=1,  col= col,lwd=2, bty="n");
  
  dev.off()
  
  
  ########## 2-group tests ###########
  test.i <- 1
  ## Mann-Whitney Test (Wilcoxon Rank Sum) ##
  if(condTests[1]==TRUE){
    for (pair in 1:num.pairs){
      group.pair.compare  <- pairs.to.compare[pair,]
      grp.1  <- pairs.to.compare[pair,1]
      grp.2  <- pairs.to.compare[pair,2]
      b[[test.i]]<-wilcox.test(distsl[[grp.2]], distsl[[grp.1]],
                               alternative="t",exact = FALSE,
                               correct = FALSE)
      b[[test.i+1]]<-wilcox.test(distsl[[grp.2]], distsl[[grp.1]],
                                 alternative="g", exact = FALSE,
                                 correct = FALSE)
      b[[test.i+2]]<-wilcox.test(distsl[[grp.2]], distsl[[grp.1]],
                                 alternative="l", exact = FALSE,
                                 correct = FALSE)
      test.i<- test.i +3
    }
    
  }
  
  ## Kolmogorov-Smirnov Test (KS Test) ##
  if(condTests[2]==TRUE){
    for (pair in 1:num.pairs){
      grp.1  <- pairs.to.compare[pair,1]
      grp.2  <- pairs.to.compare[pair,2]
      b[[test.i]]<-ks.test(distsl[[grp.2]], distsl[[grp.1]],
                           alternative="t",exact = NULL)
      b[[test.i+1]]<-ks.test(distsl[[grp.2]], distsl[[grp.1]],
                             alternative="l",exact = NULL)
      b[[test.i+2]]<-ks.test(distsl[[grp.2]], distsl[[grp.1]],
                             alternative="g",exact = NULL)
      test.i<- test.i +3
    }
  }
  
  
  ## Welch's t Test for Independent Samples ##
  if(condTests[3]==TRUE){
    for (pair in 1:num.pairs){
      grp.1  <- pairs.to.compare[pair,1]
      grp.2  <- pairs.to.compare[pair,2]
      
      b[[test.i]]<-ks.test(distsl[[grp.2]], distsl[[grp.1]],
                           alternative = "t",mu = 0, paired = FALSE, 
                           var.equal = FALSE,conf.level = 0.95)
      b[[test.i+1]]<-ks.test(distsl[[grp.2]], distsl[[grp.1]],
                             alternative = "l",mu = 0, paired = FALSE, 
                             var.equal = FALSE,conf.level = 0.95)
      b[[test.i+2]]<-ks.test(distsl[[grp.2]], distsl[[grp.1]],
                             alternative = "g",mu = 0, paired = FALSE, 
                             var.equal = FALSE,conf.level = 0.95)
      test.i<- test.i +3
    }
  }
  return(b)
}




diag.values <- c("normal","aMCI","amdMCI","nMCI","nmdMCI")
subject.list.files <-  paste0("Diag_",diag.values,"_ID_list.csv")
names(subject.list.files) <- diag.values

diag.pairs <- matrix(c(c(1,2),c(1,3),c(1,4),c(1,5), c(2,3),c(4,5)),
                       7,2,byrow=TRUE)


pooled_analysis_results <- LCDM_analysis (LCDM.stat.fileNames,condTests=rep(TRUE,3),
              REGION, pairs.to.compare,
              analysis.tag="five_class_Wave1",
              group.names=diag.values)
  