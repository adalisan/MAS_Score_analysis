library(foreign)
#Read cognitive score and Dx files
HOME_DIR <- Sys.getenv("HOME")

setwd(paste0(HOME_DIR,"/projects/MAS_Score_Analysis/",collapse=""))
MAS.normal.df <- read.spss("./data/M_w123_npsych_v20130718/M_all_npsych_normal_v20120921.sav")

MAS.normal.df<- as.data.frame(MAS.normal.df[c(1:17,143:180,184:203,207:209)])


MAS.healthy.df <- read.spss("./data/M_w123_npsych_v20130718/M_all_npsych_healthy_v20120921.sav")
MAS.healthy.df<- as.data.frame(MAS.healthy.df[c(1:14,140:177,181:200,204:206)])

# We use the subclassification from  M_123_dx_v20121023.sav
MCI_class <- read.spss("./data/M_123_dx_v20121023.sav")
subclass.df<-as.data.frame(MCI_class[c(1:3,7,8,11,27,44)])
#W1_subclass <- read.csv("./data/W1_MCI classifications_NORMS_based_ESBs only_for Anne.csv")
#W2_subclass <- read.csv("./data/MAS W1and2_Diagnostic_classifications.csv")



# Merge files
W1_dir_tag <- "_MAS.20100921"
W1_dir_tag_s <- "_MAS.20110324"
W2_dir_tag <- "_wave2"
subregions <- outer(c("lh_","rh_"),c("stg","itg","mtg","antcing","postcing"),paste0)


# Read (prev. computed)aggregate statistics of LCDM measure from csv files
# put them in data frames where columns are subregions
#wave 1 files
OUTPUT_DIR <- "./data/"
DATA_DIR <- ""
DATA_DIR_W1 <-paste0("./data/data_analysis",W1_dir_tag)
DATA_DIR_W1_s <-paste0("./data/data_analysis",W1_dir_tag_s)
DATA_DIR_W2 <- paste0("./data/data_analysis",W2_dir_tag)
lcdm_files_w1 <- c()
lcdm_files_w2 <- c()
LCDM_thick_W1 <-data.frame()
LCDM_vol_W1    <-data.frame()
LCDM_surfarea_W1 <- data.frame()
LCDM_ICV_W1 <- c()
for (SUBREGION in subregions){
  file_name <- paste(DATA_DIR_W1,"/",SUBREGION,"/",SUBREGION,"_lcdmstats.csv",sep="")
  lcdm_files_w1 <- c(lcdm_files_w1,file_name)
  temp_df <- read.csv(file_name,sep=" ",stringsAsFactors=FALSE)
  id_dates<-temp_df$IDS
  ids <- substr(id_dates,1,4)
  if (nrow(LCDM_thick_W1)==0){
  LCDM_thick_W1 <- data.frame(ID=ids,stringsAsFactors=FALSE)
  rownames(LCDM_thick_W1) <-ids 
  }
  if (nrow(LCDM_vol_W1)==0){
  LCDM_vol_W1 <- data.frame(ID=ids,stringsAsFactors=FALSE)
  rownames(LCDM_vol_W1) <-ids
  }
  if (nrow(LCDM_surfarea_W1)==0){
  LCDM_surfarea_W1<- data.frame(ID=ids,stringsAsFactors=FALSE)
  rownames(LCDM_surfarea_W1) <-ids
  }
  if (length(LCDM_ICV_W1)==0){
    LCDM_ICV_W1<-rep(NA,length(ids))
    names(LCDM_ICV_W1) <-ids
  }
  
  #"thk95.mm." "thk99.mm." "vol95.mm3." "vol99.mm3." "surfArea.mm2." "ICV"
  LCDM_thick_W1[ids,SUBREGION]<-temp_df$thk95.mm.
  LCDM_vol_W1[ids,SUBREGION]<-temp_df$vol95.mm3.
  LCDM_surfarea_W1[ids,SUBREGION]<-temp_df$surfArea.mm2.  
  
  if ("ICV" %in% names(temp_df)){
    LCDM_ICV_W1[ids] <- temp_df$ICV
  }  
}



LCDM_thick_W1_s    <- data.frame()
LCDM_vol_W1_s      <- data.frame()
LCDM_surfarea_W1_s <- data.frame()
LCDM_ICV_W1_s <- c()
for (SUBREGION in subregions){
  file_name <- paste(DATA_DIR_W1_s,"/",SUBREGION,"/",SUBREGION,"_lcdmstats.csv",sep="")
  lcdm_files_w1 <- c(lcdm_files_w1,file_name)
  temp_df <- read.csv(file_name,sep=" ",stringsAsFactors=FALSE)
  id_dates<-temp_df$IDS
  
  ids <- substr(id_dates,1,4)
  print (ids)
  if (nrow(LCDM_thick_W1_s)==0){
    LCDM_thick_W1_s <- data.frame(ID=ids,stringsAsFactors=FALSE)
    rownames(LCDM_thick_W1_s) <-ids 
  }
  if (nrow(LCDM_vol_W1_s)==0){
    LCDM_vol_W1_s <- data.frame(ID=ids,stringsAsFactors=FALSE)
    rownames(LCDM_vol_W1_s) <-ids
  }
  if (nrow(LCDM_surfarea_W1_s)==0){
    LCDM_surfarea_W1_s<- data.frame(ID=ids,stringsAsFactors=FALSE)
    rownames(LCDM_surfarea_W1_s) <-ids
  }
  if (length(LCDM_ICV_W1_s)==0){
    LCDM_ICV_W1_s<-rep(NA,length(ids))
    names(LCDM_ICV_W1_s) <-ids
  }
  
  
  #"thk95.mm." "thk99.mm." "vol95.mm3." "vol99.mm3." "surfArea.mm2." "ICV"
  
  LCDM_thick_W1_s[ids,SUBREGION]<-temp_df$thk95.mm.
  
  LCDM_vol_W1_s[ids,SUBREGION]<-temp_df$vol95.mm3.
  
  LCDM_surfarea_W1_s[ids,SUBREGION]<-temp_df$surfArea.mm2.
  
  if ("ICV" %in% names(temp_df)){
    LCDM_ICV_W1_s[ids] <- temp_df$ICV
  }
}

LCDM_thick_W1<-rbind(LCDM_thick_W1,LCDM_thick_W1_s)
LCDM_vol_W1<-rbind(LCDM_vol_W1,LCDM_vol_W1_s)
LCDM_surfarea_W1<-rbind(LCDM_surfarea_W1,LCDM_surfarea_W1_s)
LCDM_ICV_W1<- c(LCDM_ICV_W1,LCDM_ICV_W1_s)
#names(LCDM_ICV_W1)<- as.character(as.numeric(names(LCDM_ICV_W1)))


LCDM_thick_W2 <-data.frame()
LCDM_vol_W2    <-data.frame()
LCDM_surfarea_W2 <- data.frame()
LCDM_ICV_W2 <- c()
for (SUBREGION in subregions){
  file_name <- paste(DATA_DIR_W2,"/",SUBREGION,"/",SUBREGION,"_lcdmstats.csv",sep="")
  lcdm_files_w2 <- c(lcdm_files_w2,file_name)
  temp_df <- read.csv(file_name,sep=" ",stringsAsFactors=FALSE)
  id_dates<-temp_df$IDS
  ids <- substr(id_dates,1,4)
  if (nrow(LCDM_thick_W2)==0){
    LCDM_thick_W2 <- data.frame(ID=ids,stringsAsFactors=FALSE)
    rownames(LCDM_thick_W2) <-ids 
  }
  if (nrow(LCDM_vol_W2)==0){
    LCDM_vol_W2 <- data.frame(ID=ids,stringsAsFactors=FALSE)
    rownames(LCDM_vol_W2) <-ids
  }
  if (nrow(LCDM_surfarea_W2)==0){
    LCDM_surfarea_W2<- data.frame(ID=ids,stringsAsFactors=FALSE)
    rownames(LCDM_surfarea_W2) <-ids
  }
  
  if (length(LCDM_ICV_W2)==0){
    LCDM_ICV_W2<-rep(NA,length(ids))
    names(LCDM_ICV_W2) <-ids
  }
  
  #"thk95.mm." "thk99.mm." "vol95.mm3." "vol99.mm3." "surfArea.mm2." "ICV"
  LCDM_thick_W2[ids,SUBREGION]<-temp_df$thk95.mm.
  LCDM_vol_W2[ids,SUBREGION]<-temp_df$vol95.mm3.
  LCDM_surfarea_W2[ids,SUBREGION]<-temp_df$surfArea.mm2.  
  
  if ("ICV" %in% names(temp_df)){
    LCDM_ICV_W2[ids] <- temp_df$ICV
  }
}


#names(LCDM_ICV_W2)<- as.character(as.numeric(names(LCDM_ICV_W2)))


MAS.normal.df$ID<-substr(as.character(MAS.normal.df$ID),1,4)
MAS.healthy.df$ID<-substr(as.character(MAS.normal.df$ID),1,4)

all.files.merged.df <-merge(MAS.normal.df,MAS.healthy.df, by="ID")
all.files.merged.df <- merge(all.files.merged.df,LCDM_thick_W1, by="ID",all.x=TRUE)
all.files.merged.df <- merge(all.files.merged.df,LCDM_vol_W1, by="ID",all.x=TRUE,
                             suffixes=c("_thick_W1","_vol_W1"))
all.files.merged.df <- merge(all.files.merged.df,LCDM_surfarea_W1, by="ID",all.x=TRUE,
                             suffixes=c("_vol_W1","_surfarea_W1"))
all.files.merged.df <- merge(all.files.merged.df,LCDM_thick_W2, by="ID",all.x=TRUE,
                             suffixes=c("_surfarea_W1","_thick_W2"))
all.files.merged.df <- merge(all.files.merged.df,LCDM_vol_W2, by="ID",all.x=TRUE,
                             suffixes=c("_thick_W2","_vol_W2"))
all.files.merged.df <- merge(all.files.merged.df,LCDM_surfarea_W2, by="ID",all.x=TRUE,
                             suffixes=c("vol_W2","_surfarea_W2"))


all.df <-cbind(data.frame(ID=all.files.merged.df$ID,W1_subclass=subclass.df$M_W1_Classification_subtype,
                    W2_subclass=subclass.df$M_W2_Classification_subtype,
                    W1h_Global_Cogn_Score =all.files.merged.df$W1h_GlobalCognition,
                    W2h_Global_Cogn_Score =all.files.merged.df$W2h_GlobalCognition,
                    W1n_Global_Cogn_Score =all.files.merged.df$W1n_GlobalCognition,
                    W2n_Global_Cogn_Score =all.files.merged.df$W2n_GlobalCognition,
                    W1_Ed_year= subclass.df$M_W1_Education,
                    sex=all.files.merged.df$Sex.x,
                    age=all.files.merged.df$W1_Age.x
                    
                    #,LCDM_thick_W1,LCDM_Area_W1,W1_LCDM_Vol_W1,LCDM_list_index_W1                    
                    #,LCDM_thick_W2,LCDM_Area_W2,W1_LCDM_Vol_W2,LCDM_list_index_W2
                    ),
               all.files.merged.df[,153:212])  #column 153 should be the first LCDM measure 

rownames(all.df)<- all.df$ID
all.df$ICV_W1<-rep(NA,nrow(all.df))
all.df[names(LCDM_ICV_W1),"ICV_W1"]<-LCDM_ICV_W1[names(LCDM_ICV_W1)]

first.LCDM.meas.ind<-which(names(all.df)=="age")+1
LCDM_measures<- names(all.df)[first.LCDM.meas.ind:(first.LCDM.meas.ind+59)]
#Run analysis on 4 subtypes+ normal
all.df$W1_subclass[all.df$W1_subclass=="no complaints, aMCI"]<-"aMCI"
all.df$W1_subclass[all.df$W1_subclass=="no complaints, nMCI"]<-"nMCI"
all.df$W1_subclass[all.df$W1_subclass=="no complaints, amdMCI"]<-"amdMCI"
all.df$W1_subclass[all.df$W1_subclass=="no complaints, nmdMCI"]<-"nmdMCI"

classes.for.analysis <- c("normal","aMCI","nMCI","amdMCI","nmdMCI")
all.df<- all.df[all.df$W1_subclass %in% classes.for.analysis,]


#The merged data.frame is ready
print(paste0(nrow(all.df), " subjects used in the following analysis"))

#Output file lists (the list of patient IDs) for each diagnosis
for (class.label in classes.for.analysis) {
  rows.for.class <-(all.df$W1_subclass ==class.label)
IDs.for.class <- all.df$ID[rows.for.class] 
gender.for.class <- all.df$sex[rows.for.class] 
fname <- paste0("./data/Diag_",class.label,"_ID_list.csv") 
write.table(file=fname,IDs.for.class,row.names=FALSE,col.names=FALSE,quote=FALSE)

fname <- paste0("./data/Diag_",class.label,"_ID_list_male.csv")
IDs.for.class.m <- IDs.for.class[gender.for.class=="Male"]
write.table(file=fname, IDs.for.class.m,row.names=FALSE,col.names=FALSE,quote=FALSE)

fname <- paste0("./data/Diag_",class.label,"_ID_list_female.csv")
IDs.for.class.f <- IDs.for.class[gender.for.class=="Female"]
write.table(file=fname, IDs.for.class.f,row.names=FALSE,col.names=FALSE,quote=FALSE)
}


# Starting ANOVA analyses
p.cutoff<- 0.005

LCDM_measures_W1 <- LCDM_measures[1:30]
LCDM_measures_W2 <- LCDM_measures[31:60]
sink(paste0(OUTPUT_DIR,"LCDM_anova_wrt_Wave1_subclass.txt"))

for (dep.var in LCDM_measures){
  model.form <- as.formula(paste(dep.var,"~age+sex+ICV_W1+W1_subclass")) #age+sex+ICV+
  anova.fit<-aov(model.form,data=all.df)
  anova.fit.summ<-summary(anova.fit)
  
  if (anova.fit.summ[[1]][,5][4]<p.cutoff){
  print(dep.var)
  #print(anova.fit)
  print("        ")
  print(" p.val for subclass       ")
  print(anova.fit.summ[[1]][,5][4])
  print("        ")
  print(anova.fit)
  }
    
}
sink()

p.cutoff<- 0.005
#dep.var <- "W1h_Global_Cogn_Score"
dep.var <- "W1n_Global_Cogn_Score"
sink(paste0(OUTPUT_DIR,"LCDM_anova_Wave1_CognScore_wrt_LCDM_measure.txt"))
for (indep.var in LCDM_measures_W1){
  model.form <- as.formula(paste(dep.var,"~age+sex+ICV_W1+",indep.var)) #age+sex+ICV+
  anova.fit<-aov(model.form,data=all.df)
  anova.fit.summ<-summary(anova.fit)
  
  if (anova.fit.summ[[1]][,5][4]<p.cutoff){
    print(dep.var)
    #print(anova.fit)
    print("        ")
    print(" p.val for subclass       ")
    print(anova.fit.summ[[1]][,5][4])
    print("        ")
    print(anova.fit)
  }
  
}

sink()




p.cutoff<- 0.005
#dep.var <- "W2h_Global_Cogn_Score"
dep.var <- "W2n_Global_Cogn_Score"
sink(paste0(OUTPUT_DIR,"LCDM_anova_Wave2_CognScore_wrt_LCDM_measure.txt"))
for (indep.var in LCDM_measures_W1){
  model.form <- as.formula(paste(dep.var,"~age+sex+ICV_W1+",indep.var)) #age+sex+ICV+
  anova.fit<-aov(model.form,data=all.df)
  anova.fit.summ<-summary(anova.fit)
  
  if (anova.fit.summ[[1]][,5][4]<p.cutoff){
    print(dep.var)
    #print(anova.fit)
    print("        ")
    print(" p.val for subclass       ")
    print(anova.fit.summ[[1]][,5][4])
    print("        ")
    print(anova.fit)
  }
  
}

sink()



p.cutoff<- 0.005

all.df$Score_change <- all.df$W2n_Global_Cogn_Score-all.df$W1n_Global_Cogn_Score
sink(paste0(OUTPUT_DIR,"LCDM_anova_CognScore_change_wrt_LCDM_measure.txt"))
for (indep.var in LCDM_measures_W1){
  model.form <- as.formula(paste("Score_change~age+sex+ICV_W1+",indep.var)) #age+sex+ICV+
  anova.fit<-aov(model.form,data=all.df)
  anova.fit.summ<-summary(anova.fit)
  
  if (anova.fit.summ[[1]][,5][4]<p.cutoff){
    print(dep.var)
    #print(anova.fit)
    print("        ")
    print(" p.val for subclass       ")
    print(anova.fit.summ[[1]][,5][4])
    print("        ")
    print(anova.fit)
  }
  
}




sink()