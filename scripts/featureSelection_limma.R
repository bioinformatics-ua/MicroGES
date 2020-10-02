#!/usr/bin/env Rscript

# set the maximum number of nested expressions that will be evaluated
options("expressions"=5e5)

# load package limma
library(limma)

# read the data
# replace name_datasets with "asd" for the use case of autism spectrum disorder
name_datasets="hf"
nl<-scan(paste(name_datasets,"_id.txt",sep=""),what=character())
nc<-scan(paste(name_datasets,"_samples.txt",sep=""),what=character())
df<-read.table(paste(name_datasets,"_before_rem_batch.txt",sep=""),header=FALSE,sep="\t",dec=".",row.names=nl,col.names=nc)
  
# read the class of every sample (D: diseased or Z: control)
tipo<-scan(paste(name_datasets,"_type.txt",sep=""),what=character())
  
# read the platform used to obtain the expression of a sample
platform<-scan(paste(name_datasets,"_platform.txt",sep=""),what=character())
  
# create the design matrix
design.matrix<-model.matrix(~0+tipo+platform)
  
# define a contrast matrix
contrast.matrix<-makeContrasts(Diff=tipoZ-tipoD,levels = design.matrix)
  
# fit the linear model
fit1<-lmFit(df,design.matrix)
  
# extract the linear model fit for the contrasts
fit2<-contrasts.fit(fit1,contrast.matrix)
fit3<-eBayes(fit2)
  
# write the results in the file <name_datasets>_limma.txt
write.csv2(topTable(fit3,coef='Diff',number=nrow(df), sort.by = "logFC"),file=paste(name_datasets,"_limma.txt",sep=""))
  
# obtain GBACC id of the different feature sets
j<-1
if(name_datasets == "hf"){
  for(i in seq(1.50,3.00,0.25)){
    tab<-topTable(fit3,coef='Diff',number=nrow(df), sort.by = "logFC",lfc = log2(i))
    write(row.names(tab),file=paste("set",j,".txt",sep=""))
    j<-j+1
  }
} else if(name_datasets == "asd") {
  for(p in c(2.5e-4,1e-4,7.5e-5,5e-5,2.5e-5,1e-5,5e-6)){
    tab<-topTable(fit3,coef='Diff',number=nrow(df), sort.by = "p",p.value = p)
    write(row.names(tab),file=paste("set",j,".txt",sep=""))
    j<-j+1
  }
}
