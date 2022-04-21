#!/usr/bin/env Rscript

# set the maximum number of nested expressions that will be evaluated
options("expressions"=5e5)

# load packages bapred
library(bapred)

# read the data
# replace name_datasets with "asd" for the usecase of autism spectrum disorder
name_datasets="hf"
nl<-scan(paste(name_datasets,"_id.txt",sep=""),what=character())
nc<-scan(paste(name_datasets,"_samples.txt",sep=""),what=character())
df<-read.table(paste(name_datasets,"_before_rem_batch.txt",sep=""),header=FALSE,sep="\t",dec=".",row.names=nl,col.names=nc)
df<-as.matrix(t(df))

# read the platform used to obtain the expression of a sample
batchlabels<-scan(paste(name_datasets,"_platform.txt",sep=""),what=character())

#set the labels
batchnames<-unique(batchlabels)
batchlabels<-gsub(batchnames[1],"1",batchlabels)
batchlabels<-gsub(batchnames[2],"2",batchlabels)
batchlabels<-gsub(batchnames[3],"3",batchlabels)

# read the class of every sample (D: diseased or Z: control)
tipo<-factor(scan(paste(name_datasets,"_type.txt",sep=""),what=character()))
tipo2<-1:length(tipo)
for(i in 1:length(tipo)){
  if(tipo[i]=='D'){
    tipo2[i]<-"1"
  } else {
    tipo2[i]<-"2"
  }
}
tipo2<-factor(tipo2)

# remove Batch
df2<-svaba(df,tipo2,batchlabels)

# write data frame after removing batch to a text file
write.table(t(df2$xadj), file=paste(name_datasets,"_after_rem_batch_fsva.txt",sep=""), sep="\t",dec=".", row.names = FALSE, col.names = FALSE )