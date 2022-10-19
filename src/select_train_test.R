#!/usr/bin/env Rscript

# set the maximum number of nested expressions that will be evaluated
options("expressions"=5e5)

library(caret)
name_datasets="asd"

# read the data frame with the expressions
nl<-scan(paste(name_datasets,"_id.txt",sep=""),what=character())
nc<-scan(paste(name_datasets,"_samples.txt",sep=""),what=character())
df<-read.table(paste(name_datasets,"_before_rem_batch.txt",sep=""),header=FALSE,sep="\t",dec=".",row.names=nl,col.names=nc)
df1<-read.table(paste(name_datasets,"_after_rem_batch_combat.txt",sep=""),header=FALSE,sep="\t",dec=".",row.names=nl,col.names=nc)
df2<-read.table(paste(name_datasets,"_after_rem_batch_fsva.txt",sep=""),header=FALSE,sep="\t",dec=".",row.names=nl,col.names=nc)
df<-as.data.frame(t(df))
df1<-as.data.frame(t(df1))
df2<-as.data.frame(t(df2))

# read the class of every sample (D: diseased or Z: control)
tipo<-factor(scan(paste(name_datasets,"_type.txt",sep=""),what=character()))
df["type"]<-tipo
df1["type"]<-tipo

# read the platform used to obtain the expression of a sample
platform<-factor(scan(paste(name_datasets,"_platform.txt",sep=""),what=character()))
df["platform"]<-platform

for(i in 1:50){
  set.seed(i*7)
  inTraining <- createDataPartition(df$type, p = .75, list = FALSE)
  train_limma<-df[inTraining,]
  train_rf1<-df1[inTraining,]
  test1<-df1[-inTraining,] 
  train_rf2<-df2[inTraining,]
  test2<-df2[-inTraining,]
  
  write.table(train_rf1$type,file = paste("train",i,"_",name_datasets,"_type.txt",sep=""),col.names = FALSE, sep="\t",row.names = FALSE)
  write.table(test1$type,file = paste("test",i,"_",name_datasets,"_type.txt",sep=""),col.names = FALSE, sep="\t",row.names = FALSE)
  train_limma$type<-NULL
  train_rf1$type<-NULL
  test1$type<-NULL

  write.table(train_limma$platform,paste("train",i,"_",name_datasets,"_platform.txt",sep=""),col.names = FALSE, sep="\t",row.names = FALSE) 
  train_limma$platform<-NULL

  write.table(train_limma,file = paste("train",i,"_",name_datasets,"_before.txt",sep=""),col.names = FALSE, sep="\t",row.names = FALSE)
  write.table(train_rf1,file = paste("train",i,"_",name_datasets,"_combat_after.txt",sep=""),col.names = FALSE, sep="\t",row.names = FALSE)
  write.table(train_rf2,file = paste("train",i,"_",name_datasets,"_fsva_after.txt",sep=""),col.names = FALSE, sep="\t",row.names = FALSE)
  write.table(test1,file = paste("test",i,"_",name_datasets,"_combat.txt",sep=""),col.names = FALSE, sep="\t",row.names = FALSE)
  write.table(test2,file = paste("test",i,"_",name_datasets,"_fsva.txt",sep=""),col.names = FALSE, sep="\t",row.names = FALSE)
  write.table(row.names(train_rf1),file = paste("train",i,"_",name_datasets,"_samples.txt",sep=""),col.names = FALSE, sep="\t",row.names = FALSE)
  write.table(row.names(test1),file = paste("test",i,"_",name_datasets,"_samples.txt",sep=""),col.names = FALSE, sep="\t",row.names = FALSE)
}
