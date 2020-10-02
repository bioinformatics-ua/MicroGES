#!/usr/bin/env Rscript

# set the maximum number of nested expressions that will be evaluated
options("expressions"=5e5)

# load packages Biobase and sva
library(Biobase)
library(sva)

# read the data
# replace name_datasets with "asd" for the usecase of autism spectrum disorder
name_datasets="hf"
nl<-scan(paste(name_datasets,"_id.txt",sep=""),what=character())
nc<-scan(paste(name_datasets,"_samples.txt",sep=""),what=character())
df<-read.table(paste(name_datasets,"_before_rem_batch.txt",sep=""),header=FALSE,sep="\t",dec=".",row.names=nl,col.names=nc)
ex<-new("ExpressionSet",exprs=as.matrix(df))
edata<-exprs(ex)
df<-as.data.frame(t(df))

# read the platform used to obtain the expression of a sample
platform<-scan(paste(name_datasets,"_platform.txt",sep=""),what=character())
df["platform"]<-platform
df$platform=factor(df$platform)

# read the class of every sample (D: diseased or Z: control)
tipo<-scan(paste(name_datasets,"_type.txt",sep=""),what=character())
df["type"]<-tipo
df$type=factor(df$type)

# remove Batch
mod=model.matrix(~df$type)
batch=df$platform
batch=as.numeric(batch)
cleandata<-ComBat(edata,batch,mod)

# write data frame after removing batch to a text file
write.table(cleandata, file=paste(name_datasets,"_after_rem_batch_combat.txt",sep=""),sep="\t",dec=".", row.names = FALSE, col.names = FALSE )
