#!/usr/bin/env Rscript

# set the maximum number of nested expressions that will be evaluated
options("expressions"=5e5)

# load package limma
library(limma)

#replace name_datasets with "asd" for the usecase of autism spectrum disorder
name_datasets="hf"

#define the names of the files
gse_names<-c()
for(i in 1:50){
  gse_names[i]=paste("train",i,"_",name_datasets,sep="")
}

#read the names of the columns
nc<-scan(paste(name_datasets,"_id.txt",sep=""),what=character())
length(nc)

for (name_gse in gse_names) {
  # read the data frame with the expressions
  nl<-scan(paste(name_gse,"_samples.txt",sep=""),what=character())
  df<-read.table(paste(name_gse,"_before.txt",sep=""),header=FALSE,sep="\t",dec=".",row.names=nl,col.names=nc)
  df<-as.data.frame(t(df))
  
  # read the class of every sample (D: diseased or Z: control)
  tipo<-factor(scan(paste(name_gse,"_type.txt",sep=""),what=character()))
  
  # read the platform used to obtain the expression of a sample
  platform<-factor(scan(paste(name_gse,"_platform.txt",sep=""),what=character()))
  
  # create the design matrix
  design.matrix<-model.matrix(~0+tipo+platform)
  
  # define a contrast matrix
  contrast.matrix<-makeContrasts(Diff=tipoD-tipoZ,levels = design.matrix)
  
  # fit the linear model
  fit1<-lmFit(df,design.matrix)
  
  # extract the linear model fit for the contrasts
  fit2<-contrasts.fit(fit1,contrast.matrix)
  fit3<-eBayes(fit2)
  
  # write the results in the file <name_gse>_limma.txt
  write.csv2(topTable(fit3,coef='Diff',number=nrow(df)),file=paste(name_gse,"_limma.txt",sep=""))
  
  # obtain GBACC id of the different feature sets
  j<-1
  if(name_datasets == "hf"){
    for(i in seq(1.50,3.00,0.25)){
      tab<-topTable(fit3,coef='Diff',number=nrow(df), sort.by = "logFC",lfc = log2(i))
      write(row.names(tab),file=paste(name_gse,"_set_",j,"_id.txt",sep=""))
      j<-j+1
    }
  } else if(name_datasets == "asd") {
    for(p in c(5e-2,4e-2,3e-2,2e-2,1e-2,9e-3,8e-3)){
      tab<-topTable(fit3,coef='Diff',number=nrow(df), sort.by = "p",p.value = p)
      write(row.names(tab),file=paste(name_gse,"_set_",j,"_id.txt",sep=""))
      j<-j+1
    }
  }
}
  
#Intersect the different feature sets 
if(name_datasets == "hf"){
    for(j in 1:7){
      inter<-scan(paste(gse_names[1],"_set_",j,"_id.txt",sep=""),what = character())
      for(name_gse in gse_names){
      v<-scan(paste(name_gse,"_set_",j,"_id.txt",sep=""),what = character())
      inter<-intersect(inter,v)
      }
      write(inter,file=paste(name_datasets,"_set_",j,"_id.txt",sep=""))
    }
  } else if(name_datasets == "asd") {
    for(j in 1:7){
      inter<-scan(paste(gse_names[1],"_set_",j,"_id.txt",sep=""),what = character())
      for(name_gse in gse_names){
        v<-scan(paste(name_gse,"_set_",j,"_id.txt",sep=""),what = character())
        inter<-intersect(inter,v)
      }
      write(inter,file=paste(name_datasets,"_set_",j,"_id.txt",sep=""))
    }
  }    
    
