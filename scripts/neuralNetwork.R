#!/usr/bin/env Rscript

# set the maximum number of nested expressions that will be evaluated
options("expressions"=5e5)

# load packages nnet, caret, MLmetrics, plyr and mltools
library(nnet)
library(caret)
library(MLmetrics)
library(plyr)
library(mltools)


# Initialize variables
resultsACC<-data.frame()
resultsMCC<-setNames(data.frame(matrix(ncol=3,nrow=0)),c("Nome","Media","Desvio"))
resultsROC<-data.frame()
resultsAUC<-data.frame()

# read the data
# replace name_datasets with "asd" for the use case of autism spectrum disorder
# replace name_batch with "ratioa" for the batch-adjustment method ratioA
name_datasets="hf"
name_batch="combat"
nl<-scan(paste(name_datasets,"_id.txt",sep=""),what=character())
nc<-scan(paste(name_datasets,"_samples.txt",sep=""),what=character())
df_total<-read.table(paste(name_datasets,"_after_rem_batch_",name_batch,".txt",sep=""),header=FALSE,sep="\t",dec=".",row.names=nl,col.names=nc)
df_total<-as.data.frame(t(df_total))

# read the class of every sample (D: diseased or Z: control)
tipo<-scan(paste(name_datasets,"_type.txt",sep=""),what=character())

nomes=c("set1", "set2", "set3", "set4", "set5", "set6", "set7")

for(na in nomes){ 
  n<-scan(paste(na,".txt",sep=""),what=character())
  maxnumber=length(n)
  df<-df_total[,(colnames(df_total) %in% n) ]
  df["type"]<-tipo
  df$type=factor(df$type)
  
  # Tune the parameters size and decay
  control<-trainControl(method="repeatedcv", number=10,repeats=5,search="grid")
  tunegrid<-expand.grid(size=c(1:15), decay=c(0,0.5,0.1,5e-2,1e-2,5e-3,1e-3,5e-4,1e-4,5e-5))
  maxSize<-max(tunegrid$size)
  numWts<-maxSize*(length(df)+1)+maxSize+1
  set.seed(7)
  neuralNetTune<-train(type~.,data=df,method="nnet",metric="Accuracy", trControl=control, maxit=500,tuneGrid=tunegrid, trace=FALSE,MaxnwTS=numWts)
  si<-neuralNetTune$bestTune$size
  dec<-neuralNetTune$bestTune$decay
  write.csv2(neuralNetTune$results,file=paste(name_datasets,"_",na,"_",name_batch,"_neuralNet_results_tuning.txt",sep=""))
  
  # Get and write the results of the model
  control<-trainControl(method="repeatedcv", number=10,repeats=5,search="grid",savePred=T, classProb=T)
  tunegrid2<-expand.grid(size=si,decay=dec)
  set.seed(7)
  neuralNet<-train(type~.,data=df,method="nnet",metric="Accuracy", trControl=control, maxit=500,tuneGrid=tunegrid2, trace=FALSE,MaxnwTS=numWts)
  tabx<-as.data.frame(neuralNet$results)
  tabx["name"]<-c(na)
  tabx<-tabx[c("name","size","decay","Accuracy","AccuracySD","Kappa","KappaSD")]
  resultsACC<-rbind(resultsACC,tabx) 
  
  MCC_metric<-ddply(neuralNet$pred,"Resample",summarise, MCC=mcc(pred,obs))
  write.csv2(MCC_metric, file=paste(name_datasets,"_",na,"_",name_batch,"_neuralNet_mcc.txt",sep=""))
  media<-mean(MCC_metric$MCC)
  dsvp<-sd(MCC_metric$MCC)
  tabx<-data.frame(name=na,size=si,decay=dec,MCCmean=media,MCCSD=dsvp)
  resultsMCC<-rbind(resultsMCC,tabx)
  
  set.seed(7)
  control<-trainControl(method="repeatedcv", number=10,repeats=5,search="grid",classProbs = TRUE, summaryFunction = twoClassSummary)
  neuralNetROC<-train(type~.,data=df,method="nnet",metric="ROC", tuneGrid =tunegrid2, trControl=control,importance=TRUE,maxit=500,trace=FALSE,MaxnwTS=numWts)
  tabx<-as.data.frame(neuralNetROC$results)
  tabx["name"]<-c(na)
  tabx<-tabx[c("name","size","decay","ROC","ROCSD","Spec","SpecSD","Sens","SensSD")]
  resultsROC<-rbind(resultsROC,tabx)
  
  set.seed(7)
  control<-trainControl(method="repeatedcv", number=10,repeats=5,search="grid",classProbs = TRUE, summaryFunction = prSummary)
  neuralNetAUC<-train(type~.,data=df,method="nnet",metric="AUC", tuneGrid =tunegrid2, trControl=control,importance=TRUE,maxit=500,trace=FALSE,MaxnwTS=numWts)
  tabx<-as.data.frame(neuralNetAUC$results)
  tabx["name"]<-c(na)
  tabx<-tabx[c("name","size","decay","Precision","PrecisionSD","Recall","RecallSD","F","FSD","AUC","AUCSD")]
  resultsAUC<-rbind(resultsAUC,tabx)
}

write.csv2(resultsACC,file=paste(name_datasets,"_",name_batch,"_neuralNet_results_Accuracy.txt",sep=""))
write.csv2(resultsMCC,file=paste(name_datasets,"_",name_batch,"_neuralNet_results_MCC.txt",sep=""))
write.csv2(resultsROC,file=paste(name_datasets,"_",name_batch,"_neuralNet_results_ROC.txt",sep=""))
write.csv2(resultsAUC,file=paste(name_datasets,"_",name_batch,"_neuralNet_results_AUC.txt",sep=""))

