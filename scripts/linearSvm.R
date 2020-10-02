#!/usr/bin/env Rscript

# set the maximum number of nested expressions that will be evaluated
options("expressions"=5e5)

# load packages e1071, caret, MLmetrics, plyr and mltools
library(e1071)
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
  tunegrid<-expand.grid(cost=c(seq(0.1,0.95,by=0.05),seq(0.01,0.095,by=0.005),seq(0.001,0.009,by=0.001),seq(0.0001,0.0009,by=0.0002),1:90,95,100,seq(125,300,by=25)))
  set.seed(7)
  svmLinearTune<-train(type~.,data=df,method="svmLinear2",metric="Accuracy", trControl=control, tuneGrid =tunegrid, importance=TRUE)
  co=svmLinearTune$bestTune$cost
  write.csv2(svmLinearTune$results,file=paste(name_datasets,"_",na,"_",name_batch,"_svmLinear_results_tuning.txt",sep=""))

  # Get and write the results of the model
  control<-trainControl(method="repeatedcv", number=10,repeats=5,search="grid",savePred=T, classProb=T)
  tunegrid2<-expand.grid(cost=co)
  set.seed(7)
  svmLinear<-train(type~.,data=df,method="svmLinear2",metric="Accuracy", trControl=control, tuneGrid =tunegrid2, importance=TRUE)
  tabx<-as.data.frame(svmLinear$results)
  tabx["name"]<-c(na)
  tabx<-tabx[c("name","cost","Accuracy","AccuracySD","Kappa","KappaSD")]
  resultsACC<-rbind(resultsACC,tabx)
  
  MCC_metric<-ddply(svmLinear$pred,"Resample",summarise, MCC=mcc(pred,obs))
  write.csv2(MCC_metric, file=paste(name_datasets,"_",na,"_",name_batch,"_svmLinear_mcc.txt",sep=""))
  media<-mean(MCC_metric$MCC)
  dsvp<-sd(MCC_metric$MCC)
  tabx<-data.frame(name=na,cost=co,MCCmean=media,MCCSD=dsvp)
  resultsMCC<-rbind(resultsMCC,tabx)
  
  set.seed(7)
  control<-trainControl(method="repeatedcv", number=10,repeats=5,search="grid",classProbs = TRUE, summaryFunction = twoClassSummary)
  svmLinearROC<-train(type~.,data=df,method="svmLinear2",metric="ROC", tuneGrid =tunegrid2, trControl=control,importance=TRUE)
  tabx<-as.data.frame(svmLinearROC$results)
  tabx["name"]<-c(na)
  tabx<-tabx[c("name","cost","ROC","ROCSD","Spec","SpecSD","Sens","SensSD")]
  resultsROC<-rbind(resultsROC,tabx)
  
  set.seed(7)
  control<-trainControl(method="repeatedcv", number=10,repeats=5,search="grid",classProbs = TRUE, summaryFunction = prSummary)
  svmLinearAUC<-train(type~.,data=df,method="svmLinear2",metric="AUC", tuneGrid =tunegrid2, trControl=control,importance=TRUE)
  tabx<-as.data.frame(svmLinearAUC$results)
  tabx["name"]<-c(na)
  tabx<-tabx[c("name","cost","Precision","PrecisionSD","Recall","RecallSD","F","FSD","AUC","AUCSD")]
  resultsAUC<-rbind(resultsAUC,tabx)
}

write.csv2(resultsACC,file=paste(name_datasets,"_",name_batch,"_svmLinear_results_Accuracy.txt",sep=""))
write.csv2(resultsMCC,file=paste(name_datasets,"_",name_batch,"_svmLinear_results_MCC.txt",sep=""))
write.csv2(resultsROC,file=paste(name_datasets,"_",name_batch,"_svmLinear_results_ROC.txt",sep=""))
write.csv2(resultsAUC,file=paste(name_datasets,"_",name_batch,"_svmLinear_results_AUC.txt",sep=""))
