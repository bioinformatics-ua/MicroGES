#!/usr/bin/env Rscript

# set the maximum number of nested expressions that will be evaluated
options("expressions"=5e5)

# load packages randomForest, caret, MLmetrics, plyr and mltools
library(randomForest)
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
  accuracy=0
  n<-scan(paste(na,".txt",sep=""),what=character())
  maxnumber=length(n)
  df<-df_total[,(colnames(df_total) %in% n) ]
  df["type"]<-tipo
  df$type=factor(df$type)
  
  #Tune the parameters mtry and ntree  
    for (j in c(100,500,1000)){
    notree=j
    nmtry=2
    set.seed(7)
    control<-trainControl(method="repeatedcv", number=10,repeats=5,search="grid")
    tunegrid<-expand.grid(.mtry=nmtry)
    rfn<-train(type~.,data=df,method="rf",metric="Accuracy", tuneGrid =tunegrid, trControl=control,importance=TRUE, ntree=notree)
    tab<-as.data.frame(rfn$results)
    tab["ntree"]<-c(notree)
    for (i in 3:maxnumber){
      nmtry=i
      set.seed(7)
      control<-trainControl(method="repeatedcv", number=10,repeats=5,search="grid")
      tunegrid<-expand.grid(.mtry=nmtry)
      rfn<-train(type~.,data=df,method="rf",metric="Accuracy", tuneGrid =tunegrid, trControl=control,importance=TRUE, ntree=notree)
      tabx<-as.data.frame(rfn$results)
      tabx["ntree"]<-c(notree)
      tab<-rbind(tab,tabx) 
    }
    tab2<-tab[order(-tab$Accuracy),]
    if(tab2$Accuracy[1]>accuracy){
      accuracy=tab2$Accuracy[1]
      notree2=j
      nmtry2=tab2$mtry[1]
    }
    write.csv2(tab,file=paste(name_datasets,"_",na,"_",name_batch, "_",j,"_randomFoest_results_tuning.txt",sep=""))
  }
  
  # Get and write the results of the model
  control<-trainControl(method="repeatedcv", number=10,repeats=5,search="grid",savePred=T, classProb=T)
  tunegrid2<-expand.grid(mtry=nmtry2)
  set.seed(7)
  rfn<-train(type~.,data=df,method="rf",metric="Accuracy", trControl=control, tuneGrid=tunegrid2, importance=TRUE, ntree=notree2)
  tabx<-as.data.frame(rfn$results)
  tabx["ntree"]<-c(notree2)
  tabx["na"]<-c(na)
  tabx<-tabx[c("na","mtry","ntree","Accuracy","AccuracySD","Kappa","KappaSD")]
  resultsACC<-rbind(resultsACC,tabx)
  write.csv2(varImp(rfn,scale=FALSE)$importance, file=paste(name_datasets,"_",na,"_",name_batch,"_varImp.txt",sep=""))
  
  MCC_metric<-ddply(rfn$pred,"Resample",summarise, MCC=mcc(pred,obs))
  write.csv2(MCC_metric, file=paste(name_datasets,"_",na,"_",name_batch,"_randomForest_mcc.txt",sep=""))
  media<-mean(MCC_metric$MCC)
  dsvp<-sd(MCC_metric$MCC)
  tabx<-data.frame(na=na,mtry=nmtry2,ntree=notree2,MCCmean=media,MCCSD=dsvp)
  resultsMCC<-rbind(resultsMCC,tabx)
  
  set.seed(7)
  control<-trainControl(method="repeatedcv", number=10,repeats=5,search="grid",classProbs = TRUE, summaryFunction = twoClassSummary)
  rfROC<-train(type~.,data=df,method="rf",metric="ROC", tuneGrid =tunegrid2, trControl=control,importance=TRUE,ntree=notree2)
  tabx<-as.data.frame(rfROC$results)
  tabx["ntree"]<-c(notree2)
  tabx["na"]<-c(na)
  tabx<-tabx[c("na","mtry","ntree","ROC","ROCSD","Spec","SpecSD","Sens","SensSD")]
  resultsROC<-rbind(resultsROC,tabx)
  
  set.seed(7)
  control<-trainControl(method="repeatedcv", number=10,repeats=5,search="grid",classProbs = TRUE, summaryFunction = prSummary)
  rfAUC<-train(type~.,data=df,method="rf",metric="AUC", tuneGrid =tunegrid2, trControl=control,importance=TRUE,ntree=notree2)
  tabx<-as.data.frame(rfAUC$results)
  tabx["ntree"]<-c(notree2)
  tabx["na"]<-c(na)
  tabx<-tabx[c("na","mtry","ntree","Precision","PrecisionSD","Recall","RecallSD","F","FSD","AUC","AUCSD")]
  resultsAUC<-rbind(resultsAUC,tabx)
}

write.csv2(resultsACC,file=paste(name_datasets,"_",name_batch,"_randomForest_results_Accuracy.txt",sep=""))
write.csv2(resultsMCC,file=paste(name_datasets,"_",name_batch,"_randomForest_results_MCC.txt",sep=""))
write.csv2(resultsROC,file=paste(name_datasets,"_",name_batch,"_randomForest_results_ROC.txt",sep=""))
write.csv2(resultsAUC,file=paste(name_datasets,"_",name_batch,"_randomForest_results_AUC.txt",sep=""))

  
  
  