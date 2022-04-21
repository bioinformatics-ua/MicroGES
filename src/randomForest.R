#!/usr/bin/env Rscript

# set the maximum number of nested expressions that will be evaluated
options("expressions"=5e5)

# load packages randomForest, caret, MLmetrics, plyr, mltools, HEMDAG, and cvAUC
library(randomForest)
library(caret)
library(MLmetrics)
library(plyr)
library(mltools)
library(HEMDAG)
library(cvAUC)

# read the data
# replace name_datasets with "asd" for the use case of autism spectrum disorder
# replace name_batch with "fsva" for the batch-adjustment method fsva
name_dataset<-"hf"
batch<-"combat"
nc<-scan(paste(name_dataset,"_id.txt",sep=""),what=character())

for(na in 1:7){
  results<-data.frame()
  for (v in 1:50){
    accuracy=0
    
    # Read data of the trianing set
    nl<-scan(paste("train",v,"_",name_dataset,"_samples.txt",sep=""),what=character())
    df<-read.table(paste("train",v,"_",name_dataset,"_",batch,"_after.txt",sep=""),header=FALSE,sep="\t",dec=".",row.names=nl,col.names=nc)
    df<-as.data.frame(t(df))
    n<-scan(paste(name_dataset,"_set_",na,"_id.txt",sep=""),what=character())
    maxnumber=length(n)
    df<-df[(row.names(df) %in% n), ]
    df<-as.data.frame(t(df))
    tipo<-scan(paste("train",v,"_",name_dataset,"_type.txt",sep=""),what=character())
    df["type"]<-tipo
    df$type=factor(df$type)
    
    # Read data of the test set
    nltest<-scan(paste("test",v,"_",name_dataset,"_samples.txt",sep=""),what=character())
    dftest<-read.table(paste("test",v,"_",name_dataset,"_",batch,".txt",sep=""),header=FALSE,sep="\t",dec=".",row.names=nltest,col.names=nc)
    dftest<-data.frame(t(dftest))
    dftest<-dftest[(row.names(dftest) %in% n), ]
    dftest<-data.frame(t(dftest))
    tipotest<-scan(paste("test",v,"_",name_dataset,"_type.txt",sep=""),what=character())
    dftest["type"]<-tipotest
    dftest$type=factor(dftest$type)
    
    # Tune the parameters mtry and ntree using the training set
    for (j in c(100,500,1000)){
      notree=j
      nmtry=2
      set.seed(7)
      control<-trainControl(method="repeatedcv", number=10,search="grid")
      tunegrid<-expand.grid(.mtry=nmtry)
      rfn<-train(type~.,data=df,method="rf",metric="Accuracy", tuneGrid =tunegrid, trControl=control,importance=TRUE, ntree=notree)
      tab<-as.data.frame(rfn$results)
      tab["ntree"]<-c(notree)
      for (i in 3:maxnumber){
        nmtry=i
        set.seed(7)
        control<-trainControl(method="repeatedcv", number=10,search="grid")
        tunegrid<-expand.grid(.mtry=nmtry)
        rfn<-train(type~.,data=df,method="rf",metric="Accuracy", tuneGrid =tunegrid, trControl=control,importance=TRUE, ntree=notree)
        taba<-as.data.frame(rfn$results)
        taba["ntree"]<-c(notree)
        tab<-rbind(tab,taba) 
      }
      tab2<-tab[order(-tab$Accuracy),]
      if(tab2$Accuracy[1]>accuracy){
        accuracy=tab2$Accuracy[1]
        notree2=j
        nmtry2=tab2$mtry[1]
      }
    }
  
    # Get and write the results of the model
    control<-trainControl(method="repeatedcv", number=10,search="grid",savePred=T, classProb=T)
    tunegrid2<-expand.grid(mtry=nmtry2)
    set.seed(7)
    rfn<-train(type~.,data=df,method="rf",metric="Accuracy", trControl=control, tuneGrid=tunegrid2, importance=TRUE, ntree=notree2)
    rfpred <- predict(rfn,dftest)
    rfpred2 <- predict(rfn,dftest,type="prob")
    
    # Calculate the AUCROC and the AUCPR
    bin<-c()
    for (d in dftest$type) {
      if(d=="D"){
        bin<-append(bin,1)
      }
      else{
        bin<-append(bin,0)
      }
    }
    binr<-c()
    for (d in rfpred) {
      if(d=="D"){
        binr<-append(binr,1)
      }
      else{
        binr<-append(binr,0)
      }
    }
    AUCPR<-AUPRC.single.class(bin,binr)
    AUCROC<-AUC(binr,bin)
    
    # Calculate MCC
    macc<-mcc(rfpred,dftest$type)
    
    # Obtain the confusion matrix
    confmatrix<-confusionMatrix(rfpred,dftest$type)   
    tabx<-as.data.frame(confmatrix$overall)
    tabx<-t(tabx)
    taby<-as.data.frame(confmatrix$byClass)
    taby<-t(taby)
    tabx<-merge(tabx,taby)
    tabx["ROC"]<-c(AUCROC)
    tabx["AUCPR"]<-c(AUCPR)
    tabx["MCC"]<-c(macc)
    tabx["mtry"]<-c(nmtry2)
    tabx["ntree"]<-c(notree2)
    tabx["set"]<-c(paste("train",v,sep=""))
    tabx<-tabx[c("set","mtry","ntree","Accuracy","Specificity","Precision","Recall","F1", "MCC", "ROC","AUCPR", "Kappa", "Pos Pred Value","Neg Pred Value","Prevalence","Detection Rate","Detection Prevalence","Balanced Accuracy","AccuracyLower","AccuracyUpper","AccuracyNull","AccuracyPValue","McnemarPValue")]
    results<-rbind(results,tabx)
    
    # Write the results
    write.csv2(varImp(rfn,scale=FALSE)$importance, file=paste(name_dataset,"_set_",na,"_",batch,"_test_",v,"_varImp_rf.txt",sep=""))
    capture.output(confmatrix,file=paste(name_dataset,"_set_",na,"_",batch,"_test_",v,"_confmatrix_rf.txt",sep=""))
    write.csv2(rfpred2,file=paste(name_dataset,"_set_",na,"_",batch,"_test_",v,"_predprobability_rf.txt",sep=""))
  }
  write.csv2(results,file=paste(name_dataset,"_set_",na,"_",batch,"_results_rf.txt",sep=""))
}
  
  
  