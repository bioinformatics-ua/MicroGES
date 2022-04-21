#!/usr/bin/env Rscript

# set the maximum number of nested expressions that will be evaluated
options("expressions"=5e5)

# load packages nnet, caret, MLmetrics, plyr, mltools, HEMDAG, and cvAUC
library(nnet)
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
    
    # Tune the parameters size and decay using the training set
    set.seed(7)
    control<-trainControl(method="repeatedcv", number=10,search="grid")
    tunegrid<-expand.grid(size=c(1:15), decay=c(0,0.5,0.1,5e-2,1e-2,5e-3,1e-3,5e-4,1e-4,5e-5))
    maxSize<-max(tunegrid$size)
    numWts<-maxSize*(length(df)+1)+maxSize+1
    neuralNet<-train(type~.,data=df,method="nnet",metric="Accuracy", trControl=control, maxit=500,tuneGrid=tunegrid, trace=FALSE,MaxnwTS=numWts)
    tab<-as.data.frame(neuralNet$results)
    si<-neuralNet$bestTune$size
    dec<-neuralNet$bestTune$decay
    
    # Get the results of the model using the test set
    control<-trainControl(method="repeatedcv", number=10,search="grid",savePred=T, classProb=T)
    tunegrid2<-expand.grid(size=si,decay=dec)
    set.seed(7)
    neuralNet<-train(type~.,data=df,method="nnet",metric="Accuracy", trControl=control, maxit=500,tuneGrid=tunegrid2, trace=FALSE,MaxnwTS=numWts)
    neuralnetpred <- predict(neuralNet,dftest)
    neuralnetpred2 <- predict(neuralNet,dftest,type="prob")
   
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
    for (d in neuralnetpred) {
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
    macc<-mcc(neuralnetpred,dftest$type)
    
    # Obtain the confusion matrix
    confmatrix<-confusionMatrix(neuralnetpred,dftest$type)   
    tabx<-as.data.frame(confmatrix$overall)
    tabx<-t(tabx)
    taby<-as.data.frame(confmatrix$byClass)
    taby<-t(taby)
    tabx<-merge(tabx,taby)
    tabx["ROC"]<-c(AUCROC)
    tabx["AUCPR"]<-c(AUCPR)
    tabx["MCC"]<-c(macc)
    tabx["size"]<-c(si)
    tabx["decay"]<-c(dec)
    tabx["set"]<-c(paste("train",v,sep=""))
    tabx<-tabx[c("set","size","decay","Accuracy","Specificity","Precision","Recall","F1", "MCC", "ROC","AUCPR","Kappa", "Pos Pred Value","Neg Pred Value","Prevalence","Detection Rate","Detection Prevalence","Balanced Accuracy","AccuracyLower","AccuracyUpper","AccuracyNull","AccuracyPValue","McnemarPValue")]
    results<-rbind(results,tabx)
    
    # Write the results
    write.csv2(varImp(neuralNet,scale=FALSE)$importance, file=paste(name_dataset,"_set_",na,"_",batch,"_test_",v,"_varImp_nn.txt",sep=""))
    capture.output(confmatrix,file=paste(name_dataset,"_set_",na,"_",batch,"_test_",v,"_confmatrix_nn.txt",sep=""))
    write.csv2(neuralnetpred2,file=paste(name_dataset,"_set_",na,"_",batch,"_test_",v,"_predprobability_nn.txt",sep=""))
  }
  write.csv2(results,file=paste(name_dataset,"_set_",na,"_",batch,"_results_nn.txt",sep=""))
}