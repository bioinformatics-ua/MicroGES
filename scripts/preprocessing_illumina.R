#!/usr/bin/env Rscript

# loads package limma
library(limma)

#read non-normalized data
dfillumina<-read.ilmn("GSE111175_GSE42133_non-normalized_data.txt",expr = "Signal",probeid = "PROBE_ID")

#correct background using normexp
dfbk<-backgroundCorrect(dfillumina,method = "normexp")

#normalize using quantile
dfnorm<-normalizeBetweenArrays(dfbk,method = "quantile")

#write the normalized data to a text file
write.table(dfnorm$E,"dataGPL10558.txt")
