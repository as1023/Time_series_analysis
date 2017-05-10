library(DESeq2)
#####load the meta file 
setwd("/Users/amitsingh/Desktop/Light-dark/")
meta<-read.delim("meta_1.txt",header=T,sep="\t")
sampleTable<-data.frame(sampleName=meta$condition, fileName=meta$fileName, condition=meta$sampletype,time=meta$time,sampletype=meta$sampletype,samplelabel=meta$samplelabel)
#### load full data 
fullData <- DESeqDataSetFromHTSeqCount(sampleTable=sampleTable,directory = ".",design = ~ 1)
fullData <- estimateSizeFactors(fullData)
####normalize the fullData
norm<-counts(fullData, normalized=TRUE)
###save normalized data
save(fullData,file="fullData.rda")
#####contrast between light and dark 
dds <- fullData[,fullData$samplelabel=="dl-wt" & fullData$time %in% c(4,6,8,10,11.5)]
design(dds) <- ~ condition
dds <- DESeq(dds)
res <- results(dds, contrast=c("condition", "light", "dark"))
ressig<- subset(res, padj <0.1)
up<-subset(res, padj <0.1 & log2FoldChange >0)
down<-subset(res, padj <0.1 & log2FoldChange <0)

