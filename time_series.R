library(Cairo)
CairoFonts(
  regular="Arial:style=Regular",
  bold="Arial:style=Bold",
  italic="Arial:style=Italic",
  bolditalic="Arial:style=Bold Italic,BoldItalic",
  symbol="Symbol")
library(DESeq2)
library(pheatmap)
library(ggplot2)
library(dplyr)
library(dplyr)
library(ggplus)
library(tidyr)
library(openxlsx)

setwd("/Users/amitsingh/Desktop/Light-dark/")
meta<-read.delim("meta_1.txt",header=T,sep="\t")
sampleTable<-data.frame(sampleName=meta$condition, fileName=meta$fileName, condition=meta$sampletype,time=meta$time,sampletype=meta$sampletype,samplelabel=meta$samplelabel)
fullData <- DESeqDataSetFromHTSeqCount(sampleTable=sampleTable,directory = ".",design = ~ 1)
fullData <- estimateSizeFactors(fullData)
norm<-counts(fullData, normalized=TRUE)
save(fullData,file="fullData.rda")
dds <- fullData[,fullData$samplelabel=="dl-wt" & fullData$time %in% c(4,6,8,10,11.5)]
design(dds) <- ~ condition
dds <- DESeq(dds)
res <- results(dds, contrast=c("condition", "light", "dark"))
ressig<- subset(res, padj <0.1)
up<-subset(res, padj <0.1 & log2FoldChange >0)
down<-subset(res, padj <0.1 & log2FoldChange 0<)
#####annotate gene 
anno<-read.delim("Ncrassa.genes.FungiDB.txt",header=T,sep="\t")
anno<-anno[,1:2]
idx<-unique(anno[anno$X.Gene.ID.%in% rownames(ressig),])
final=as.data.frame(cbind(ressig,idx))
write.xlsx(final,file="light_inducible_gene.xlsx",asTable = TRUE)
####plot all ressig
 pdf('light_inducibale_gene.pdf')
 p<-data.frame(colData(fullData),t(counts(fullData, normalized=TRUE)[rownames(ressig),] ) ) %>%
 gather( "gene", "ncount", -(1:5) ) %>%
 ggplot +geom_line(aes(x=time, y=log2(ncount+1), col=samplelabel)) +facet_wrap(~gene)
 p<-p + xlab( "Time[h]") + ylab("Log2(count)+1") + theme_bw()
 facet_multiple(plot = p, facets = 'gene', ncol = 4, nrow = 4)
dev.off()
######heatmap 

mat <- log2(norm[rownames(ressig)[ressig$log2FoldChange>0], ] + 1 )
mat <- mat - rowMeans( mat[,c(3:6)] )
mat <- mat[ order( -mat[,"dl-wt-11.5"] ), ]
#mat <- mat[ order( -rowMeans(mat[,9:11]) ), ]

#mat <- mat[ order( ressig[rownames(mat),"pvalue"] ), ]

CairoPDF(file="up regulated gene heatmap",width=12/1.54,height=15/1.54)
heatmap.2( mat[,c(1:14)], col=greenred(100), trace="none",
           Colv=FALSE, Rowv=FALSE, dendrogram = "none",
           breaks = c(-10, seq(-2, 2, length.out=99), 10 ),main="up regulated light indicuble gene")
dev.off()
####
k<-0.5
##logfold changege
A<-5.768127
p<-data.frame (colData(fullData),t(counts(fullData, normalized=TRUE )["NCU00552", , drop=FALSE] ) ) %>%
  mutate ( tl = ifelse( time > 11, time - 11, 1e5 ) )  %>%
  gather ( "gene", "ncount", starts_with("NCU") )

#data<-p[,c(6,8)]
#data<-data[1:14,]

#tl<-data$tl
#mu<- mean(data$ncount)

fit <- nls(
  ncount ~ ifelse( time>11, mu + A*exp(-k*(time-11)), muD ),
  algorithm="port",
  start = c( mu=300, A=2^5, k=0.5, muD=300 ), 
  lower = c( mu=1, A=2^0, k=0.01, muD=1 ), 
  upper = c( mu=1e5, A=2^20, k=50, muD=1e5 ), 
  data=subset(p, samplelabel=="dl-wt" & time>0 ), trace=TRUE )
fittedline <- data.frame( time = seq( 0, 22, length.out=1000 ) )
fittedline$tl <- ifelse( fittedline$time > 11, fittedline$time - 11, 1e5 )
fittedline$ncount <- predict( fit, fittedline )
ggplot(data) + 
  geom_point(aes( x=time, y=ncount), data=subset( p, samplelabel=="dl-wt" ) ) +
  geom_line(aes( x=time, y=ncount), data=fittedline ) +
  scale_y_continuous(trans="log2")

#write.xlsx(ressig,file="light_inducible_gene.xlsx",asTable = TRUE)
######## for all gene 
#dds<-fullData[,fullData$samplelabel=="dl-wt" & fullData$time %in% c(0,2,4,6,8,10,11,11.5,12,14,16,18,20,22)]
#dds<-counts(dds,normalized=TRUE)[rownames(up),]

p<-data.frame(colData(fullData),t(counts(fullData, normalized=TRUE )[rownames(up),] ) ) %>%
mutate ( tl = ifelse( time > 11, time - 11, 1e5 ) )  %>%
 gather( "gene", "ncount", starts_with("NCU")) 

fitModel <- function(gene) {
    df <- subset(
    data.frame(
       colData( fullData ),
       ncount = counts( fullData, normalized=TRUE )[ gene, ] ),
  fullData$samplelabel == "dl-wt" )
  nls( log2(ncount+1) ~ ifelse( time > 11, mu + A*exp(-k*(time-11)), muD ),
      algorithm="port",
      start = c( mu=300, A=2^5, k=0.5, muD=300 ), 
      lower = c( mu=1, A=2^0, k=0.001, muD=0.001 ), 
      upper = c( mu=1e8, A=2^20, k=50, muD=1e8 ), 
      data=df )
}

getCurve <- function( fit ) {
  fittedline <- data.frame( time = seq( 0, 22, length.out=1000 ) )
  fittedline$ncount <- predict( fit, fittedline )
  fittedline
}








