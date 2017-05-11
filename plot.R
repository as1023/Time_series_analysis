library(dplyr)
library(ggplus)
library(tidyr)

#####plot all ressignificant gene in one pdf file 

pdf('light_inducibale_gene.pdf')
p<- data.frame(colData(fullData),t(counts(fullData, normalized=TRUE)[rownames(ressig),][i]))[-58,]%>%
gather( "gene", "ncount", -(1:5) ) %>%
ggplot + geom_line(aes( x=time, y=log2(ncount+1), col=samplelabel)) +facet_wrap(~gene)
p<- p + xlab( "Time[h]") + ylab("Log2(count)+1") + theme_bw()
p<-p+geom_point(aes(x=time,y=log2(ncount+1),col=samplelabel))
facet_multiple( plot = p, facets = 'gene', ncol = 4, nrow = 4)

######plot single gene 

p<-data.frame( colData(fullData),t(counts(fullData, normalized=TRUE)["NCU00552", , drop=FALSE]))[-58,]%>%
gather( "gene", "ncount", -(1:5) )%>%
ggplot +geom_line(aes(x=time, y=log2(ncount+1), col=samplelabel)) + facet_wrap(~gene)
p+ xlab("Time[h]") + ylab("Log2(count)+1") + theme_bw()

###### plot in one gene in one png file, removed one sample for fulldata
for( i in row.names(ressig))
      {
p<-data.frame(colData(fullData),t(counts(fullData, normalized=TRUE)[i, , drop=FALSE]) )[-58,] %>%
gather( "gene", "ncount", -(1:5) )%>%
ggplot +geom_line(aes(x=time, y=log2(ncount+1), col=samplelabel)) + facet_wrap(~gene)
p<- p+ xlab("Time[h]") + ylab("Log2(count)+1")
p<-p+theme_bw()
p<-p+geom_point(aes(x=time,y=log2(ncount+1),col=samplelabel))
#p<-p+ labs(title = "i")
ggsave(paste('~/Desktop/Light-dark/plot/',i , '.png', sep=''), p)
#print(i)
}


########test html plot 
 HTMLoutput=file.path(".","output.html")
 graph1="light_inducibale_gene.png"
 HTMLInsertGraph(graph1,file=HTMLoutput,caption="Sample discrete distribution plot")
