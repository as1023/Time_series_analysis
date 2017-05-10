library(dplyr)
library(ggplus)
library(tidyr)

#####plot all ressignificant gene in one pdf file 

pdf('light_inducibale_gene.pdf')
p<- data.frame(colData(fullData),t(counts(fullData, normalized=TRUE)[rownames(ressig),][i] ) ) %>%
gather( "gene", "ncount", -(1:5) ) %>%
ggplot + geom_line(aes( x=time, y=log2(ncount+1), col=samplelabel)) +facet_wrap(~gene)
p<- p + xlab( "Time[h]") + ylab("Log2(count)+1") + theme_bw()
facet_multiple( plot = p, facets = 'gene', ncol = 4, nrow = 4)

######plot single gene 

p<-data.frame( colData(fullData),t(counts(fullData, normalized=TRUE)["NCU00552", , drop=FALSE] ) ) %>%
gather( "gene", "ncount", -(1:5) )%>%
ggplot +geom_line(aes(x=time, y=log2(ncount+1), col=samplelabel)) + facet_wrap(~gene)
p+ xlab("Time[h]") + ylab("Log2(count)+1") + theme_bw()

###### plot in one gene in one png file

d<-as.vector(row.names(ressig))
for ( i in 1:length(d)){
p<-data.frame(colData(fullData),t(counts(fullData, normalized=TRUE) [i, , drop=FALSE]) ) %>%
gather( "gene", "ncount", -(1:5) )%>%
ggplot +geom_line(aes(x=time, y=log2(ncount+1), col=samplelabel)) + facet_wrap(~ gene[i])
p<- p+ xlab( "Time[h]") + ylab("Log2(count)+1") 
p<-p+theme_bw()
ggsave(paste('~/Desktop/plot/plot_',i , '.png', sep=''), p)
print(i)
}

########test html plot 
 HTMLoutput=file.path(".","output.html")
 graph1="light_inducibale_gene.png"
 HTMLInsertGraph(graph1,file=HTMLoutput,caption="Sample discrete distribution plot")
