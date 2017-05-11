####fit the model, here I took log2foldchnage>3
###get 
store<-list()
for(i in rownames(up_fold)){
  print(i)
  line<- getCurve(fitModel(i))  
  store[[i]]<-line
}

#dat<-list()
#pdf('lfittedplot.pdf')
for (i in names(store)) {
  p<-data.frame (colData(fullData),t(counts(fullData, normalized=TRUE )[i, , drop=FALSE] ) ) %>%
    mutate ( tl = ifelse( time > 11, time - 11, 1e5 ) )  %>%
    gather ( "gene", "ncount", starts_with("NCU") )
  #dat[[i]]<-p
  p<-ggplot(data) + 
    geom_point(aes( x=time, y=log2(ncount)+1),data=subset( p, samplelabel=="dl-wt" ) ) +
    geom_line(aes( x=time, y=ncount), data=store[[i]] )+
    scale_y_continuous(trans="log2")
  p<- p+ xlab("Time[h]") + ylab("Log2(count)+1")
  p<-p+theme_bw()
  #p<-facet_wrap()
  ggsave(paste('~/Desktop/Light-dark/fitt/',i , '.png', sep=''), p)
  
}
#dev.off()
