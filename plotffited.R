####fit the model, here I took log2foldchnage>2
###get 
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
    scale_y_continuous(trans="log2")+ facet_wrap(~gene)
  p<- p+ xlab("Time[h]") + ylab("Log2(count)+1")
  p<-p+theme_bw()+coord_fixed(ratio=.5)
  p<-p+ scale_x_continuous(breaks = c(0,2, 4, 6,8,10,12,14,16,18,20,22)) 
  p<-p+ ylim(0,16)
 p<-p+theme(axis.text = element_text (face="bold", size=14), axis.title=element_text(size=18,face="bold"))
 p<-p+theme(legend.title = element_text(size=16, face="bold"),legend.text = element_text(size = 16, face = "bold"))
 p<-p+theme(strip.text = element_text(size=25,face="bold"))
  #p<-facet_wrap()
   print(i)
ggsave(paste('~/Desktop/Light-dark/fitt/',i , '.png', sep=''),width=9, height=5,p)
  
}
#dev.off()
