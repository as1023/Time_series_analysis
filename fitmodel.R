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
####extract fitted value for plot 
store<-list()
for(i in rownames(up)){
  print(i)
  line<- getCurve(fitModel(i))  
  store[[i]]<-line
}
