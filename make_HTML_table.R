library( hwriter )
#load( "final.rda" )
final$log2FoldChange <- sprintf( "%.2f", final$log2FoldChange )
plotfiles <- paste0( rownames(final), ".png" )
plotfiles2<-paste0( rownames(final), ".jpeg" )
final$plot <-hwriteImage(plotfiles, height="70px", table=FALSE, link = plotfiles )
final$plot2<-hwriteImage(plotfiles2, height="70px", table=FALSE, link = plotfiles2 )
maintable <- hwrite( final, 
                     table.style = "border-collapse: collapse;",
                     cellpadding = 5,row.bgcolor='#ffdc98',
                     center=TRUE,onmouseover="this.bgColor='#ffaaaa'", 
                     onmouseout="this.bgColor='white'", bgcolor='white')

hwrite(maintable, page="table.html")
#####ressig plot 
ressig$log2FoldChange <- sprintf( "%.2f", ressig$log2FoldChange )
plotfiles <- paste0( rownames(ressig), ".png" )
ressig$plot <-hwriteImage(plotfiles, height="70px", table=FALSE, link = plotfiles )
#final$plot2<-hwriteImage(plotfiles2, height="70px", table=FALSE, link = plotfiles2 )
maintable <- hwrite( ressig, 
                     table.style = "border-collapse: collapse;",
                     cellpadding = 5,row.bgcolor='#ffdc98',
                     center=TRUE,onmouseover="this.bgColor='#ffaaaa'", 
                     onmouseout="this.bgColor='white'", bgcolor='white')
hwrite(maintable, page="differential regulated gene table.html")
