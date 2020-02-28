rm(list=ls())

library(sf)
library(cartography)


mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"))
# Population density
mtq$POPDENS <- 1e6 * mtq$POP / st_area(x = mtq)
#Params
x=mtq
var="POPDENS"
bks <- getBreaks(mtq$POPDENS, method = "quantile", nclass=15)
cols <- carto.pal(pal1 = "wine.pal", length(bks)-1)
#end


values<-st_drop_geometry(x)
values<-as.double(values[,var])
df<-data.frame(values)


#Guess the breaks on hist
bins<-pretty(df$values,n=50)
bins<-pmax(bins,1.01*min(df$values))
bins<-pmin(bins,max(df$values))
vcol<-cut(bins,bks, include.lowest = TRUE, labels = FALSE)
vcol<-as.data.frame(vcol)
vcol$col=cols[vcol$vcol]

hist(x=values, breaks=50,col = vcol$col,
     axes=FALSE,xlab="", ylab = "", main = "", border = NA)
axis(1, line=0.01, pos=NA, col=NA, col.ticks = "black")
axis(2, line=0.01, tck=1, col=NA, col.ticks="grey95", lwd.ticks="1", lwd=0.1, lty=3)
