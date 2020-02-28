rm(list=ls())

library(sf)
library(cartography)


mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"))
# Population density
mtq$POPDENS <- 1e6 * mtq$POP / st_area(x = mtq)
mtq$area=st_area(mtq)/1000000
mtq$paro=mtq$CHOM/mtq$POP
#Params
x=mtq
var="area"

#end


values<-st_drop_geometry(x)
values<-as.double(values[,var])
bks <- getBreaks(values, method = "quantile", nclass=15)
cols <- carto.pal(pal1 = "wine.pal", length(bks)-1)
df<-data.frame(values)



#Guess the breaks on hist
hbreaks<-length(unique(values))
bins<-pretty(df$values,n=hbreaks)
bins<-pmax(bins,1.01*min(df$values))
bins<-pmin(bins,max(df$values))

vcol<-cut(bins,bks, include.lowest = TRUE, labels = FALSE)
vcol<-as.data.frame(vcol)
vcol$col=cols[vcol$vcol]

hist(x=values, breaks=hbreaks,col = vcol$col,
     axes=FALSE,xlab="", ylab = "", main = "", border = NA)
axis(1, line=0.01, pos=NA, col=NA, col.ticks = "black")
axis(2, line=0.01, tck=1, col=NA, col.ticks="grey95", lwd.ticks="1", lwd=0.1, lty=3)
