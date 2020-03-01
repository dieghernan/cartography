library(sf)
mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"), quiet = TRUE)
plot(st_geometry(mtq))
s=sample(seq(1,100),20, replace =TRUE)

expect_silent(maphist(mtq$POP))
expect_silent(maphist(s, axes=FALSE, pos="bottom"))
expect_silent(maphist(s,hist.width = 3,hist.height =  5, axes=FALSE, pos="top"))
expect_silent(maphist(s,title.txt = "ABCde", 
                      title.cex=3, pos="right", frame=TRUE))

plot(st_geometry(mtq))
centr=st_coordinates(st_centroid(st_union(mtq)))
expect_silent(maphist(s, col=colors(),
                      hist.width = 0.1,
                      hist.height = 0.1,
                       pos=centr,
                      frame=TRUE))
expect_silent(maphist(s,pos="nada"))



          