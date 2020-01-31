library(sf)
mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"), quiet = TRUE)


#Local file
dirpng = system.file("img/LogoMartinique.png", package = "cartography")

#Remote
remoteurl="https://i.imgur.com/gePiDvB.png"


expect_true(methods::is(getPngLayer(x=mtq, pngpath=dirpng), "RasterBrick"))
expect_true(methods::is(getPngLayer(x=mtq, pngpath=remoteurl), "RasterBrick"))
expect_true(methods::is(getPngLayer(x=mtq, pngpath=remoteurl, dwmode="curl"), "RasterBrick"))
expect_error(getPngLayer(x=mtq, pngpath=dirpng, align ="fake" ))
expect_silent(getPngLayer(mtq, dirpng))
expect_silent(getPngLayer(mtq, dirpng, align = "left"))
expect_silent(getPngLayer(mtq, dirpng, align = "right"))
expect_error(getPngLayer(x=mtq, pngpath="https://i.imgur.com/2CJpz98.jpg"))