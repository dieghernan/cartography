#' @title \code{*.png} Layer
#' @name getPngLayer
#' @description Get a \code{RasterBrick} from a \code{*.png} image cut using the shape of a spatial object. The \code{*.png} file could be either a local file or extracted from a given url.
#' @param x an sf object, a simple feature collection (POLYGON or MULTIPOLYGON).  
#' @param pngpath local path or url of a \code{*.png} file. 
#' @param align set how the \code{*.png} file should be fitted within \code{x}. Possible values are \code{'left'},\code{'right'} or \code{'center'}.
#' @param inverse  logical. If \code{FALSE}, overlapped areas of \code{x} on \code{pngpath} are extracted, otherwise non-overlapping areas are returned. See \code{\link[raster:mask]{mask}}.
#' @param dwmode Set the download mode. It could be \code{'base'} for \code{\link[utils:download.file]{download.file}} or \code{'curl'} for \code{\link[curl:curl_download]{curl_download}}.
#' @param ... additional arguments for downloading the file. See \code{\link[utils:download.file]{download.file}} or \code{\link[curl:curl_download]{curl_download}}.
#' @return A \code{RasterBrick} object is returned. 
#' @note The accuracy of the final plot would depend on the quality of the \code{*.png} file, 
#' the scale of \code{x} and the resolution setup of the graphic device.
#' @author dieghernan \url{https://stackoverflow.com/users/7877917/dieghernan}
#' @seealso \link{pngLayer}
#' @examples 
#' \dontrun{
#' library(sf)
#' 
#' mtq <- st_read(system.file("gpkg/mtq.gpkg", package = "cartography"))
#' 
#' #Local file
#' dirpng = system.file("img/LogoMartinique.png", package = "cartography")
#' 
#' mask <- getPngLayer(mtq, dirpng)
#' #Remote file
#' urlpng = "https://i.imgur.com/gePiDvB.png"
#' 
#' masksea <- getPngLayer(mtq, urlpng, mode = "wb", inverse = TRUE)
#' }
#' @export
getPngLayer <-  function(x, pngpath, align = "center", inverse = FALSE, dwmode = "curl", ...) {
  shp <- x
  crs = sf::st_crs(x)$proj4string
  if (file.exists(pngpath)) {
    pngRB = raster::brick(png::readPNG(pngpath) * 255, crs = crs)
  } else {
    # Download
    dirfile = tempfile(fileext = ".png")
    if (dwmode == "base") {
      download.file(pngpath, dirfile, ...)
    } else if (dwmode == "curl") {
      curl::curl_download(pngpath, dirfile, ...)
    }
    pngRB = raster::brick(png::readPNG(dirfile) * 255, crs = crs)
  }
  
  if (!align %in% c("left", "right", "center")) {
    stop("align should be 'left','right' or 'center'")
  }
  
  #Geotagging the raster
  #Now cover with the pngRB the whole extent of the shape
  ratiopngRB = dim(pngRB)[2] / dim(pngRB)[1]
  
  #Middle point
  extshp = raster::extent(shp)
  w = (extshp@xmax - extshp@xmin) / 2
  h = (extshp@ymax - extshp@ymin) / 2
  w_mp = extshp@xmin + w
  h_mp = extshp@ymin + h
  # Depending of the shape the fitting could be in height or width
  if (w > h * ratiopngRB) {
    new_ext = c(extshp@xmin,
                extshp@xmax,
                h_mp - w / ratiopngRB,
                h_mp + w / ratiopngRB)
  } else {
    if (align == "center") {
      new_ext = c(w_mp - h * ratiopngRB,
                  w_mp + h * ratiopngRB,
                  extshp@ymin,
                  extshp@ymax)
    }   else if (align == "left") {
      new_ext = c(extshp@xmin,
                  extshp@xmin + 2 * h * ratiopngRB,
                  extshp@ymin,
                  extshp@ymax)
    }   else {
      new_ext = c(extshp@xmax - 2 * h * ratiopngRB,
                  extshp@xmax ,
                  extshp@ymin,
                  extshp@ymax)
    }
    
  }
  raster::extent(pngRB) <- new_ext
  # Mask
  fig = raster::mask(pngRB, shp, inverse = inverse)
  fig
}

#' @importFrom utils download.file
NULL