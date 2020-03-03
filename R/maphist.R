#' @title Histogram on maps
#' @description Plot an histogram over a map.
#' @name maphist
#' @param v a vector of numeric values.
#' @param breaks,method,nclass classification options, see \link{getBreaks}.
#' @param col a vector of colors. 
#' @param pos position of the legend, one of "topleft", "top", 
#' "topright", "right", "bottomright", "bottom", "bottomleft", 
#' "bottomleftextra", "left" or a vector of two coordinates in map units 
#' (c(x, y)).
#' @param hist.width,hist.height width and height of the histogram, in normalized device coordinates (NDC). See Details. 
#' @param title.txt title of the legend.
#' @param title.cex size of the legend title.
#' @param axes whether to add a horizontal (\code{"h"}), vertical (\code{"v"}) or both (\code{TRUE}) axes.
#' @param axes.cex size of the values in the axes.
#' @author dieghernan, \url{https://github.com/dieghernan/}
#' @param frame whether to add a frame to the legend (\code{TRUE}) or not (\code{FALSE}).
#' @details \code{hist.width} and \code{hist.height} are defined in NDC, meaning that
#' the height and width are percentages of the overall dimensions of the device plot. Default parameters produce
#' a histogram that covers 25\% (on width) and 35\% (on height) of the overall device plot.
#' @seealso \link{getBreaks}, \link{choroLayer}, \link{legendChoro}.
#' @importFrom graphics grconvertX grconvertY hist axis
#' @examples 
#' library(sf)
#' mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"))
#' # Population density
#' mtq$POPDENS <- 1e6 * mtq$POP / st_area(x = mtq)
#' 
#' choroLayer(x = mtq, var = "POPDENS",
#'           border = "grey40",
#'           legend.pos = "topright", legend.values.rnd = 0,
#'           legend.title.txt = "Population Density\n(people per km2)")
#' maphist(v = mtq$POPDENS)
#' @export
maphist <- function(v,breaks = NULL, method = "quantile", nclass = NULL,
                    col = NULL, pos = "topleft", hist.width = .25, hist.height = .35,
                    title.txt=NULL, title.cex = 0.8,
                    axes = TRUE, axes.cex = 0.6, frame = FALSE) {
 
  v <- as.vector(na.omit(v))
  # get the colors and breaks
  layer <- choro(var = v, distr = breaks, col = col,
      nclass = nclass, method = method)
  
  # exit for none
  positions <- c("bottomleft", "topleft", "topright", "bottomright",
                 "left", "right", "top", "bottom", "center", 
                 "bottomleftextra")
  if(length(pos) == 1){if(!pos %in% positions){return(invisible())}}
  
  # figdim in geo coordinates
  x1 <- par()$usr[1]
  x2 <- par()$usr[2]
  y1 <- par()$usr[3]
  y2 <- par()$usr[4]
  
  # offsets
  delta1 <- xinch(0.01)
  delta2 <- xinch(0.01)
  
  # variables internes
  hist.width <- max(0, min(1, hist.width))
  hist.height <- max(0, min(1, hist.height))
  
  legend_xsize <- (x2 - x1) * hist.width
  legend_ysize <- (y2 - y1) * hist.height
  
  legcoord <- legpos(pos = pos, x1 = x1, x2 = x2, y1 = y1, y2 = y2,
                     delta1 = delta1, delta2 = delta2,
                     legend_xsize = legend_xsize,
                     legend_ysize = legend_ysize)
  
  xref <- legcoord$xref
  yref <- legcoord$yref
  
  # Frame
  if (frame == TRUE){
    rect(xref - delta1, yref - delta1, xref + legend_xsize + delta1 * 2,
         yref + legend_ysize + delta1 * 2, border = "black",  col="white")
  }
  
  #Guess the bins on hist
  hbreaks <- min(length(unique(v)), 50) #Max 50 bins
  bins <- pretty(v, n = hbreaks)
  intervs <- layer$distr
  intervs[c(1, length(intervs))] <- c(-Inf, Inf)
  
  # get the colors and breaks for bins
  hist <-  choro(var = bins, distr = intervs, col = col)
  
  #vscale
  vlabs <- table(cut(v, bins, labels = FALSE))
  vlabs <- as.integer(pretty(vlabs))
  
  #Store initial parasm
  opar <- par(no.readonly = TRUE)
  
  #Setting fig params
  histfig <- c(grconvertX(c(xref - delta1, xref + legend_xsize + delta1 * 2), "user", "ndc"),
              grconvertY(c(yref - delta1, yref + legend_ysize + delta1 * 2), "user", "ndc"))
  histfig <- pmin(0.99,pmax(0.01,histfig))
  
  #Control internal margins
  if (is.null(title.txt) || title.txt == "") {
    topinch <- 0.05
  } else {
    topinch <- strheight(title.txt, cex = title.cex, units = "inches") + 0.05
  }
  labsx <- max(strheight(as.character(bins),
                         cex = axes.cex, units = "inches"))
  if (isTRUE(axes) || axes == "h") {
    bottinc <- labsx +0.05
  } else {
    bottinc <- 0.05
  }
  labsy <- max(strwidth(as.character(vlabs),
                         cex = axes.cex, units = "inches"))
  
  if (isTRUE(axes) || axes == "v") {
    leftinc <- labsy + 0.05
  } else {
    leftinc <- 0.05
  }
  
  
  par(new = TRUE, mgp = c(0, 0, 0), 
      mai = c(bottinc, leftinc, topinch, 0.05), 
      fig = histfig, cex.axis = axes.cex, cex.main = title.cex)
  
  hist(x = v, breaks = hbreaks, col = hist$colMap,
    axes = FALSE, xlab = "", ylab = "",
    main = title.txt, border = hist$colMap)
  
  #Axes control
  if (isTRUE(axes) || axes == "h"){
    axis(1, 
         line = -.9+labsx/0.2, 
         col = NA, col.ticks = "grey20", lwd.ticks = 0)
    }
  if (isTRUE(axes) || axes == "v"){
    axis(2, at=vlabs[-1], 
         line = 0, tck = 1,
         las= 2, col = NA, col.ticks = "grey20",
         lwd.ticks = "1", lwd = 0.1, lty = 3)
    }
  par(opar)
  }
