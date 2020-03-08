#' @title Pretty format for labels
#' @description Convert numeric arrays into formatted strings.
#' @name getFormatNums
#' @param v a vector of numeric values.
#' @param thousands thousands separator.
#' @param decimals decimals separator.
#' @param values.rnd desired number of digits after the decimal separator.
#' @param prefix,suffix strings to be pasted before of after the number, see Details.
#' @param align alignment of the final string, possible values are "left" or "right".
#' @param leadzero logical, convert leading zeros to \code{" ."} for values in (-1,1).
#' @param replace.zero replace zero values for this parameter. See Details.
#' @details If \code{suffix} contains \code{"\%"} the values are converted to percentages. \cr\cr
#' \code{NA} values of \code{v} are converted to \code{"NA"}. \cr\cr
#' If \code{replace.zero} is not \code{NULL}, exact zeroes would be replaced for this value.
#' @return A character vector containing the formatted values.
#' @author dieghernan, \url{https://github.com/dieghernan/}
#' @examples
#' library(sf)
#' mtq <- st_read(system.file("gpkg/mtq.gpkg", package = "cartography"))
#' # Population density
#' mtq$POPDENS <- 1e6 * mtq$POP / st_area(x = mtq)
#' brks <- getBreaks(mtq$POPDENS, 6)
#' cols <- carto.pal("red.pal", 6)
#' labs <- getFormatNums(brks,
#'                      thousands = ",",
#'                      suffix = " in/km2",
#'                      align = "right")
#' 
#' choroLayer(mtq,
#'            var = "POPDENS",
#'            col = cols,
#'            legend.pos = "n")
#' legendChoro(
#'   breaks = labs,
#'   col = cols,
#'   title.txt = "Population Density",
#'   pos = "topright"
#' )
#' 
#' # Percentage of population
#' mtq$PORCPOP <- mtq$POP / sum(mtq$POP)
#' 
#' brks <- seq(0, .3, .05)
#' cols = carto.pal("sand.pal", 6)
#' labs <- getFormatNums(brks,
#'                      values.rnd = 2,
#'                      leadzero = FALSE,
#'                      replace.zero = "0000")
#' 
#' choroLayer(mtq,
#'            var = "PORCPOP",
#'            col = cols,
#'            legend.pos = "n")
#' 
#' legendChoro(
#'   breaks = labs,
#'   col = cols,
#'   title.txt = "Population Distr.",
#'   pos = "topright",
#'   nodata = FALSE
#' )
#' 
#' # Area percent
#' mtq$AREA <- (st_area(mtq) / sum(st_area(mtq)))
#' 
#' brks <- getBreaks(mtq$AREA, method = "q6")
#' cols = carto.pal("orange.pal", 6)
#' labs <- getFormatNums(brks, suffix = "%", values.rnd = 1, prefix = "<")
#' choroLayer(mtq,
#'            var = "AREA",
#'            legend.pos = "n",
#'            col = cols)
#' 
#' legendChoro(
#'   breaks = labs,
#'   col = cols,
#'   title.txt = "% Area",
#'   pos = "left",
#'   nodata = FALSE
#' )
#' @export
getFormatNums <- function(v,
                         thousands = "",
                         decimals = getOption("OutDec"),
                         values.rnd = 0,
                         prefix = "",
                         suffix = "",
                         align = "left",
                         leadzero = TRUE,
                         replace.zero = NULL) {
  v <- as.vector(v)
  if (is.character(v)) {
    return(v)
  }
  
  if (length(grep("%", suffix)) > 0) {
    v <- 100 * v
  }
  v <- round(v, values.rnd)
  
  labs <- format(
    x = v,
    big.mark = thousands,
    decimal.mark = decimals,
    scientific = FALSE,
    trim = TRUE
  )
  
  if (isFALSE(leadzero)) {
    totrail <- abs(v) < 1
    labs[totrail] <- sub(paste0("0", decimals),
                         paste0(" ", decimals),
                         labs[totrail])
  }
  
  labs <- paste0(prefix, labs, suffix)
  labs[is.na(v)] <- "NA"
  if (!is.null(replace.zero)) {
    labs[v == 0] <- replace.zero
  }
  if (align == "right") {
    labs <- format(labs, justify = "right")
  }
  return(labs)
}