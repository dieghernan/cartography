#' @title Get Tiles from Open Map Servers
#' @name getTiles
#' @description Get map tiles based on a spatial object extent. Maps can be 
#' fetched from various open map servers.
#' @param x an sf object, a simple feature collection or a Spatial*DataFrame.
#' @param spdf  deprecated, a Spatial*DataFrame with a valid projection attribute.
#' @param type the tile server from which to get the map. See Details for providers.
#' For other sources use a list: type = list(src = "name of the source" , 
#' q = "tiles address", sub = "subdomains", cit = "how to cite the tiles"). See Examples.
#' @param zoom the zoom level. If null, it is determined automatically 
#' (see Details).
#' @param crop TRUE if results should be cropped to the specified x extent, 
#' FALSE otherwise. If x is an sf object with one POINT, crop is set to FALSE. 
#' @param verbose if TRUE, tiles filepaths, zoom level and citation are displayed. 
#' @param apikey Needed for Thunderforest and Jawg maps.
#' @param cachedir name of a directory used to cache tiles. If TRUE, places a 
#' 'tile.cache' folder in the working directory. If FALSE, tiles are not cached.
#' @param forceDownload if TRUE, cached tiles are downloaded again. 
#' @details 
#' Zoom levels are described on the OpenStreetMap wiki: 
#' \url{https://wiki.openstreetmap.org/wiki/Zoom_levels}. \cr\cr
#' Full list of providers:  
#' \tabular{lll}{
#' 'OpenStreetMap' (or 'osm')                 \tab	'Stamen' (or 'stamenbw')	                  \tab  'Esri' \cr
#' 'OpenStreetMap.DE'                         \tab	'Stamen.Toner'                            	\tab  'Esri.WorldStreetMap'\cr
#' 'OpenStreetMap.France'                     \tab	'Stamen.TonerBackground'                    \tab  'Esri.DeLorme'\cr
#' 'OpenStreetMap.HOT' (or 'hotstyle')        \tab	'Stamen.TonerHybrid'                        \tab  'Esri.WorldTopoMap'\cr
#'                                            \tab  'Stamen.TonerLines'                         \tab  'Esri.WorldImagery'\cr
#' 'OpenMapSurfer'                            \tab  'Stamen.TonerLabels'                        \tab  'Esri.WorldTerrain'\cr
#' 'OpenMapSurfer.Roads'                      \tab  'Stamen.TonerLite'                          \tab  'Esri.WorldShadedRelief'\cr
#' 'OpenMapSurfer.Hybrid'                     \tab  'Stamen.Watercolor' (or 'stamenwatercolor') \tab  'Esri.OceanBasemap'\cr
#' 'OpenMapSurfer.AdminBounds'                \tab  'Stamen.Terrain'                            \tab  'Esri.NatGeoWorldMap'\cr
#' 'OpenMapSurfer.ElementsAtRisk'             \tab  'Stamen.TerrainBackground'                  \tab  'Esri.WorldGrayCanvas'\cr
#'                                            \tab  'Stamen.TerrainLabels'                      \tab \cr
#' 'CartoDB'                                  \tab                                              \tab  'Hydda'\cr
#' 'CartoDB.Positron' (or 'cartolight')       \tab  'Thunderforest'                             \tab  'Hydda.Full'\cr 
#' 'CartoDB.PositronNoLabels'                 \tab  'Thunderforest.OpenCycleMap'                \tab  'Hydda.Base'\cr
#' 'CartoDB.PositronOnlyLabels'               \tab  'Thunderforest.Transport'                   \tab  'Hydda.RoadsAndLabels'\cr
#' 'CartoDB.DarkMatter' (or 'cartodark')      \tab  'Thunderforest.TransportDark'               \tab \cr 
#' 'CartoDB.DarkMatterNoLabels'               \tab  'Thunderforest.SpinalMap'                   \tab  'HikeBike' (or 'hikebike')\cr
#' 'CartoDB.DarkMatterOnlyLabels'             \tab  'Thunderforest.Landscape'                   \tab  'HikeBike.HikeBike'\cr
#' 'CartoDB.Voyager'                          \tab  'Thunderforest.Outdoors'                    \tab \cr
#' 'CartoDB.VoyagerNoLabels'                  \tab  'Thunderforest.Pioneer'                     \tab  'OpenTopoMap' (or 'opentopomap') \cr
#' 'CartoDB.VoyagerOnlyLabels'                \tab  'Thunderforest.MobileAtlas'                 \tab  'Wikimedia'\cr
#' 'CartoDB.VoyagerLabelsUnder'               \tab  'Thunderforest.Neighbourhood'               \tab  'OpenStreetMap.MapnikBW' (or 'osmgrayscale')\cr
#' 'OpenStreetMap.CH'                         \tab 'Jawg.Streets'                               \tab 'Stadia.AlidadeSmooth'\cr
#' 'OpenStreetMap.BZH'                        \tab 'Jawg.Terrain'                               \tab 'BasemapAT.basemap'\cr
#' 'OpenSeaMap'                               \tab 'Jawg.Sunny'                                 \tab 'BasemapAT.grau'\cr
#' 'OpenPtMap'                                \tab 'Jawg.Dark'                                  \tab 'BasemapAT.overlay'\cr
#' 'OpenRailwayMap'                           \tab 'Jawg.Light'                                 \tab 'BasemapAT.terrain'\cr
#' 'USGS'                                     \tab 'Jawg.Matrix'                                \tab 'BasemapAT.surface'\cr
#' 'USGS.USTopo'                              \tab 'WaymarkedTrails.hiking'                     \tab 'BasemapAT.highdpi'\cr
#' 'USGS.USImagery'                           \tab 'WaymarkedTrails.cycling'                    \tab 'BasemapAT.orthofoto'\cr
#' 'USGS.USImageryTopo'                       \tab 'WaymarkedTrails.mtb'                        \tab 'nlmaps.standaard'\cr
#' 'Esri.WorldPhysical'                       \tab 'WaymarkedTrails.slopes'                     \tab 'nlmaps.pastel'\cr
#' 'FreeMapSK'                                \tab 'WaymarkedTrails.riding'                     \tab 'nlmaps.grijs'\cr
#' 'MtbMap'                                   \tab 'WaymarkedTrails.skating'                    \tab 'NLS'\cr
#' 'SafeCast'                                 \tab 'CyclOSM'                                    \tab \cr
#' HEREv3.normalDay'                          \tab 'HEREv3.normalNightGreyMobile'               \tab 'HEREv3.hybridDay'\cr
#' HEREv3.normalDayCustom'                    \tab 'HEREv3.normalNightTransit'                  \tab 'HEREv3.hybridDayMobile'\cr
#' HEREv3.normalDayGrey'                      \tab 'HEREv3.normalNightTransitMobile'            \tab 'HEREv3.hybridDayTransit'\cr
#' HEREv3.normalDayMobile'                    \tab 'HEREv3.reducedDay'                          \tab 'HEREv3.hybridDayGrey'\cr
#' HEREv3.normalDayGreyMobile'                \tab 'HEREv3.reducedNight'                        \tab 'HEREv3.pedestrianDay'\cr
#' HEREv3.normalDayTransit'                   \tab 'HEREv3.basicMap'                            \tab 'HEREv3.pedestrianNight'\cr
#' HEREv3.normalDayTransitMobile'             \tab 'HEREv3.mapLabels'                           \tab 'HEREv3.satelliteDay'\cr
#' HEREv3.normalNight'                        \tab 'HEREv3.trafficFlow'                         \tab 'HEREv3.terrainDay'\cr
#' HEREv3.normalNightMobile'                  \tab 'HEREv3.carnavDayGrey'                       \tab 'HEREv3.terrainDayMobile'\cr
#' HEREv3.normalNightGrey'                    \tab                                              \tab \cr
#' }
#' @references \url{https://leaflet-extras.github.io/leaflet-providers/preview/}
#' @export
#' @return A RasterBrick is returned.
#' @seealso \link{tilesLayer}
#' @examples
#' \dontrun{
#' library(sf)
#' mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"))
#' # Download the tiles, extent = Martinique
#' mtqOSM <- getTiles(x = mtq, type = "osm", crop = TRUE)
#' # Plot the tiles
#' tilesLayer(mtqOSM)
#' # Plot countries
#' plot(st_geometry(mtq), add=TRUE)
#' txt <- paste0("\u00A9 OpenStreetMap contributors.", 
#'               " Tiles style under CC BY-SA, www.openstreetmap.org/copyright")
#' mtext(text = txt, side = 1, adj = 0, cex = 0.7, font = 3)
#' 
#' # Download esri tiles
#' fullserver = paste("https://server.arcgisonline.com/ArcGIS/rest/services",
#'                    "Specialty/DeLorme_World_Base_Map/MapServer",
#'                    "tile/{z}/{y}/{x}.jpg",
#'                    sep = "/"
#'                    )
#' typeosm <-  list(
#'   src = 'esri',
#'   q = fullserver,
#'   sub = NA,
#'   cit = 'Tiles; Esri; Copyright: 2012 DeLorme'
#' )
#' mtqESRI <- getTiles(x = mtq, type = typeosm, crop = TRUE, verbose = T, zoom = 10)
#' # Plot the tiles
#' tilesLayer(mtqESRI)
#' txt <- typeosm$cit
#' mtext(text = txt, side = 1, adj = 0, cex = 0.6, font = 3)
#' }
getTiles <- function(x, spdf, type = "OpenStreetMap", zoom = NULL, crop = FALSE, 
                     verbose = FALSE, apikey=NA, cachedir=FALSE, forceDownload=FALSE){
  # deprecated check
  if(!missing(spdf)){
    warning("spdf is deprecated; use x instead.", call. = FALSE)
    x <- spdf
  }
  # test for sp
  if(methods::is(x,"Spatial") == TRUE){
    x <- convertToSf(spdf = x)
  }
  # test for single point (apply buffer to obtain a correct bbox)
  if(nrow(x)==1 && sf::st_is(x, "POINT")){
    xt <- sf::st_transform(x, 3857)
    sf::st_geometry(xt) <- sf::st_buffer(sf::st_geometry(xt), 1000)
    crop <- FALSE
    # use x bbox to select the tiles to get 
    bbx <- sf::st_bbox(sf::st_transform(sf::st_as_sfc(sf::st_bbox(xt)), 4326))
  }else{
    # use x bbox to select the tiles to get 
    bbx <- sf::st_bbox(sf::st_transform(sf::st_as_sfc(sf::st_bbox(x)), 4326))
  }
  # select a default zoom level
  if(is.null(zoom)){
    gz <- slippymath::bbox_tile_query(bbx)
    zoom <- min(gz[gz$total_tiles %in% 4:10,"zoom"])
  }
  
  # get tile list
  tile_grid <- slippymath::bbox_to_tile_grid(bbox = bbx, zoom = zoom)
  
  # get query parameters according to type 
  param <- get_param(type)
  # subdomains management
  tile_grid$tiles$s <- sample(param$sub, nrow(tile_grid$tiles), replace = T)
  # src mgmnt
  tile_grid$src <- param$src
  # query mgmnt
  tile_grid$q <- sub("XXXXXX",apikey,param$q)
  # citation
  tile_grid$cit <- param$cit
  
  # extension management 
  if (length(grep("jpg",param$q))>0){
    ext="jpg"
  } else if (length(grep("png",param$q))>0){
    ext="png"
  } else if (length(grep("jpeg",param$q))>0){
    ext="jpeg"
  }
  # Overwrite if declared
  if ("ext" %in% names(param)) {
    ext = param$ext
  }
  
  tile_grid$ext<-ext
  #tile_grid$ext <- substr(param$q, nchar(param$q)-2, nchar(param$q))
  
  # download images
  images <- get_tiles(tile_grid, verbose, cachedir, forceDownload)
  # compose images
  rout <- compose_tile_grid(tile_grid, images)
  
  
  # reproject rout
  rout <- raster::projectRaster(from = rout, crs = sf::st_crs(x)$proj4string)
  rout <- raster::clamp(rout,lower = 0, upper = 255, useValues = TRUE)
  
  # crop management  
  if(crop == TRUE){
    cb <- sf::st_bbox(x)
    k <- min(c(0.052 * (cb[4] - cb[2]), 0.052 * (cb[3] - cb[1])))
    cb <- cb + c(-k, -k, k, k)
    rout <- raster::crop(rout,cb[c(1,3,2,4)])
  }
  
  rout
}


# get the tiles according to the grid
get_tiles <- function(tile_grid, verbose, cachedir, forceDownload) {
  # go through tile_grid tiles and download
  images <- apply(
    X = tile_grid$tiles,
    MARGIN = 1, 
    FUN = dl_t,
    z = tile_grid$zoom,
    ext = tile_grid$ext,
    src = tile_grid$src,
    q = tile_grid$q,
    verbose = verbose,
    cachedir = cachedir,
    forceDownload = forceDownload
  )
  
  if (verbose) {
    message("Zoom:", tile_grid$zoom, "\nData and map tiles sources:\n",
            tile_grid$cit)
  }
  images
}

# download tile according to parameters
dl_t <- function(x, z, ext, src, q, verbose, cachedir, forceDownload) {
  # forceDownload will overwrite any files existing in cache
  if(!is.logical(forceDownload)) stop("forceDownload must be TRUE or FALSE")
  # if cachedir==F, save to temporary filepath
  if(cachedir == FALSE) {
    cachedir <- tempdir()
  } else { 
    # if cachedir==T, place in working directory
    if(cachedir == TRUE) cachedir <- paste0(getwd(),'/tile.cache')
    #create the cachedir if it doesn't exist.
    if(!dir.exists(cachedir)) dir.create(cachedir)
    # uses subdirectories based on src to make the directory easier for users to navigate
    subdir <- paste0(cachedir,"/",src)
    if(!dir.exists(subdir)) dir.create(subdir)
    cachedir <- subdir
  }
  
  outfile <- paste0(cachedir, "/", src, "_", z, "_", x[1], "_", x[2],".", ext)
  if (!file.exists(outfile) | isTRUE(forceDownload)) {
    q <- gsub(pattern = '{s}', replacement = x[3], x = q, fixed = TRUE)
    q <- gsub(pattern = '{x}', replacement = x[1], x = q, fixed = TRUE)
    q <- gsub(pattern = '{y}', replacement = x[2], x = q, fixed = TRUE)
    q <- gsub(pattern = '{z}', replacement = z, x = q, fixed = TRUE)
    if (verbose) {
      message(q, " => ", outfile)
    }
    curl::curl_download(url = q, destfile = outfile)
  }
  outfile
} 

# compose tiles 
compose_tile_grid <- function (tile_grid, images){
  bricks = vector("list", nrow(tile_grid$tiles))
  for (i in seq_along(bricks)){
    bbox <- slippymath::tile_bbox(tile_grid$tiles$x[i], tile_grid$tiles$y[i],
                                  tile_grid$zoom)
    img <- images[i]
    # special for png tiles
    if (tile_grid$ext=="png"){
      img <- png::readPNG(img)*255
      
      # Give transparency if available
      if (dim(img)[3] == 4) {
        nrow <- dim(img)[1]
        
        for (j in seq_len(nrow)) {
          row <- img[j, , ]
          alpha <- row[, 4] == 0
          row[alpha, ] <- NA
          img[j, , ] <- row
        }
      }
    }

    # compose brick raster
    r_img <- raster::brick(img, crs = sf::st_crs(3857)$proj4string)
    raster::extent(r_img) <- raster::extent(bbox[c("xmin", "xmax", 
                                                   "ymin", "ymax")])
    bricks[[i]] <- r_img
  }
  # if only one tile is needed
  if(length(bricks)==1){
    return(bricks[[1]])
  }
  # all tiles together
  rout <- do.call(raster::merge, bricks)
  rout
}

# providers parameters
get_param <- function(type) {
  if (length(type) == 4) {
    param <- type
  } else{
    param <- switch(
      type,
      osm              = get_param('OpenStreetMap'),
      hotstyle         = get_param('OpenStreetMap.HOT'),
      hikebike         = get_param('HikeBike'),
      osmgrayscale     = get_param('OpenStreetMap.MapnikBW'),
      stamenbw         = get_param('Stamen'),
      stamenwatercolor = get_param('Stamen.Watercolor'),
      cartodark        = get_param('CartoDB.DarkMatter'),
      cartolight       = get_param('CartoDB.Positron'),
      opentopomap      = get_param('OpenTopoMap'),
      OpenStreetMap.MapnikBW = list(
        src = "osmgrayscale",
        q = "https://tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png" ,
        sub = NA,
        cit = "\u00A9 OpenStreetMap contributors. Tiles style under CC BY-SA, www.openstreetmap.org/copyright."
      ),
      OpenStreetMap = list(
        src = "OpenStreetMap",
        q = 'http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png',
        sub = c("a", "b", "c"),
        cit = "\u00A9 OpenStreetMap contributors"
      ),
      OpenStreetMap.DE = list(
        src = "OpenStreetMap.DE",
        q = "https://{s}.tile.openstreetmap.de/tiles/osmde/{z}/{x}/{y}.png",
        sub = c("a", "b", "c"),
        cit = "\u00A9 OpenStreetMap contributors"
      ),
      OpenStreetMap.France = list(
        src = "OpenStreetMap.France",
        q = "https://{s}.tile.openstreetmap.fr/osmfr/{z}/{x}/{y}.png",
        sub = c("a", "b", "c"),
        cit = "\u00A9 Openstreetmap France | \u00A9 OpenStreetMap contributors"
      ),
      OpenStreetMap.HOT = list(
        src = "OpenStreetMap.HOT",
        q = "https://{s}.tile.openstreetmap.fr/hot/{z}/{x}/{y}.png",
        sub = c("a", "b", "c"),
        cit = "\u00A9 OpenStreetMap contributors, Tiles style by OpenStreetMap France"
      ),
      OpenTopoMap = list(
        src = "OpenTopoMap",
        q = "https://{s}.tile.opentopomap.org/{z}/{x}/{y}.png",
        sub = c("a", "b", "c"),
        cit = "Map data: \u00A9 OpenStreetMap contributors, OpenTopoMap (CC-BY-SA)"
      ),
      OpenMapSurfer = list(
        src = "OpenMapSurfer",
        q = "https://maps.heigit.org/openmapsurfer/tiles/roads/webmercator/{z}/{x}/{y}.png",
        sub = NA,
        cit = "Imagery from GIScience Research Group \uFE6B University of Heidelberg | Map data "
      ),
      OpenMapSurfer.Roads = list(
        src = "OpenMapSurfer.Roads",
        q = "https://maps.heigit.org/openmapsurfer/tiles/roads/webmercator/{z}/{x}/{y}.png",
        sub = NA,
        cit = "Imagery from GIScience Research Group \uFE6B University of Heidelberg | Map data \u00A9 OpenStreetMap contributors"
      ),
      OpenMapSurfer.Hybrid = list(
        src = "OpenMapSurfer.Hybrid",
        q = "https://maps.heigit.org/openmapsurfer/tiles/hybrid/webmercator/{z}/{x}/{y}.png",
        sub = NA,
        cit = "Imagery from GIScience Research Group \uFE6B University of Heidelberg | Map data \u00A9 OpenStreetMap contributors"
      ),
      OpenMapSurfer.AdminBounds = list(
        src = "OpenMapSurfer.AdminBounds",
        q = "https://maps.heigit.org/openmapsurfer/tiles/adminb/webmercator/{z}/{x}/{y}.png",
        sub = NA,
        cit = "Imagery from GIScience Research Group \uFE6B University of Heidelberg | Map data \u00A9 OpenStreetMap contributors"
      ),
      OpenMapSurfer.ElementsAtRisk = list(
        src = "OpenMapSurfer.ElementsAtRisk",
        q = "https://maps.heigit.org/openmapsurfer/tiles/elements_at_risk/webmercator/{z}/{x}/{y}.png",
        sub = NA,
        cit = "Imagery from GIScience Research Group \uFE6B University of Heidelberg | Map data \u00A9 OpenStreetMap contributors"
      ),
      Hydda = list(
        src = "Hydda",
        q = "https://{s}.tile.openstreetmap.se/hydda/full/{z}/{x}/{y}.png",
        sub = c("a", "b", "c"),
        cit = "Tiles courtesy of OpenStreetMap Sweden \u2014 Map data \u00A9 OpenStreetMap contributors"
      ),
      Hydda.Full = list(
        src = "Hydda.Full",
        q = "https://{s}.tile.openstreetmap.se/hydda/full/{z}/{x}/{y}.png",
        sub = c("a", "b", "c"),
        cit = "Tiles courtesy of OpenStreetMap Sweden \u2014 Map data \u00A9 OpenStreetMap contributors"
      ),
      Hydda.Base = list(
        src = "Hydda.Base",
        q = "https://{s}.tile.openstreetmap.se/hydda/base/{z}/{x}/{y}.png",
        sub = c("a", "b", "c"),
        cit = "Tiles courtesy of OpenStreetMap Sweden \u2014 Map data \u00A9 OpenStreetMap contributors"
      ),
      Hydda.RoadsAndLabels = list(
        src = "Hydda.RoadsAndLabels",
        q = "https://{s}.tile.openstreetmap.se/hydda/roads_and_labels/{z}/{x}/{y}.png",
        sub  = c("a", "b", "c"),
        cit = "Tiles courtesy of OpenStreetMap Sweden \u2014 Map data \u00A9 OpenStreetMap contributors"
      ),
      Stamen = list(
        src = "Stamen",
        q = "https://stamen-tiles-{s}.a.ssl.fastly.net/toner/{z}/{x}/{y}.png",
        sub = c("a", "b", "c", "d"),
        cit = "Map tiles by CC BY 3.0 \u2014 Map data \u00A9 OpenStreetMap contributors"
      ),
      Stamen.Toner = list(
        src = "Stamen.Toner",
        q = "https://stamen-tiles-{s}.a.ssl.fastly.net/toner/{z}/{x}/{y}.png",
        sub = c("a", "b", "c", "d"),
        cit = "Map tiles by CC BY 3.0 \u2014 Map data \u00A9 OpenStreetMap contributors"
      ),
      Stamen.TonerBackground = list(
        src = "Stamen.TonerBackground",
        q = "https://stamen-tiles-{s}.a.ssl.fastly.net/toner-background/{z}/{x}/{y}.png",
        sub = c("a", "b", "c", "d"),
        cit = "Map tiles by CC BY 3.0 \u2014 Map data \u00A9 OpenStreetMap contributors"
      ),
      Stamen.TonerHybrid = list(
        src = "Stamen.TonerHybrid",
        q = "https://stamen-tiles-{s}.a.ssl.fastly.net/toner-hybrid/{z}/{x}/{y}.png",
        sub = c("a", "b", "c", "d"),
        cit = "Map tiles by CC BY 3.0 \u2014 Map data \u00A9 OpenStreetMap contributors"
      ),
      Stamen.TonerLines = list(
        src = "Stamen.TonerLines",
        q = "https://stamen-tiles-{s}.a.ssl.fastly.net/toner-lines/{z}/{x}/{y}.png",
        sub = c("a", "b", "c", "d"),
        cit = "Map tiles by CC BY 3.0 \u2014 Map data \u00A9 OpenStreetMap contributors"
      ),
      Stamen.TonerLabels = list(
        src = "Stamen.TonerLabels",
        q = "https://stamen-tiles-{s}.a.ssl.fastly.net/toner-labels/{z}/{x}/{y}.png",
        sub = c("a", "b", "c", "d"),
        cit = "Map tiles by CC BY 3.0 \u2014 Map data \u00A9 OpenStreetMap contributors"
      ),
      Stamen.TonerLite = list(
        src = "Stamen.TonerLite",
        q = "https://stamen-tiles-{s}.a.ssl.fastly.net/toner-lite/{z}/{x}/{y}.png",
        sub = c("a", "b", "c", "d"),
        cit = "Map tiles by CC BY 3.0 \u2014 Map data \u00A9 OpenStreetMap contributors"
      ),
      Stamen.Watercolor = list(
        src = "Stamen.Watercolor",
        q = "https://stamen-tiles-{s}.a.ssl.fastly.net/watercolor/{z}/{x}/{y}.jpg",
        sub = c("a", "b", "c", "d"),
        cit = "Map tiles by CC BY 3.0 \u2014 Map data \u00A9 OpenStreetMap contributors"
      ),
      Stamen.Terrain = list(
        src = "Stamen.Terrain",
        q = "https://stamen-tiles-{s}.a.ssl.fastly.net/terrain/{z}/{x}/{y}.png",
        sub = c("a", "b", "c", "d"),
        cit = "Map tiles by CC BY 3.0 \u2014 Map data \u00A9 OpenStreetMap contributors"
      ),
      Stamen.TerrainBackground = list(
        src = "Stamen.TerrainBK",
        q = "https://stamen-tiles-{s}.a.ssl.fastly.net/terrain-background/{z}/{x}/{y}.png",
        sub = c("a", "b", "c", "d"),
        cit = "Map tiles by CC BY 3.0 \u2014 Map data \u00A9 OpenStreetMap contributors"
      ),
      Stamen.TerrainLabels = list(
        src = "Stamen.Terrainlabs",
        q = "https://stamen-tiles-{s}.a.ssl.fastly.net/terrain-labels/{z}/{x}/{y}.png",
        sub = c("a", "b", "c", "d"),
        cit = "Map tiles by CC BY 3.0 \u2014 Map data \u00A9 OpenStreetMap contributors"
      ),
      Esri = list(
        src = "Esri",
        q = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Street_Map/MapServer/tile/{z}/{y}/{x}.jpg",
        sub = NA,
        cit = "Tiles \u00A9 Esri"
      ),
      Esri.WorldStreetMap = list(
        src = "EsriWSM",
        q = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Street_Map/MapServer/tile/{z}/{y}/{x}.jpg",
        sub = NA,
        cit = "Tiles \u00A9 Esri \u2014 Source: Esri, DeLorme, NAVTEQ, USGS, Intermap, iPC, NRCAN, Esri Japan, METI, Esri China (Hong Kong), Esri (Thailand), TomTom, 2012"
      ),
      Esri.DeLorme = list(
        src = "EsriDLor",
        q = "https://server.arcgisonline.com/ArcGIS/rest/services/Specialty/DeLorme_World_Base_Map/MapServer/tile/{z}/{y}/{x}.jpg",
        sub = NA,
        cit = "Tiles \u00A9 Esri \u2014 Copyright: \u00A92012 DeLorme"
      ),
      Esri.WorldTopoMap = list(
        src = "EsriWTM",
        q = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}.jpg",
        sub = NA,
        cit = "Tiles \u00A9 Esri \u2014 Esri, DeLorme, NAVTEQ, TomTom, Intermap, iPC, USGS, FAO, NPS, NRCAN, GeoBase, Kadaster NL, Ordnance Survey, Esri Japan, METI, Esri China (Hong Kong), and the GIS User Community"
      ),
      Esri.WorldImagery = list(
        src = "EsriWI",
        q = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}.jpg",
        sub = NA,
        cit = "Tiles \u00A9 Esri \u2014 Source: Esri, i-cubed, USDA, USGS, AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP, and the GIS User Community"
      ),
      Esri.WorldTerrain = list(
        src = "EsriWT",
        q = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Terrain_Base/MapServer/tile/{z}/{y}/{x}.jpg",
        sub = NA,
        cit = "Tiles \u00A9 Esri \u2014 Source: USGS, Esri, TANA, DeLorme, and NPS"
      ),
      Esri.WorldShadedRelief = list(
        src = "EsriWSR",
        q = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Shaded_Relief/MapServer/tile/{z}/{y}/{x}.jpg",
        sub = NA,
        cit = "Tiles \u00A9 Esri \u2014 Source: Esri"
      ),
      Esri.OceanBasemap = list(
        src = "EsriOBM",
        q = "https://server.arcgisonline.com/ArcGIS/rest/services/Ocean_Basemap/MapServer/tile/{z}/{y}/{x}.jpg",
        sub = NA,
        cit = "Tiles \u00A9 Esri \u2014 Sources: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri"
      ),
      Esri.NatGeoWorldMap = list(
        src = "EsriNGW",
        q = "https://server.arcgisonline.com/ArcGIS/rest/services/NatGeo_World_Map/MapServer/tile/{z}/{y}/{x}.jpg",
        sub = NA,
        cit = "Tiles \u00A9 Esri \u2014 National Geographic, Esri, DeLorme, NAVTEQ, UNEP-WCMC, USGS, NASA, ESA, METI, NRCAN, GEBCO, NOAA, iPC"
      ),
      Esri.WorldGrayCanvas = list(
        src = "EsriWGC",
        q = "https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}.jpg",
        sub = NA,
        cit = "Tiles \u00A9 Esri \u2014 Esri, DeLorme, NAVTEQ"
      ),
      CartoDB = list(
        src = "Carto",
        q = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png",
        sub = c("a", "b", "c", "d"),
        cit = "\u00A9 OpenStreetMap contributors \u00A9 CARTO"
      ),
      CartoDB.Positron = list(
        src = "CartoP",
        q = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png",
        sub = c("a", "b", "c", "d"),
        cit = "\u00A9 OpenStreetMap contributors \u00A9 CARTO"
      ),
      CartoDB.PositronNoLabels = list(
        src = "CartoPNL",
        q = "https://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png",
        sub = c("a", "b", "c", "d"),
        cit = "\u00A9 OpenStreetMap contributors \u00A9 CARTO"
      ),
      CartoDB.PositronOnlyLabels = list(
        src = "CartoPOL",
        q = "https://{s}.basemaps.cartocdn.com/light_only_labels/{z}/{x}/{y}.png",
        sub = c("a", "b", "c", "d"),
        cit = "\u00A9 OpenStreetMap contributors \u00A9 CARTO"
      ),
      CartoDB.DarkMatter = list(
        src = "CartoDM",
        q = "https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png",
        sub = c("a", "b", "c", "d"),
        cit = "\u00A9 OpenStreetMap contributors \u00A9 CARTO"
      ),
      CartoDB.DarkMatterNoLabels = list(
        src = "CartoDMNL",
        q = "https://{s}.basemaps.cartocdn.com/dark_nolabels/{z}/{x}/{y}.png",
        sub = c("a", "b", "c", "d"),
        cit = "\u00A9 OpenStreetMap contributors \u00A9 CARTO"
      ),
      CartoDB.DarkMatterOnlyLabels = list(
        src = "CartoDMOL",
        q = "https://{s}.basemaps.cartocdn.com/dark_only_labels/{z}/{x}/{y}.png",
        sub = c("a", "b", "c", "d"),
        cit = "\u00A9 OpenStreetMap contributors \u00A9 CARTO"
      ),
      CartoDB.Voyager = list(
        src = "CartoV",
        q = "https://{s}.basemaps.cartocdn.com/rastertiles/voyager/{z}/{x}/{y}.png",
        sub = c("a", "b", "c", "d"),
        cit = "\u00A9 OpenStreetMap contributors \u00A9 CARTO"
      ),
      CartoDB.VoyagerNoLabels = list(
        src = "CartoVNL",
        q = "https://{s}.basemaps.cartocdn.com/rastertiles/voyager_nolabels/{z}/{x}/{y}.png",
        sub = c("a", "b", "c", "d"),
        cit = "\u00A9 OpenStreetMap contributors \u00A9 CARTO"
      ),
      CartoDB.VoyagerOnlyLabels = list(
        src = "CartoVOL",
        q = "https://{s}.basemaps.cartocdn.com/rastertiles/voyager_only_labels/{z}/{x}/{y}.png",
        sub = c("a", "b", "c", "d"),
        cit = "\u00A9 OpenStreetMap contributors \u00A9 CARTO"
      ),
      CartoDB.VoyagerLabelsUnder = list(
        src = "CartoVLU",
        q = "https://{s}.basemaps.cartocdn.com/rastertiles/voyager_labels_under/{z}/{x}/{y}.png",
        sub = c("a", "b", "c", "d"),
        cit = "\u00A9 OpenStreetMap contributors \u00A9 CARTO"
      ),
      HikeBike = list(
        src = "HikeBike",
        q = "https://tiles.wmflabs.org/hikebike/{z}/{x}/{y}.png",
        sub = NA,
        cit = "\u00A9 OpenStreetMap contributors"
      ),
      HikeBike.HikeBike = list(
        src = "HikeBike2",
        q = "https://tiles.wmflabs.org/hikebike/{z}/{x}/{y}.png",
        sub = NA,
        cit = "\u00A9 OpenStreetMap contributors"
      ),
      Wikimedia = list(
        src = "Wikimedia",
        q = "https://maps.wikimedia.org/osm-intl/{z}/{x}/{y}.png",
        sub = NA,
        cit = "Wikimedia"
      ),
      Thunderforest = list(
        src = "Tf",
        q = "https://tile.thunderforest.com/cycle/{z}/{x}/{y}.png?apikey=XXXXXX",
        sub = NA,
        cit = "Maps \u00A9 www.thunderforest.com, Data \u00A9 www.osm.org/copyright"
      ),
      Thunderforest.OpenCycleMap = list(
        src = "Tf",
        q = "https://tile.thunderforest.com/cycle/{z}/{x}/{y}.png?apikey=XXXXXX",
        sub = NA,
        cit = "Maps \u00A9 www.thunderforest.com, Data \u00A9 www.osm.org/copyright"
      ),
      Thunderforest.Transport = list(
        src = "Tf.Tr",
        q = "https://tile.thunderforest.com/transport/{z}/{x}/{y}.png?apikey=XXXXXX",
        sub = NA,
        cit = "Maps \u00A9 www.thunderforest.com, Data \u00A9 www.osm.org/copyright"
      ),
      Thunderforest.TransportDark = list(
        src = "Tf.TrDr",
        q = "https://tile.thunderforest.com/transport-dark/{z}/{x}/{y}.png?apikey=XXXXXX",
        sub = NA,
        cit = "Maps \u00A9 www.thunderforest.com, Data \u00A9 www.osm.org/copyright"
      ),
      Thunderforest.SpinalMap = list(
        src = "Tf.SP",
        q = "https://tile.thunderforest.com/spinal-map/{z}/{x}/{y}.png?apikey=XXXXXX",
        sub = NA,
        cit = "Maps \u00A9 www.thunderforest.com, Data \u00A9 www.osm.org/copyright"
      ),
      Thunderforest.Landscape = list(
        src = "Tf.Lc",
        q = "https://tile.thunderforest.com/landscape/{z}/{x}/{y}.png?apikey=XXXXXX",
        sub = NA,
        cit = "Maps \u00A9 www.thunderforest.com, Data \u00A9 www.osm.org/copyright"
      ),
      Thunderforest.Outdoors= list(
        src = "Tf.Out",
        q = "https://tile.thunderforest.com/outdoors/{z}/{x}/{y}.png?apikey=XXXXXX",
        sub = NA,
        cit = "Maps \u00A9 www.thunderforest.com, Data \u00A9 www.osm.org/copyright"
      ),
      Thunderforest.Pioneer = list(
        src = "Tf.Pion",
        q = "https://tile.thunderforest.com/pioneer/{z}/{x}/{y}.png?apikey=XXXXXX",
        sub = NA,
        cit = "Maps \u00A9 www.thunderforest.com, Data \u00A9 www.osm.org/copyright"
      ),
      Thunderforest.MobileAtlas= list(
        src = "Tf.MB",
        q = "https://tile.thunderforest.com/mobile-atlas/{z}/{x}/{y}.png?apikey=XXXXXX",
        sub = NA,
        cit = "Maps \u00A9 www.thunderforest.com, Data \u00A9 www.osm.org/copyright"
      ),
      Thunderforest.Neighbourhood= list(
        src = "Tf.Nbg",
        q = "https://tile.thunderforest.com/neighbourhood/{z}/{x}/{y}.png?apikey=XXXXXX",
        sub = NA,
        cit = "Maps \u00A9 www.thunderforest.com, Data \u00A9 www.osm.org/copyright"
      ),
      # New providers
      OpenStreetMap.CH = list(
        src = "OpenStreetMap.CH",
        q = "https://tile.osm.ch/switzerland/{z}/{x}/{y}.png",
        sub = NA,
        cit = "\u00A9 OpenStreetMap contributors"
      ),
      OpenStreetMap.BZH = list(
        src = "OpenStreetMap.BZH",
        q = "https://tile.openstreetmap.bzh/br/{z}/{x}/{y}.png",
        sub = NA,
        cit = "\u00A9 OpenStreetMap, Tiles courtesy of Breton OpenStreetMap Team"
      ),
      OpenSeaMap = list(
        src = "OpenSeaMap",
        q = "https://tiles.openseamap.org/seamark/{z}/{x}/{y}.png",
        sub = NA,
        cit = "Map data: \u00A9 OpenSeaMap contributors"
      ),
      OpenPtMap = list(
        src = "OpenPtMap",
        q = "http://openptmap.org/tiles/{z}/{x}/{y}.png",
        sub = NA,
        cit = "Map data: \u00A9 OpenPtMap contributors"
      ),
      OpenRailwayMap =  list(
        src = "OpenRailwayMap",
        sub = c("a", "b", "c"),
        q = "https://{s}.tiles.openrailwaymap.org/standard/{z}/{x}/{y}.png",
        cit = "Map data: \u00A9 OpenStreetMap contributors | Map style: \u00A9 OpenRailwayMap (CC-BY-SA)"
      ),
      SafeCast =  list(
        src = "SafeCast",
        sub = NA,
        q = "https://s3.amazonaws.com/te512.safecast.org/{z}/{x}/{y}.png",
        cit = "Map data: \u00A9 OpenStreetMap contributors | Map style: \u00A9 SafeCast (CC-BY-SA)"
      ),
      Stadia.AlidadeSmooth =  list(
        src = "Stadia.AlidadeSmooth",
        sub = NA,
        q = "https://tiles.stadiamaps.com/tiles/alidade_smooth/{z}/{x}/{y}.png",
        cit = "\u00A9 Stadia Maps, \u00A9 OpenMapTiles \u00A9 OpenStreetMap contributors"
      ),
      CyclOSM =  list(
        src = "CyclOSM",
        sub = c("a", "b", "c"),
        q = "https://{s}.tile-cyclosm.openstreetmap.fr/cyclosm/{z}/{x}/{y}.png",
        cit = "CyclOSM | Map data: \u00A9 OpenStreetMap contributors"
      ),
      
      Jawg.Streets =  list(
        src = "Jawg.Streets",
        sub = c("a", "b", "c", "d"),
        q = "https://{s}.tile.jawg.io/jawg-streets/{z}/{x}/{y}.png?access-token=XXXXXX",
        cit = "\u00A9 JawgMaps \u00A9 OpenStreetMap contributors"
      ),
      Jawg.Terrain =  list(
        src = "Jawg.Terrain",
        sub = c("a", "b", "c", "d"),
        q = "https://{s}.tile.jawg.io/jawg-terrain/{z}/{x}/{y}.png?access-token=XXXXXX",
        cit = "\u00A9 JawgMaps \u00A9 OpenStreetMap contributors"
      ),
      Jawg.Sunny =  list(
        src = "Jawg.Sunny",
        sub = c("a", "b", "c", "d"),
        q = "https://{s}.tile.jawg.io/jawg-sunny/{z}/{x}/{y}.png?access-token=XXXXXX",
        cit = "\u00A9 JawgMaps \u00A9 OpenStreetMap contributors"
      ),
      Jawg.Dark =  list(
        src = "Jawg.Dark",
        sub = c("a", "b", "c", "d"),
        q = "https://{s}.tile.jawg.io/jawg-dark/{z}/{x}/{y}.png?access-token=XXXXXX",
        cit = "\u00A9 JawgMaps \u00A9 OpenStreetMap contributors"
      ),
      Jawg.Light =  list(
        src = "Jawg.Light",
        sub = c("a", "b", "c", "d"),
        q = "https://{s}.tile.jawg.io/jawg-light/{z}/{x}/{y}.png?access-token=XXXXXX",
        cit = "\u00A9 JawgMaps \u00A9 OpenStreetMap contributors"
      ),
      Jawg.Matrix =  list(
        src = "Jawg.Matrix",
        sub = c("a", "b", "c", "d"),
        q = "https://{s}.tile.jawg.io/jawg-matrix/{z}/{x}/{y}.png?access-token=XXXXXX",
        cit = "\u00A9 JawgMaps \u00A9 OpenStreetMap contributors"
      ),
      Esri.WorldPhysical =  list(
        src = "Esri.WorldPhysical",
        sub = NA,
        q = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Physical_Map/MapServer/tile/{z}/{y}/{x}",
        cit = "Tiles \u00A9 Esri \u2014 Source: US National Park Service",
        ext = "jpeg"
      ),
      FreeMapSK =  list(
        src = "FreeMapSK",
        sub = c("a", "b", "c", "d"),
        q = "https://{s}.freemap.sk/T/{z}/{x}/{y}.jpeg",
        cit = "\u00A9 OpenStreetMap contributors, vizualization CC-By-SA 2.0 Freemap.sk",
        ext = "jpeg"
      ),
      MtbMap =  list(
        src = "MtbMap",
        sub = NA,
        q = "http://tile.mtbmap.cz/mtbmap_tiles/{z}/{x}/{y}.png",
        cit = "\u00A9 OpenStreetMap contributors & USGS"
      ),
      BasemapAT.basemap =  list(
        src = "BasemapAT.basemap",
        sub = c("", "1", "2", "3", "4"),
        q = "https://maps{s}.wien.gv.at/basemap/geolandbasemap/normal/google3857/{z}/{y}/{x}.png",
        cit = "Datenquelle: basemap.at"
      ),
      
      BasemapAT.grau =  list(
        src = "BasemapAT.grau",
        sub = c("", "1", "2", "3", "4"),
        q = "https://maps{s}.wien.gv.at/basemap/bmapgrau/normal/google3857/{z}/{y}/{x}.png",
        cit = "Datenquelle: basemap.at"
      ),
      BasemapAT.overlay =  list(
        src = "BasemapAT.overlay",
        sub = c("", "1", "2", "3", "4"),
        q = "https://maps{s}.wien.gv.at/basemap/bmapoverlay/normal/google3857/{z}/{y}/{x}.png",
        cit = "Datenquelle: basemap.at"
      ),
      BasemapAT.terrain =  list(
        src = "BasemapAT.terrain",
        sub = c("", "1", "2", "3", "4"),
        q = "https://maps{s}.wien.gv.at/basemap/bmapgelaende/grau/google3857/{z}/{y}/{x}.jpeg",
        cit = "Datenquelle: basemap.at",
        ext = "jpeg"
      ),
      BasemapAT.surface =  list(
        src = "BasemapAT.surface",
        sub = c("", "1", "2", "3", "4"),
        q = "https://maps{s}.wien.gv.at/basemap/bmapoberflaeche/grau/google3857/{z}/{y}/{x}.jpeg",
        cit = "Datenquelle: basemap.at",
        ext = "jpeg"
      ),
      BasemapAT.highdpi =  list(
        src = "BasemapAT.highdpi",
        sub = c("", "1", "2", "3", "4"),
        q = "https://maps{s}.wien.gv.at/basemap/bmaphidpi/normal/google3857/{z}/{y}/{x}.jpeg",
        cit = "Datenquelle: basemap.at",
        ext = "jpeg"
      ),
      BasemapAT.orthofoto =  list(
        src = "BasemapAT.orthofoto",
        sub = c("", "1", "2", "3", "4"),
        q = "https://maps{s}.wien.gv.at/basemap/bmaporthofoto30cm/normal/google3857/{z}/{y}/{x}.jpeg",
        cit = "Datenquelle: basemap.at",
        ext = "jpeg"
      ),
      nlmaps.standaard =  list(
        src = "nlmaps.standaard",
        sub = NA,
        q = "https://geodata.nationaalgeoregister.nl/tiles/service/wmts/brtachtergrondkaart/EPSG:3857/{z}/{x}/{y}.png",
        cit = "Kaartgegevens \u00A9 Kadaster"
      ),
      nlmaps.pastel =  list(
        src = "nlmaps.pastel",
        sub = NA,
        q = "https://geodata.nationaalgeoregister.nl/tiles/service/wmts/brtachtergrondkaartpastel/EPSG:3857/{z}/{x}/{y}.png",
        cit = "Kaartgegevens \u00A9 Kadaster"
      ),
      nlmaps.grijs =  list(
        src = "nlmaps.grijs",
        sub = NA,
        q = "https://geodata.nationaalgeoregister.nl/tiles/service/wmts/brtachtergrondkaartgrijs/EPSG:3857/{z}/{x}/{y}.png",
        cit = "Kaartgegevens \u00A9 Kadaster"
      ),
      NLS =  list(
        src = "NLS",
        sub = c("0", "1", "2", "3"),
        q = "https://nls-{s}.tileserver.com/nls/{z}/{x}/{y}.jpg",
        cit = "National Library of Scotland Historic Maps"
      ),
      USGS =  list(
        src = "USGS",
        sub = NA,
        q = "https://basemap.nationalmap.gov/arcgis/rest/services/USGSTopo/MapServer/tile/{z}/{y}/{x}",
        cit = "Tiles courtesy of the U.S. Geological Survey",
        ext = "jpeg"
      ),
      USGS.USTopo =  list(
        src = "USGS.USTopo",
        sub = NA,
        q = "https://basemap.nationalmap.gov/arcgis/rest/services/USGSTopo/MapServer/tile/{z}/{y}/{x}",
        cit = "Tiles courtesy of the U.S. Geological Survey",
        ext = "jpeg"
      ),
      USGS.USImagery =  list(
        src = "USGS.USImagery",
        sub = NA,
        q = "https://basemap.nationalmap.gov/arcgis/rest/services/USGSImageryOnly/MapServer/tile/{z}/{y}/{x}",
        cit = "Tiles courtesy of the U.S. Geological Survey",
        ext = "jpeg"
      ),
      USGS.USImageryTopo =  list(
        src = "USGS.USImageryTopo",
        sub = NA,
        q = "https://basemap.nationalmap.gov/arcgis/rest/services/USGSImageryTopo/MapServer/tile/{z}/{y}/{x}",
        cit = "Tiles courtesy of the U.S. Geological Survey",
        ext = "jpeg"
      ),
      WaymarkedTrails.hiking =  list(
        src = "WaymarkedTrails.hiking",
        sub = NA,
        q = "https://tile.waymarkedtrails.org/hiking/{z}/{x}/{y}.png",
        cit = "Map data: \u00A9 OpenStreetMap contributors | Map style: \u00A9 waymarkedtrails.org (CC-BY-SA)"
      ),
      WaymarkedTrails.cycling =  list(
        src = "WaymarkedTrails.cycling",
        sub = NA,
        q = "https://tile.waymarkedtrails.org/cycling/{z}/{x}/{y}.png",
        cit = "Map data: \u00A9 OpenStreetMap contributors | Map style: \u00A9 waymarkedtrails.org (CC-BY-SA)"
      ),
      WaymarkedTrails.mtb =  list(
        src = "WaymarkedTrails.mtb",
        sub = NA,
        q = "https://tile.waymarkedtrails.org/mtb/{z}/{x}/{y}.png",
        cit = "Map data: \u00A9 OpenStreetMap contributors | Map style: \u00A9 waymarkedtrails.org (CC-BY-SA)"
      ),
      WaymarkedTrails.slopes =  list(
        src = "WaymarkedTrails.slopes",
        sub = NA,
        q = "https://tile.waymarkedtrails.org/slopes/{z}/{x}/{y}.png",
        cit = "Map data: \u00A9 OpenStreetMap contributors | Map style: \u00A9 waymarkedtrails.org (CC-BY-SA)"
      ),
      WaymarkedTrails.riding =  list(
        src = "WaymarkedTrails.riding",
        sub = NA,
        q = "https://tile.waymarkedtrails.org/riding/{z}/{x}/{y}.png",
        cit = "Map data: \u00A9 OpenStreetMap contributors | Map style: \u00A9 waymarkedtrails.org (CC-BY-SA)"
      ),
      WaymarkedTrails.skating =  list(
        src = "WaymarkedTrails.skating",
        sub = NA,
        q = "https://tile.waymarkedtrails.org/skating/{z}/{x}/{y}.png",
        cit = "Map data: \u00A9 OpenStreetMap contributors | Map style: \u00A9 waymarkedtrails.org (CC-BY-SA)"
      ),
      HEREv3.normalDay =  list(
        src = "HEREv3.normalDay",
        sub = c("1", "2", "3", "4"),
        q = "https://{s}.base.maps.ls.hereapi.com/maptile/2.1/maptile/newest/normal.day/{z}/{x}/{y}/256/png8?apiKey=XXXXXX&lg=eng",
        
        cit = "Map \u00A9 1987-2020 HERE"
      ),
      HEREv3.normalDayCustom =  list(
        src = "HEREv3.normalDayCustom",
        sub = c("1", "2", "3", "4"),
        q = "https://{s}.base.maps.ls.hereapi.com/maptile/2.1/maptile/newest/normal.day.custom/{z}/{x}/{y}/256/png8?apiKey=XXXXXX&lg=eng",
        cit = "Map \u00A9 1987-2020 HERE"
      ),
      HEREv3.normalDayGrey =  list(
        src = "HEREv3.normalDayGrey",
        sub = c("1", "2", "3", "4"),
        q = "https://{s}.base.maps.ls.hereapi.com/maptile/2.1/maptile/newest/normal.day.grey/{z}/{x}/{y}/256/png8?apiKey=XXXXXX&lg=eng",
        cit = "Map \u00A9 1987-2020 HERE"
      ),
      HEREv3.normalDayMobile =  list(
        src = "HEREv3.normalDayMobile",
        sub = c("1", "2", "3", "4"),
        q = "https://{s}.base.maps.ls.hereapi.com/maptile/2.1/maptile/newest/normal.day.mobile/{z}/{x}/{y}/256/png8?apiKey=XXXXXX&lg=eng",
        cit = "Map \u00A9 1987-2020 HERE"
      ),
      HEREv3.normalDayGreyMobile =  list(
        src = "HEREv3.normalDayGreyMobile",
        sub = c("1", "2", "3", "4"),
        q = "https://{s}.base.maps.ls.hereapi.com/maptile/2.1/maptile/newest/normal.day.grey.mobile/{z}/{x}/{y}/256/png8?apiKey=XXXXXX&lg=eng",
        cit = "Map \u00A9 1987-2020 HERE"
      ),
      HEREv3.normalDayTransit =  list(
        src = "HEREv3.normalDayTransit",
        sub = c("1", "2", "3", "4"),
        q = "https://{s}.base.maps.ls.hereapi.com/maptile/2.1/maptile/newest/normal.day.transit/{z}/{x}/{y}/256/png8?apiKey=XXXXXX&lg=eng",
        cit = "Map \u00A9 1987-2020 HERE"
      ),
      HEREv3.normalDayTransitMobile =  list(
        src = "HEREv3.normalDayTransitMobile",
        sub = c("1", "2", "3", "4"),
        q = "https://{s}.base.maps.ls.hereapi.com/maptile/2.1/maptile/newest/normal.day.transit.mobile/{z}/{x}/{y}/256/png8?apiKey=XXXXXX&lg=eng",
        cit = "Map \u00A9 1987-2020 HERE"
      ),
      HEREv3.normalNight =  list(
        src = "HEREv3.normalNight",
        sub = c("1", "2", "3", "4"),
        q = "https://{s}.base.maps.ls.hereapi.com/maptile/2.1/maptile/newest/normal.night/{z}/{x}/{y}/256/png8?apiKey=XXXXXX&lg=eng",
        cit = "Map \u00A9 1987-2020 HERE"
      ),
      HEREv3.normalNightMobile =  list(
        src = "HEREv3.normalNightMobile",
        sub = c("1", "2", "3", "4"),
        q = "https://{s}.base.maps.ls.hereapi.com/maptile/2.1/maptile/newest/normal.night.mobile/{z}/{x}/{y}/256/png8?apiKey=XXXXXX&lg=eng",
        cit = "Map \u00A9 1987-2020 HERE"
      ),
      HEREv3.normalNightGrey =  list(
        src = "HEREv3.normalNightGrey",
        sub = c("1", "2", "3", "4"),
        q = "https://{s}.base.maps.ls.hereapi.com/maptile/2.1/maptile/newest/normal.night.grey/{z}/{x}/{y}/256/png8?apiKey=XXXXXX&lg=eng",
        cit = "Map \u00A9 1987-2020 HERE"
      ),
      HEREv3.normalNightGreyMobile =  list(
        src = "HEREv3.normalNightGreyMobile",
        sub = c("1", "2", "3", "4"),
        q = "https://{s}.base.maps.ls.hereapi.com/maptile/2.1/maptile/newest/normal.night.grey.mobile/{z}/{x}/{y}/256/png8?apiKey=XXXXXX&lg=eng",
        cit = "Map \u00A9 1987-2020 HERE"
      ),
      HEREv3.normalNightTransit =  list(
        src = "HEREv3.normalNightTransit",
        sub = c("1", "2", "3", "4"),
        q = "https://{s}.base.maps.ls.hereapi.com/maptile/2.1/maptile/newest/normal.night.transit/{z}/{x}/{y}/256/png8?apiKey=XXXXXX&lg=eng",
        cit = "Map \u00A9 1987-2020 HERE"
      ),
      HEREv3.normalNightTransitMobile =  list(
        src = "HEREv3.normalNightTransitMobile",
        sub = c("1", "2", "3", "4"),
        q = "https://{s}.base.maps.ls.hereapi.com/maptile/2.1/maptile/newest/normal.night.transit.mobile/{z}/{x}/{y}/256/png8?apiKey=XXXXXX&lg=eng",
        cit = "Map \u00A9 1987-2020 HERE"
      ),
      HEREv3.reducedDay =  list(
        src = "HEREv3.reducedDay",
        sub = c("1", "2", "3", "4"),
        q = "https://{s}.base.maps.ls.hereapi.com/maptile/2.1/maptile/newest/reduced.day/{z}/{x}/{y}/256/png8?apiKey=XXXXXX&lg=eng",
        cit = "Map \u00A9 1987-2020 HERE"
      ),
      HEREv3.reducedNight =  list(
        src = "HEREv3.reducedNight",
        sub = c("1", "2", "3", "4"),
        q = "https://{s}.base.maps.ls.hereapi.com/maptile/2.1/maptile/newest/reduced.night/{z}/{x}/{y}/256/png8?apiKey=XXXXXX&lg=eng",
        cit = "Map \u00A9 1987-2020 HERE"
      ),
      HEREv3.basicMap =  list(
        src = "HEREv3.basicMap",
        sub = c("1", "2", "3", "4"),
        q = "https://{s}.base.maps.ls.hereapi.com/maptile/2.1/basetile/newest/normal.day/{z}/{x}/{y}/256/png8?apiKey=XXXXXX&lg=eng",
        cit = "Map \u00A9 1987-2020 HERE"
      ),
      HEREv3.mapLabels =  list(
        src = "HEREv3.mapLabels",
        sub = c("1", "2", "3", "4"),
        q = "https://{s}.base.maps.ls.hereapi.com/maptile/2.1/labeltile/newest/normal.day/{z}/{x}/{y}/256/png?apiKey=XXXXXX&lg=eng",
        cit = "Map \u00A9 1987-2020 HERE"
      ),
      HEREv3.trafficFlow =  list(
        src = "HEREv3.trafficFlow",
        sub = c("1", "2", "3", "4"),
        q = "https://{s}.traffic.maps.ls.hereapi.com/maptile/2.1/flowtile/newest/normal.day/{z}/{x}/{y}/256/png8?apiKey=XXXXXX&lg=eng",
        cit = "Map \u00A9 1987-2020 HERE"
      ),
      HEREv3.carnavDayGrey =  list(
        src = "HEREv3.carnavDayGrey",
        sub = c("1", "2", "3", "4"),
        q = "https://{s}.base.maps.ls.hereapi.com/maptile/2.1/maptile/newest/carnav.day.grey/{z}/{x}/{y}/256/png8?apiKey=XXXXXX&lg=eng",
        cit = "Map \u00A9 1987-2020 HERE"
      ),
      HEREv3.hybridDay =  list(
        src = "HEREv3.hybridDay",
        sub = c("1", "2", "3", "4"),
        q = "https://{s}.aerial.maps.ls.hereapi.com/maptile/2.1/maptile/newest/hybrid.day/{z}/{x}/{y}/256/png8?apiKey=XXXXXX&lg=eng",
        cit = "Map \u00A9 1987-2020 HERE"
      ),
      HEREv3.hybridDayMobile =  list(
        src = "HEREv3.hybridDayMobile",
        sub = c("1", "2", "3", "4"),
        q = "https://{s}.aerial.maps.ls.hereapi.com/maptile/2.1/maptile/newest/hybrid.day.mobile/{z}/{x}/{y}/256/png8?apiKey=XXXXXX&lg=eng",
        cit = "Map \u00A9 1987-2020 HERE"
      ),
      HEREv3.hybridDayTransit =  list(
        src = "HEREv3.hybridDayTransit",
        sub = c("1", "2", "3", "4"),
        q = "https://{s}.aerial.maps.ls.hereapi.com/maptile/2.1/maptile/newest/hybrid.day.transit/{z}/{x}/{y}/256/png8?apiKey=XXXXXX&lg=eng",
        cit = "Map \u00A9 1987-2020 HERE"
      ),
      HEREv3.hybridDayGrey =  list(
        src = "HEREv3.hybridDayGrey",
        sub = c("1", "2", "3", "4"),
        q = "https://{s}.aerial.maps.ls.hereapi.com/maptile/2.1/maptile/newest/hybrid.grey.day/{z}/{x}/{y}/256/png8?apiKey=XXXXXX&lg=eng",
        cit = "Map \u00A9 1987-2020 HERE"
      ),
      HEREv3.pedestrianDay =  list(
        src = "HEREv3.pedestrianDay",
        sub = c("1", "2", "3", "4"),
        q = "https://{s}.base.maps.ls.hereapi.com/maptile/2.1/maptile/newest/pedestrian.day/{z}/{x}/{y}/256/png8?apiKey=XXXXXX&lg=eng",
        cit = "Map \u00A9 1987-2020 HERE"
      ),
      HEREv3.pedestrianNight =  list(
        src = "HEREv3.pedestrianNight",
        sub = c("1", "2", "3", "4"),
        q = "https://{s}.base.maps.ls.hereapi.com/maptile/2.1/maptile/newest/pedestrian.night/{z}/{x}/{y}/256/png8?apiKey=XXXXXX&lg=eng",
        cit = "Map \u00A9 1987-2020 HERE"
      ),
      HEREv3.satelliteDay =  list(
        src = "HEREv3.satelliteDay",
        sub = c("1", "2", "3", "4"),
        q = "https://{s}.aerial.maps.ls.hereapi.com/maptile/2.1/maptile/newest/satellite.day/{z}/{x}/{y}/256/png8?apiKey=XXXXXX&lg=eng",
        cit = "Map \u00A9 1987-2020 HERE"
      ),
      HEREv3.terrainDay =  list(
        src = "HEREv3.terrainDay",
        sub = c("1", "2", "3", "4"),
        q = "https://{s}.aerial.maps.ls.hereapi.com/maptile/2.1/maptile/newest/terrain.day/{z}/{x}/{y}/256/png8?apiKey=XXXXXX&lg=eng",
        cit = "Map \u00A9 1987-2020 HERE"
      ),
      HEREv3.terrainDayMobile =  list(
        src = "HEREv3.terrainDayMobile",
        sub = c("1", "2", "3", "4"),
        q = "https://{s}.aerial.maps.ls.hereapi.com/maptile/2.1/maptile/newest/terrain.day.mobile/{z}/{x}/{y}/256/png8?apiKey=XXXXXX&lg=eng",
        cit = "Map \u00A9 1987-2020 HERE"
      )
    )
  }
  param
}
