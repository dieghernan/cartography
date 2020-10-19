library(sf)
library(sp)
mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"), quiet = TRUE)
mtq1 <- mtq[1,]
st_geometry(mtq1) <- st_centroid(st_geometry(mtq1))
home <- length(unclass(packageVersion("cartography"))[[1]]) == 4
if(home){
  suppressMessages(expect_warning(getTiles(x = mtq, verbose=TRUE)))
  expect_true(methods::is(getTiles(x=mtq, crop = TRUE), "RasterBrick"))
  expect_true(methods::is(getTiles(x=mtq1, crop = TRUE), "RasterBrick"))
  expect_warning(getTiles(spdf = as(mtq, "Spatial"), zoom = 1))
  expect_true(methods::is(getTiles(x = as(mtq, "Spatial")), "RasterBrick"))
  expect_true(methods::is(getTiles(x=mtq, type = "Stamen.Watercolor", zoom = 2), 
                          "RasterBrick"))
  x <- getTiles(x = mtq)
  st_crs(mtq) <- NA
  expect_error(getTiles(x = mtq, zoom = 1))
  expect_silent(tilesLayer(x, add = FALSE))
  expect_silent(tilesLayer(x, add = TRUE))
}

# Tracking providers for regular check
mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"), quiet = TRUE)

allprovs = c('CartoDB', 'CartoDB.DarkMatter','CartoDB.DarkMatterNoLabels',
           'CartoDB.DarkMatterOnlyLabels','CartoDB.Positron' ,'CartoDB.PositronNoLabels',
           'CartoDB.PositronOnlyLabels','CartoDB.Voyager','CartoDB.VoyagerLabelsUnder',
           'CartoDB.VoyagerNoLabels','CartoDB.VoyagerOnlyLabels',
           'Esri','Esri.DeLorme','Esri.NatGeoWorldMap','Esri.OceanBasemap',
           'Esri.WorldGrayCanvas','Esri.WorldImagery','Esri.WorldShadedRelief',
           'Esri.WorldStreetMap','Esri.WorldTerrain','Esri.WorldTopoMap',
           'HikeBike' ,'HikeBike.HikeBike',
           'Hydda','Hydda.Base','Hydda.Full','Hydda.RoadsAndLabels',
           'OpenMapSurfer','OpenMapSurfer.AdminBounds','OpenMapSurfer.ElementsAtRisk',
           'OpenMapSurfer.Hybrid','OpenMapSurfer.Roads',
           'OpenStreetMap' ,'OpenStreetMap.DE','OpenStreetMap.France',
           'OpenStreetMap.HOT' ,'OpenStreetMap.MapnikBW' ,
           'OpenTopoMap' ,
           'Stamen' ,'Stamen.Terrain','Stamen.TerrainBackground','Stamen.TerrainLabels',
           'Stamen.Toner','Stamen.TonerBackground','Stamen.TonerHybrid',
           'Stamen.TonerLabels','Stamen.TonerLines','Stamen.TonerLite','Stamen.Watercolor' ,
           'Wikimedia'
           )
           
allprovs<- allprovs[1:5]

for (i in 1:length(allprovs)) {
  expect_true(methods::is(getTiles(x=mtq, zoom=3, type = allprovs[i]), "RasterBrick"))
}

tfapi = c(
  'Thunderforest',
  'Thunderforest.Landscape',
  'Thunderforest.MobileAtlas',
  'Thunderforest.Neighbourhood',
  'Thunderforest.OpenCycleMap',
  'Thunderforest.Outdoors',
  'Thunderforest.Pioneer',
  'Thunderforest.SpinalMap',
  'Thunderforest.Transport',
  'Thunderforest.TransportDark'
)

#for (i in 1:length(tfapi)) {
  #expect_true(methods::is(getTiles(x=mtq, zoom=3, type = tfapi[i], apikey = "98a397435a0b4f6b96fbf2781579e06e"), "RasterBrick"))
#}
