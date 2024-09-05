# 2024-08-25 ====

## * extract buildings around kleiner feldberg from osm and calculate area of 
##   larger features
## * assess roof slope from lidar

library(sf)
library(osmdata)
library(terra)
library(mapview)


### osm (poi) ----

poi = getbb(
  "Kleiner Feldberg"
  , format_out = "data.frame"
) |> 
  st_as_sf(
    coords = c("lon", "lat")
    , crs = 4326L
  ) |> 
  st_transform(
    crs = 25832L
  )

poi$boundingbox = NULL


### lidar ----

pattern = st_centroid(poi) |> 
  st_coordinates() |> 
  substr(1, c(3, 4)) |> 
  paste(
    collapse = "_"
  )

# downloaded from https://gds.hessen.de/INTERSHOP/web/WFS/HLBG-Geodaten-Site/de_DE/-/EUR/ViewDownloadcenter-Start
# > 3D-Daten > Digitales Oberflächenmodell (DOM1) > Hochtaunuskreis
ifl = list.files(
  "inst/extdata/Königstein im Taunus - DOM1"
  , pattern = sprintf(
    "%s.*\\.tif$"
    , pattern
  )
  , full.names = TRUE
)

rst = rast(ifl)


### osm (aoi) ----

## draw 250-m buffer around poi
aoi = st_buffer(
  poi
  , dist = 250
)

## select osm buildings in buffer extent
bbx = aoi |> 
  st_transform(
    crs = 4326L
  ) |> 
  st_bbox()

osm_query = opq(bbx) |> 
  add_osm_feature(
    key = "building"
  )

osm_data = osmdata_sf(osm_query)

buildings = st_transform(
  osm_data$osm_polygons
  , crs = 25832L
)

## calculate base area
buildings$area = st_area(
  buildings
) |> 
  round(
    digits = 2L
  )

## discard smaller buildings < 50 sqm
minimum_size = 60
units(minimum_size) = "m^2"

large_buildings = subset(
  buildings
  , area > minimum_size
)

sum(large_buildings$area)

## visualize
crp = crop(
  rst
  , st_buffer(
    large_buildings
    , 50
  )
)

slp = terrain(crp)
slp_hires = disagg(slp, fact = 4, method = "bilinear")

(
  m = mapview(
    large_buildings
    , layer.name = "Buildings"
    , legend = FALSE
    , color = "cornflowerblue"
    , lwd = 2
    , alpha.regions = 0
  ) + 
    mapview(
      raster::raster(crp)
      , layer.name = "Elevation (m)"
      # , alpha.regions = 1
      , hide = TRUE
    ) + 
    mapview(
      raster::raster(slp_hires)
      , maxpixels = 1e6
      , layer.name = "Slope (°)"
      # , alpha.regions = 1
      , hide = TRUE
    )
)
