# 2024-08-25 ====

## * extract buildings around kleiner feldberg from osm and calculate area of 
##   larger features
## * assess roof slope from lidar

library(sf)
library(osmdata)


### AOI ----

shp = st_bbox(rst) |> 
  st_as_sfc()

shp = st_as_sf(
  data.table::data.table(
    "Source" = "Königstein im Taunus"
    , geometry = shp
  )
)

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

aoi = st_buffer(
  st_as_sfc(poi) |> st_transform(crs = 25832L)
  , dist = 250
)

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

areas = st_area(
  buildings
)

minimum_size = 60
units(minimum_size) = "m^2"

large_buildings = subset(
  buildings
  , idx <<- areas >= minimum_size
)

sum(areas[idx])


### LiDAR ----

tfs = list.files(
  "/mnt/c/Users/flowd/Downloads/Königstein im Taunus - DOM1"
  , pattern = "\\.tif$"
  , full.names = TRUE
)

lst = lapply(
  tfs
  , terra::rast
)

rst = Reduce(
  terra::merge
  , lst
)

crp = terra::crop(
  rst
  , large_buildings
)

mapview::mapview(large_buildings) + 
  crp
