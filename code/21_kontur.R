# Day 21: Kontur. Dasymetric map of Catalonia
library(sf)
library(tidyverse)
library(tmap)
library(spatstat)


# read catalonia

cat <- st_read("data/divisions-administratives_20221026_092107/divisions-administratives-comarques-5000.shp")


# read kontur
#design filter
wkt = cat |> 
  st_union() |> 
  st_transform("EPSG:3857") |> 
  st_geometry() |> 
  st_as_text()

kontur <- st_read("data/kontur_population_20220630.gpkg", wkt_filter = wkt)

tm_shape(cat) + 
  tm_borders() + 
  tm_shape(kontur) +
  tm_polygons(col = "population",
              style = "equal",
              convert2density = TRUE,
              title = "hab/km2",
              palette = "viridis",
              lwd = 0) + 
  tm_compass() + 
  tm_scale_bar() + 
  tm_credits("El Kontur Population Dataset recull la població del món en hexàgons de 400 metres de costat",
             size = 12) +
  tm_layout(title = "Dia 21: Kontur Population Dataset\nDensitat de Població a Catalunya",
            title.position = c("right", "top"),
            title.size = 1,
            fontfamily = "Roboto condensed",
            legend.format = list(text.separator = "-"))
