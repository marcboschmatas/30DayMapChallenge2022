# BAD MAP: Getting Latitude and longitude mixed up

library(sf)
library(tmap)
library(tmaptools)
library(tidyverse)
library(httr)
library(jsonlite)

# download libraries of BCN province

lib <- read_csv("https://do.diba.cat/api/dataset/biblioteques/format/csv/pag-ini/1/pag-fi/29999")

# filter NA
lib <- lib[!is.na(lib$Localització),]

# transform to sf

lib_sf_good <- lib |> 
  mutate(`Localització` = strsplit(`Localització`, ","),
         lat =  sapply(Localització, \(x) x[1]),
         lon = sapply(Localització, \(x) x[2])) |> 
  st_as_sf(coords = c(x = "lon", y = "lat"), crs = "EPSG:4326")

lib_sf_bad <- lib |> 
  mutate(`Localització` = strsplit(`Localització`, ","),
         lat =  sapply(Localització, \(x) x[1]),
         lon = sapply(Localització, \(x) x[2])) |> 
  st_as_sf(coords = c(x = "lat", y = "lon"), crs = "EPSG:4326")

# get basemaps

bm_good <- tmaptools::read_osm(lib_sf_good)
bm_bad <- tmaptools::read_osm(lib_sf_bad)

bad <- tm_shape(bm_bad) + 
  tm_rgb() + 
  tm_shape(lib_sf_bad) +
  tm_dots() + 
  tm_compass() +
  tm_scale_bar() + 
  tm_credits("Biblioteques de la DIBA... Per Somàlia") +
  tm_layout(main.title = "Dia 10: Mapa Dolent\nBiblioteques de la Diputació de BCN",
            fontfamily = "Roboto condensed")


good <- tm_shape(bm_good) + 
  tm_rgb() + 
  tm_shape(lib_sf_good) +
  tm_dots() + 
  tm_compass() +
  tm_scale_bar() + 
  tm_credits("Recordeu mainada: X és Longitud i Y és Latitud!") + 
  tm_layout(fontfamily = "Roboto condensed")
tmap_arrange(bad, good)
