library(sf)
library(terra)
library(stars)
library(tidyverse)
library(tmap)

# read image

# downloaded w/ following query 
# https://geoserveis.icgc.cat/servei/catalunya/ndvi/wms?REQUEST=GetMap&SERVICE=WMS&VERSION=1.3.0&LAYERS=ndvi_2021&STYLES=&FORMAT=image/tiff&BGCOLOR=0xFFFFFF&TRANSPARENT=TRUE&CRS=EPSG:25831&BBOX=206985.645933014,4480000,573014.354066986,4780000&WIDTH=2048&HEIGHT=1678

image <- stars::read_stars("./data/ndvi_catalunya.tiff")

# transform to -1/1 scale

image <- st_apply(image, 1, \(x) ((x-100)/100))

# catalan counties - downloaded from the following link https://analisi.transparenciacatalunya.cat/en/Urbanisme-infraestructures/L-mits-administratius-municipals-de-Catalunya/97qg-zvqd


com <- st_read("data/divisions-administratives_20221026_092107/divisions-administratives-comarques-5000.shp")
# crop raster to catalonia

image <- st_crop(image, com)

# write image and read it again to get a pointer

stars::write_stars(image, "./data/cropped_raster.tiff")

image <- stars::read_stars("./data/cropped_raster.tiff")
# map


tm_shape(terra::rast(image)[[1]]) +
  tm_raster(palette = "RdYlGn",
            breaks = c(-1,0,0.2,0.4,0.6,1),
            labels = c("Aigua, cobertes artificials, etc.", 
                       "Sòl nu o vegetació morta",
                       "Vegetació poc abundant, sense vigor",
                       "Vegetació abundant i/o vigorosa",
                       "Vegetació molt abundant o vigorosa"),
                       title = "Índex de Vegetació de Diferència Normalitzada",
            midpoint = 0.3) +
  tm_shape(com) +
  tm_borders() +
  tm_compass() +
  tm_scale_bar() +
  tm_layout("Dia 04: Verd\nDensitat de vegetació a Catalunya",
            title.position = c("right","top"),
            legend.position = c("right", "bottom"),
            fontfamily = "Roboto Condensed")
