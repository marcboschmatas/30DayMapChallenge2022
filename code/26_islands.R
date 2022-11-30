# cobertures del sòl a les Illes Medes
library(sf)
library(tidyverse)
library(tmap)
library(terra)
library(modthemes)
library(osmdata)
set.seed(19920313)
# MEDES

p <- opq("Catalunya") |> 
  add_osm_feature(key = "place", value = "archipelago") |> 
  add_osm_feature(key = "name", value = "Illes Medes") |> 
  osmdata_sf()


medes <- p$osm_multipolygons
medes <- st_transform(medes, "EPSG:25831")


bbox_medes <- st_bbox(medes)

# download data
# https://geoserveis.icgc.cat/icc_mapesbase/wms/service?REQUEST=GetMap&VERSION=1.1.0&SERVICE=WMS&SRS=EPSG:25831&BBOX=518121.8,4654371.3,518934.4,4655577.4&WIDTH=543&HEIGHT=806&LAYERS=ortoi5m&STYLES=&FORMAT=image/tiff&BGCOLOR=0xFFFFFF&TRANSPARENT=TRUE&EXCEPTION=INIMAGE


raster_infrared <- terra::rast("data/raster_medes.tiff") # infrared colours


#kmeans



vls <- values(raster_infrared)

vls <- vls[,c(1:3)]


kmncluster2 <- kmeans(vls, centers = 3, iter.max = 500, algorithm = "Lloyd")

knr <- terra::rast(raster_infrared)
values(knr) <- kmncluster2$cluster


palette <- c("#c9ad98", "#80abd6", "#4b513d")
p1 <- tm_shape(knr[[1]]) + 
  tm_raster(breaks = c(1,2,3,4),
            palette = palette,
            labels = c("Sòl nu", "Aigua i ombra", "Vegetació"),
            title = "valors") +
  tm_layout(legend.position = c("left", "bottom"),
            legend.bg.color = "white",
            title = "\nPredicció",
            main.title = "Dia 26: Illes.\nPredicció de cobertes del sòl\na les Illes Medes",
            fontfamily = "Roboto condensed",
            frame = FALSE)


bm <- terra::rast("data/raster_medes_real.tiff") # real colours


p2 <- tm_shape(bm) + 
  tm_rgb() + 
  tm_layout(title = "\nReal",
            fontfamily = "Roboto condensed",
            frame = FALSE)

p3 <- tm_shape(raster_infrared) + 
  tm_rgb() + 
  tm_layout(title = "\nInfraroig",
            fontfamily = "Roboto condensed",
            frame = FALSE)


tmap_arrange(p1, p2,p3)


