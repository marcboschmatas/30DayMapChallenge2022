
# Day 18: Blue. Streams and canals in Barcelona. Most of them run through the sewers if at all
#https://geoserveis.icgc.cat/icgc_geotecnicbcn25m/wms/service?REQUEST=GetMap&SERVICE=WMS&VERSION=1.3.0&LAYERS=RIER_LN&STYLES=&FORMAT=image/png&BGCOLOR=0xFFFFFF&TRANSPARENT=TRUE&CRS=EPSG:25831&BBOX=420812.5,4574282.4,435480.4,4591066.0&WIDTH=865&HEIGHT=990
library(sf)
library(terra)
library(stars)
library(tidyverse)
library(tmap)


a <- rast("data/aigua_bcn.tiff")






# the coordinates are hella wrong, so I need to set the extent right and reproject to EPSG:25831


bcn <- st_read("data/adm_units/0301100100_UNITATS_ADM_POLIGONS.json")
bcn <- bcn[1,]
bbox <- st_bbox(bcn)

bbox <- c(bbox[[1]],bbox[[3]],bbox[[2]],bbox[[4]]) # reordered from st_bbox(bcn)




ext(a) <- bbox

terra::crs(a) <- "EPSG:25831"

stars_a <- stars::st_as_stars(a)

crop_a <- st_crop(stars_a,bcn)
# download basemap

# plot




tm_shape(crop_a) +
  tm_rgb() + 
  tm_shape(bcn) + 
  tm_borders(col = "black",
             lwd = 2) + 
  tm_add_legend(type = "fill",
                labels = c("Riera","Canal o rec","Altres"),
                col = c("blue4","skyblue","coral")) +
  tm_compass() + 
  tm_scale_bar() + 
  tm_credits("La major part de rieres van per les clavegueres,\nsi és que tenen aigua",
             bg.color = "white",
             bg.alpha = 0.7) +
  tm_layout("Dia 18: Blau\nRieres i canals de Barcelona",
            fontfamily = "Roboto condensed",
            legend.position = c("left","bottom"))

