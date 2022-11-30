# DAY 12: SCALE: Largest and smallest municipality in Catalonia by area with equal and free scales
# WEB DuBois style inspired from https://github.com/ddekadt/dubois_theme/
library(tmap)
#library(tmaptools)
library(sf)
library(tidyverse)


munis <- st_read("./data/munis_cat.geojson")

munis <- munis[,c("nom_muni", "geometry")]


munis$area <- st_area(munis$geometry)

# get largest and smallest municipality

munis_maxmin <- munis[munis$area == max(munis$area) | munis$area == min(munis$area),]



unequal <-  tm_shape(munis_maxmin) +
  tm_polygons(col = "#dc143c",
              alpha = 0.5,
              border.col = "black",
             lwd = 2) +
  tm_compass() + 
  tm_scale_bar() +
  tm_facets(by = "nom_muni") + 
  tm_layout(main.title = "Dia 12: Escala\nMunicipi més gran i més petit de Catalunya",
            bg.color = "antiquewhite2",
            panel.label.bg.color = "linen",
            fontfamily = "Roboto condensed")

equal <-  tm_shape(munis_maxmin) +
  tm_polygons(col = "#dc143c",
              alpha = 0.5,
              border.col = "black",
              lwd = 2) +
  tm_compass() + 
  tm_scale_bar() +
  tm_facets(by = "nom_muni", free.coords = FALSE) + 
  tm_layout(bg.color = "antiquewhite2",
            panel.label.bg.color = "linen",
            fontfamily = "Roboto condensed")


tmap_arrange(unequal, equal)
