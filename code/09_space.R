# EMPTY SPACES IN BCN

library(sf)
library(tidyverse)
library(tmap)
# data downloaded from https://datacloud.icgc.cat/datacloud/cobertes-sol/gpkg_unzip/cobertes-sol-v1r0-2018.gpkg
landuse_cats <- st_read("./data/cobertes-sol-v1r0-2018.gpkg", layer = "cobertes_sol_categories")

# read layer with open spaces, defined as beaches, meadows/grassy agricultural land, barren forest land, empty urban space
open_spaces <- st_read("./data/cobertes-sol-v1r0-2018.gpkg", query = "SELECT * FROM cobertes_sol WHERE nivell_2 IN (111,228,230,233,352)")

# crop to Barcelona

bcn <- st_read("data/adm_units/0301100100_UNITATS_ADM_POLIGONS.json")

bcn <- bcn[1,c("CONJ_DESCR", "geometry")]

open_spaces_bcn <- st_intersection(open_spaces, bcn)


open_spaces_bcn <- mutate(open_spaces_bcn,
                          "land_use" = as_factor(case_when(nivell_2 == 111 | nivell_2 == 228 ~ "herba",
                                                           nivell_2 == 230 ~ "zona forestal buida",
                                                           nivell_2 == 352 ~ "espai urbà buit",
                                                           TRUE ~ "platja")))


palette <- c("#004225", "#676767", "#2B3300", "#967117")

bm <- tmaptools::read_osm(bcn)

tm_shape(bm) + 
  tm_rgb(alpha = 0.5) +
  tm_shape(bcn) +
  tm_borders() +
  tm_shape(open_spaces_bcn) +
  tm_polygons(col = "land_use",
              palette = palette,
              border.alpha = 0.5) +
  tm_compass() +
  tm_scale_bar() +
  tm_layout(title = "Dia 09: Espai\nEspais buits a Barcelona",
            fontfamily = "Roboto condensed",
            legend.bg.color = "white",
            legend.bg.alpha = 0.5,
            legend.frame = "gray")
