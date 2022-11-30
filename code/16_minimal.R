# Day 16 minimal: zones with highest concentration of heritage buildings

library(sf)
library(tidyverse)
library(tmap)
library(dbscan)




# bcn

bcn <- st_read("data/adm_units/0301100100_UNITATS_ADM_POLIGONS.json")

bcn <- bcn[bcn$TIPUS_UA == "BARRI",]

heritage <- st_read("data/0703060100_BCN_Patrimoni_Arqui_PUNTS_V.shp")

# filter for ore interesting sites

heritage_f <- heritage[heritage$SCONJ_DESC %in% c("Bé d'interès urbanístic - C",
                                                  "Bé cultural d'interès local - B", 
                                                  "Bé cultural d'interès nacional - A"),]


heritage_f_coords <- st_coordinates(heritage_f)

# clusters

db <- hdbscan(heritage_f_coords, minPts = 30)

heritage_f$cluster <- db$cluster


ch <- heritage_f |> 
  filter(cluster != 0) |> 
  group_by(cluster) |>
  summarise() |> 
  st_convex_hull()
  

tm_shape(bcn) + 
  tm_borders(lwd = 0.5,
             col = "black") + 
  tm_shape(ch) + 
  tm_borders(col = "black",
             lwd = 2) + 
  tm_compass() +
  tm_scale_bar() + 
  tm_credits("Elaborat amb l'algorisme HDBSCAN, recull les zones més\npetites amb 30 o més elements catalogats amb classificació C, B o A") +
  tm_layout(title = "Dia 16: Mínim\nZones de concentració de patrimoni",
            title.size = 1,
            fontfamily = "Roboto condensed")
