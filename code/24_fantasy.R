# DAY 24: Fantasy - shops in the "Nerd triangle" (Triangle friqui) of Barcelona

library(sf)
library(tmap)
library(tidyverse)

#' overpass query to download Triangle Friqui
#' https://overpass-turbo.eu/s/1nC6
#' 
# read
tf <- st_read("data/tf.geojson")

# read shops in BCN (2019 data)
b <- st_read("https://opendata-ajuntament.barcelona.cat/data/dataset/62fb990e-4cc3-457a-aea1-497604e15659/resource/495c434e-b005-416e-b760-dc79f56dff3a/download/2019_censcomercialbcn_detall.geojson")

# get intersections

b_tf <- st_intersection(b,tf)


# filter for active shops in the leisure and culture sector

b_tf_filtered <- b_tf |> 
  filter(Nom_Principal_Activitat == "Actiu" &
           Nom_Grup_Activitat == "Oci i cultura")
# get base map

bm <- tmaptools::read_osm(tf,type = "stamen-toner")



tm_shape(bm) + 
  tm_rgb() +
  tm_shape(tf) + 
  tm_borders(col = "blue",
             lwd = 1.5) + 
  tm_shape(b_tf_filtered) + 
  tm_symbols(shape = 22,
             col = "black",
             size = 0.5) + 
  tm_compass() + 
  tm_scale_bar() + 
  tm_layout("Dia 24: Fantasia\nBotigues d'oci i cultura al Triangle Friqui",
            title.position = c("right","top"),
            title.bg.color = "white",
            title.bg.alpha = 0.7,
            fontfamily = "Roboto condensed")
