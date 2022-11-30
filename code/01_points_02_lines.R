# finding all bike lane cul-de-sacs in Barcelona


library(sf)
library(tidyverse)
library(tmap)
library(sfnetworks)
library(tidygraph)
library(osmdata)
library(stplanr)
# download bike lane data



streets <- st_read("https://opendata-ajuntament.barcelona.cat/data/dataset/e3497ea4-0bae-4093-94a7-119df50a8a74/resource/4608cf0c-2f11-4a25-891f-c5afc3af82c5/download")

# break at intersections

streets_brok = rnet_breakup_vertices(total)

# create network object
network <- sfnetworks::as_sfnetwork(streets)

# determine cul-de-sacs (centrality = 0)

# get centrality

simple <- network |>
  activate("edges") |>
  filter(!tidygraph::edge_is_multiple()) |>
  filter(!tidygraph::edge_is_loop())



culdesac <- simple  |> 
  activate(nodes) |>
  mutate(degree = tidygraph::centrality_degree()) |> 
  filter(degree == 0) |> 
  st_as_sf()



# map points


# get basemap

bm <- tmaptools::read_osm(streets)

tm_shape(bm) + 
  tm_rgb() + 
  tm_shape(culdesac) +
  tm_dots(col = "#dc143c",
          size = 0.1) +
  tm_compass() +
  tm_scale_bar() +
  tm_layout("Dia 01: Punts\nCarrils bici sense sortida a Barcelona",
            fontfamily = "Roboto condensed")



# DAY TWO: CONNECTIVITY OF BIKE LANES


# download osm bike lane data
bl <- opq("Barcelona") |>
  add_osm_feature(key = 'highway', value = "cycleway") |>
  osmdata_sf()

lanes <- bl$osm_lines

# transform into network and determine edge betweenness
network <- sfnetworks::as_sfnetwork(lanes)

simple <- network |>
  activate("edges") |>
  filter(!tidygraph::edge_is_multiple()) |>
  filter(!tidygraph::edge_is_loop())


lane_connectivity <- simple |> 
  activate(edges) |> 
  mutate(degree = tidygraph::centrality_edge_betweenness()) |> 
  st_as_sf()

palette <- c("#00aa00", "#ffd700", "#dc143c","#654321","#000000")
tm_shape(bm) + 
  tm_rgb(alpha = 0.4) + 
  tm_shape(lane_connectivity) +
  tm_lines(col = "degree",
           palette = rev(palette),
           lwd = 2,
           style = "jenks") +
  tm_compass() +
  tm_scale_bar() +
  tm_credits("La connectivitat dels vèrtexs és el nombre de trajectes\nmés curts dins d'una xarxa que passen per un segment en concret",
             bg.color = "white",
             bg.alpha = 0.6) +
  tm_layout("Dia 02: Línies\nConnectivitat dels vèrtexs en els\ncarrils bici de Barcelona",
            fontfamily = "Roboto condensed")
