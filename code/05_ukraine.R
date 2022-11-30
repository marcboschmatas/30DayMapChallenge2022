# municipalities with a street, square or similar dedicated to Ukraine in Catalonia

library(sf)
library(tmap)
library(tmaptools)
library(data.table)
library(tidytable)

# read highways from OSM - data downloaded from here https://download.geofabrik.de/europe/spain/cataluna.html

st_layers("./data/cataluna-latest.osm.pbf")


ukraine_streets <- st_read("./data/cataluna-latest.osm.pbf", query = "SELECT highway, name FROM lines")


ukraine_streets_dt <- ukraine_streets |> 
  st_drop_geometry() |> 
  as.data.table() |> 
  filter.(str_detect(name, "Ucraïna") | 
            str_detect(name, "Ucraina") |
            str_detect(name, "ucraina") |
            str_detect(name, "ucraïna") |
            str_detect(name, "Ucrania") |
            str_detect(name, "ucrania"))


ukraine_streets <- filter(ukraine_streets, name %in% ukraine_streets_dt$name)

# catalan municipalities - downloaded from the following link https://analisi.transparenciacatalunya.cat/en/Urbanisme-infraestructures/L-mits-administratius-municipals-de-Catalunya/97qg-zvqd


munis <- st_read("./data/munis_cat.geojson")

munis <- munis[,c("nom_muni", "geometry")]


ukraine_streets <- st_join(ukraine_streets, munis)


munis$ukraine <- ifelse(munis$nom_muni %in% ukraine_streets$nom_muni, "Hi ha carrer d'Ucraïna", "No hi ha carrer d'Ucraïna")


tm_shape(munis) +
  tm_polygons(col = "ukraine",
              palette = c('#0057b7','#ffd700'),
              title = "") +
              tm_compass() +
              tm_scale_bar() +
              tm_layout("Dia 5: Ucraïna\nMunicipis catalans amb un carrer\ndedicat a Ucraïna",
                        title.position = c("right","top"),
                        bg.color = "antiquewhite2",
                          legend.position = c("right", "bottom"),
                        fontfamily = "Roboto condensed")
