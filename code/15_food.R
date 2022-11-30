# FOOD: Distance to BCN Municipal markets
# WEB DuBois style inspired from https://github.com/ddekadt/dubois_theme/

library(tmap)
library(sf)
library(tidyverse)

dubois_yellow = "#ebc246"
dubois_green = "#00843E"
dubois_red = "#dc143c"
dubois_blue = "#2e28d4"
dubois_brown <- "#2B1C00"
dubois_black <- "#242424"
dubois_palette = c(dubois_yellow, dubois_blue, dubois_red, dubois_brown, dubois_black)


markets <- read_csv("https://www.bcn.cat/tercerlloc/files/mercats-centrescomercials/opendatabcn_mercats-centrescomercials_mercats-municipals.csv",
                    locale = locale(encoding = "UTF-16LE"))

markets <- markets |> 
  distinct() |> 
  st_as_sf(coords = c(x = "geo_epgs_25831_x", y = "geo_epgs_25831_y"), crs = "EPSG:25831")



# BCN_census_tracts

ct <- st_read("data/seccens/0301040100_SecCens_UNITATS_ADM.shp")

# create distance matrix


dist <- st_distance(st_centroid(ct$geometry), markets)


min_dist <- apply(dist,1,min)


ct$min_dist <- min_dist


tm_shape(ct) + 
  tm_polygons(col = "min_dist",
              style = "jenks",
              palette = dubois_palette,
              title = "Distància",
              legend.format = list(text.separator = "-"),
              alpha = 0.8) + 
  tm_shape(markets) + 
  tm_dots(size = 0.25) + 
  tm_compass() +
  tm_scale_bar() + 
  tm_credits("Distància = línia recta del centroide de la secció\ncensal al mercat més proper",
             bg.color = "white",
             bg.alpha = 0.5) +
  tm_layout("Dia 15: Menjar\nDistància al mercat municipal més proper",
            title.size = 1.2,
            bg.color = "antiquewhite2",
            fontfamily = "Roboto condensed")
