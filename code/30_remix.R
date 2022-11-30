# 30 remix - of this, of course https://tjukanovt.github.io/neighbors/#3/34.85/-19.51 and also of my previous map on catalan counties
# https://twitter.com/IdenCity_World/status/1390213064675500032
# Also using Martin Fleischmann's morphological tessellation
library(sf)
library(tmap)
library(tmaptools)
library(tidyverse)
library(sfdep)
library(moter)
library(furrr)
future::multicore()
d <- st_read("data/adm_units/0301100100_UNITATS_ADM_POLIGONS.json")


d <- d[d$CONJ_DESCR == "Districtes",c("FID", "NOM","geometry")]

# create neighbour list for each district


d$neighbours <- st_contiguity(d$geometry)



# function to define the nearest neighbouring district w/in each district


nearest_pol <- function(n){ # note, this function ONLY works with the data as given, I haven't had the time to make it generisable
  dn <- d[d$NOM == n,]
  nodn <- d[d$FID %in% unlist(dn$neighbours),]
  l <- st_as_sf(st_as_sfc(st_bbox(d)))
  tes <- moter::motess(nodn, "NOM", limit = l$x,segment=10)
  tesn <- st_intersection(tes,dn)
  return(tesn)
  
}



result <- d |> 
  st_drop_geometry() |> 
  mutate("voronois" = furrr::future_map(NOM, possibly(nearest_pol,otherwise=NA))) |> 
  rename("neighbouring_district" = "NOM") |> 
  select(-c(neighbours, FID)) |> # remove columns we don't need and which will cause an error once
  unnest(cols = voronois)

result <- st_sf(result)

result <- mutate(result,"NOM_clean" = str_remove(NOM,"\\.\\d"))


tmap_mode("plot")

dubois_yellow = "#ebc246"
dubois_green = "#00843E"
dubois_red = "#dc143c"
dubois_blue = "#2e28d4"
dubois_brown <- "#2B1C00"
dubois_black <- "#242424"
dubois_pink <- "#ffc0cb"
dubois_tan <- "#d2b48c"
dubois_palette = c(dubois_blue, dubois_yellow, dubois_brown, dubois_red, dubois_green, dubois_black,
                   dubois_tan, dubois_green, dubois_red, dubois_pink)


tm_shape(d) + 
  tm_polygons() + 
  tm_shape(result) + 
  tm_polygons(col = "NOM_clean",
              palette = dubois_palette,
              stretch_palette = FALSE,
              title = "Districte més proper") + 
  tm_shape(d) + 
  tm_borders(col = "black",
             lwd = 2) + 
  tm_compass() + 
  tm_scale_bar() + 
  tm_credits("Remix of Topi Tjukanov's Neighbor map") + 
  tm_layout("Dia 30: Remix\nDistricte veí més proper",
            fontfamily = "Roboto condensed",
            bg.color = "antiquewhite2")
