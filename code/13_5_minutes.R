# 5 minutes: just a simple OSM render of Barcelona

library(sf)
library(tmap)
library(tmaptools)


# read neighbourhoods

b <- st_read("data/adm_units/0301100100_UNITATS_ADM_POLIGONS.json")

b <- b[b$CONJ_DESCR=="Barris",]


bm <- tmaptools::read_osm(b)



tm_shape(bm) + 
  tm_rgb() +
  tm_shape(b) +
  tm_borders(lwd = 2) + 
  tm_compass() + 
  tm_scale_bar() +
  tm_credits("Un minut de sobra. Hagués pogut anar més ràpid si no fos tant despistat",
             bg.color = "white",
             bg.alpha = 0.5) + 
  tm_layout(title = "Dia 13: 5 minuts\nBarris de Barcelona",
            fontfamily = "Roboto condensed")
