# day 20: my favourite. Gravel roads and cycling tracks in Les Gavarres, a hill range in northeastern Catalonia


library(RPostgreSQL)
library(tidyverse)
library(sf)
library(tmap)
library(tmaptools)

tryCatch({
  drv <- dbDriver("PostgreSQL")
  print("Connecting to Database…")
  connec <- dbConnect(drv, 
                      dbname = "",
                      host = "", 
                      port = "",
                      user = "", 
                      password = "")
  print("Database Connected!")
},
error=function(cond) {
  print("Unable to connect to Database.")
})


q_gavarres <- "SELECT name, ST_TRANSFORM(way,25831) AS geometry FROM planet_osm_polygon WHERE name LIKE '%natural de les Gavarres' AND boundary = 'protected_area'"


gavarres <- st_read(connec, query = q_gavarres)



q_fitor <- "SELECT name, ST_TRANSFORM(ST_CENTROID(way),25831) FROM planet_osm_polygon WHERE name = 'Santa Coloma de Fitor'"


fitor <- st_read(connec, query = q_fitor)


q_ways <- "WITH
g AS (SELECT name,way
	 FROM planet_osm_polygon
	 WHERE name LIKE '%natural de les Gavarres'
	 AND boundary = 'protected_area')
SELECT l.highway,ST_TRANSFORM(l.way,25831) AS geometry
FROM planet_osm_line l, g
WHERE (highway in ('track','cycleway')
OR bicycle = 'yes')
AND ST_INTERSECTS(l.way, g.way);"


q_ways <- gsub("\n"," ", q_ways)
q_ways <- gsub("\t","",q_ways)


paths <- st_read(connec, query = q_ways)


bm <- tmaptools::read_osm(gavarres)

tm_shape(bm) + 
  tm_rgb(alpha = 0.6) + 
  tm_shape(gavarres) + 
  tm_polygons(col = "#6E8B3D",
              alpha = 0.6) + 
  tm_shape(paths) + 
  tm_lines(col = "#696969") + 
  tm_shape(fitor) +
  tm_dots(col = "red",
             size = 1.5) + 
  tm_add_legend(type = "symbol",
                labels = "Santa Coloma de Fitor",
                col = "red") + 
  tm_logo(file = "data/fitor.png",
          position = c("right", "top")) + 
  tm_compass() + 
  tm_scale_bar() + 
  tm_credits("Foto original a at https://commons.wikimedia.org/wiki/File:Fitor,_Esgl%C3%A9sia_de_Santa_Coloma-PM_28457.jpg",
             bg.color = "white",
             bg.alpha = 0.5) +
  tm_layout(main.title = "Dia 20: My Favourite\nPistes ciclables a Les Gavarres",
            fontfamily = "Roboto condensed",
            legend.position = c("right","top"))
