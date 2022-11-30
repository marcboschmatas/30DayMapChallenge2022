# Concave hulls of most common brands in Catalonia - Idea by Topi Tjukanov


library(sf)
library(osmdata)
library(tidyverse)
library(tmap)
library(RPostgreSQL)


# read cadaster data & upload it to postgis server


cad <- st_read("/home/marc/Documents/imatges_bcn/buildings_bcn/A.ES.SDGC.BU.08900.building.gml")

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









q <- "WITH brands AS (SELECT brand, name, ST_CENTROID(ST_TRANSFORM(way,4326)) as geometry
FROM planet_osm_polygon
WHERE brand IS NOT NULL
UNION ALL
SELECT brand,name,ST_TRANSFORM(way,4326) AS geometry
FROM planet_osm_point
WHERE brand IS  NOT NULL),
top10 AS (SELECT brand, COUNT(brand)
FROM brands
GROUP BY brand
ORDER BY COUNT(brand) DESC
LIMIT 10)
SELECT brands.brand, ST_CONCAVEHULL(ST_COLLECT(brands.geometry),0.8)
FROM brands, top10
WHERE brands.brand IN (top10.brand)
GROUP BY brands.brand;
"

q <- gsub("\n", " ", q)


ch <- st_read(connec, query = q)


cat <- st_read("data/divisions-administratives_20221026_092107/divisions-administratives-comarques-5000.shp")

ch_t <- st_transform(ch, st_crs(cat))
ch_l <- st_cast(ch_t,"LINESTRING")


tm_shape(cat) + 
  tm_borders() + 
  tm_shape(ch_l) + 
  tm_lines(col = "#2B1C00",
             lwd = 2,
           palette = dubois_palette_long) +
  tm_credits("Font: Col·laboradors d'OpenStreetMap") +
  tm_compass() + 
  tm_scale_bar()+ 
  tm_facets(by = "brand",
            as.layers = TRUE,
            free.coords = FALSE) + 
  tm_layout(main.title = "Dia 8: OpenStreetMap\nÀrees de cobertura de les 10 marques més representades a OSM",
            fontfamily = "Roboto condensed",
            attr.outside = TRUE,
            scale = 1.2)
