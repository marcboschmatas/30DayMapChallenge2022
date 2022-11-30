# Day 22: NULL. Null island, of course!


library(sf)
library(tmap)
library(tmaptools)

# make sf for null island
null <- data.frame("name" = "Null Island",
                   "x" = 0,
                   "y" = 0)
null <- st_as_sf(null, coords = c("x","y"), crs = "EPSG:4326")

# create 2km buffer
null <- st_transform(null,"EPSG:3857")
null_buffer <- st_buffer(null,2000000)


bm <- read_osm(null_buffer)



tm_shape(bm) + 
  tm_rgb() + 
  tm_shape(null) +
  tm_dots(col = "red",
          size = 1.5) + 
  tm_compass() + 
  tm_scale_bar() + 
  tm_layout(main.title = "Day 22: NULL\nNull Island!",
            fontfamily = "Roboto condensed")
