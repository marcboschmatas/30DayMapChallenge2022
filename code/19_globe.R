# 19 globe - a globe map
library(sf)
library(tmap)
library(tmaptools)
library(tidyverse)
library(rnaturalearth)



earth <- rnaturalearth::ne_coastline(returnclass = "sf")


earth_reproj <- st_transform(earth, "ESRI:54032")


tm_shape(earth_reproj) + 
  tm_lines() + 
  tm_compass() + 
  tm_scale_bar() + 
  tm_layout(title = "Dia 19: Globus",
            fontfamily = "Roboto condensed",
            bg.color = "antiquewhite2")
