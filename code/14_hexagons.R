# 14 hexagons.
# This is a DnD Hexcrawl based on Barcelona. Rules are as follows.
# 1) If average altitude >200 m, classified as mountain hexes.
# 2) If 1) is false and the average NDVI is over 0.4, classified as forest hexes.
# 3) If both 1) and 2) are false and they are in the top 10 of heritage buildings per hex, classified as dungeon hexes
# 4) all the rest classified as desert hexes

library(sf)
library(stars)
library(tidyverse)
library(tmap)
library(geobgu) # extract raster to polygon
library(raster)

# read BCN map

bcn <- st_read("data/adm_units/0301100100_UNITATS_ADM_POLIGONS.json")
bcn <- bcn[1,]

bcn_hex <- st_make_grid(bcn, square = FALSE)


bcn_hex <- bcn_hex |> 
  st_sf() |> 
  st_intersection(bcn)



bcn_hex <- bcn_hex |> 
  select("bcn_hex") |> 
  rename("geometry" = "bcn_hex")



# read altitude raster


elevation <- raster("data/elevation_bcn.tif")

# get average per hex
elevation_hex <- extract(elevation,bcn_hex,fun=mean, na.rm=TRUE)


bcn_hex$elevation <- as.vector(elevation_hex)

# read ndvi raster

ndvi <- stars::read_stars("/home/marc/Documents/imatges_bcn/raster_normalitzat.tiff")



# crop raster
ndvi <- st_crop(ndvi, bcn_hex)

# rescale

ndvi <- st_apply(ndvi, 1, \(x) ((x-100)/100))


write_stars(ndvi,"data/ndvi_bcn.tiff")


ndvi <- raster::raster("data/ndvi_bcn.tiff")

ndvi_hex <- extract(ndvi,bcn_hex,fun=mean, na.rm=TRUE)


bcn_hex$ndvi <- as.vector(ndvi_hex)

rm(elevation)
rm(ndvi)



# read heritage layer - https://opendata-ajuntament.barcelona.cat/data/es/dataset/patrimoni-arquitectonic-protegit

heritage <- st_read("data/0703060100_BCN_Patrimoni_Arqui_PUNTS_V.shp")


# count points per hex

bcn_hex$id <- 1:85


heritage <- heritage |> 
  st_intersection(bcn_hex[,c("id", "bcn_hex")]) |> 
  st_drop_geometry() |> 
  group_by(id) |> 
  summarise("heritage" = n())


bcn_hex <- left_join(bcn_hex, heritage, by = "id")

bcn_hex <- mutate(bcn_hex,hex = case_when(elevation > 300 ~ "muntanya",
                                            ndvi > 0.4 ~ "bosc",
                                            heritage > 700 ~ "ruïnes",
                                            TRUE ~ "desert"),
                  hex = as.factor(hex))


palette <- c("springgreen4", "burlywood", "white", "gray26")


tm_shape(bcn_hex) + 
  tm_polygons(col = "hex",
              palette = palette) + 
  tm_credits("Hexàgons muntanya tenen una alçada mitjana de més de 300 m\nHexàgons bosc tenen un índex de vegetació mitjà de més de 0.4\nHexàgons ruïnes són el top 10 d'espais catalogats per hexàgon\nHexàgons desert són la resta",
             bg.color = "white",
             bg.alpha = 0.6,
             size = 10) + 
  tm_layout("Dia 14: Hexàgons\nMapa d'exploració de Barcelona",
            sepia.intensity = 0.75,
            fontfamily = "Roboto condensed")
