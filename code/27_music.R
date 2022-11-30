# 27 music Music schools in BCN

library(tidyverse)
library(sf)
library(RSocrata)
library(tmap)
library(tmaptools)
library(modthemes)

# download music schools
elemental <- read.socrata("https://analisi.transparenciacatalunya.cat/resource/kvmv-ahh4.json?nom_municipi=Barcelona&curs=2022/2023&muse=MUSE")

professional <- read.socrata("https://analisi.transparenciacatalunya.cat/resource/kvmv-ahh4.json?nom_municipi=Barcelona&curs=2022/2023&musp=MUSP")

# prepare

total <- unique(rbind(elemental,professional))

total_geo <- total |> 
  rename("titularitat" = "nom_naturalesa") |> 
  mutate("nivell" = case_when(muse == "MUSE" & musp == "MUSP" ~ "Elemental i professional",
                              muse == "MUSE" & is.na(musp) ~ "Elemental",
                              is.na(muse) & musp == "MUSP" ~ "Professional")) |> 
  st_as_sf(coords = c("coordenades_geo_x","coordenades_geo_y"), crs = "EPSG:4326") |> 
  st_transform("EPSG:25831")


# read neighborhoods

barris <- read_sf("data/adm_units/0301100100_UNITATS_ADM_POLIGONS.json")

barris <- barris[barris$SCONJ_DESC == "Barri",]


bm <- tmaptools::read_osm(barris, type = "osm")



tm_shape(bm) + 
  tm_rgb(alpha = 0.5) + 
  tm_shape(barris) + 
  tm_borders(col = "black",
             lwd = 2) + 
  tm_shape(total_geo) +
  tm_symbols(col = "titularitat",
             palette = c(dubois_blue,
                         dubois_red),
             shape = "nivell",
             size = 0.25) + 
  tm_compass() + 
  tm_scale_bar() + 
  tm_layout("Dia 27: Música\nEscoles de música a Barcelona",
            fontfamily = "Roboto condensed",
            legend.position = c("left", "bottom"),
            legend.bg.color = "white",
            legend.bg.alpha = 0.5)
