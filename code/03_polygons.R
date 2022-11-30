# terrace area as percentage of neighbourhood total non-built area
library(sf)
library(tmap)
library(tmaptools)
library(tidyverse)
library(dbscan)
library(fs)
set.seed(1992)
# download terraces
t <- read.csv("https://opendata-ajuntament.barcelona.cat/data/dataset/9cefbfa2-bcdf-44a0-b63a-372b48f9da93/resource/291a2c18-b786-4dfd-ae92-737a5f353699/download/2022_1s_data_set_opendata_terrasses.csv")

t_sf <- st_as_sf(t, coords = c(x = "X_ETRS89", y = "Y_ETRS89"), crs = "EPSG:25831")


#

t_area_neighbourhood <- tapply(t$SUPERFICIE_OCUPADA,t$NOM_BARRI,sum)

t_area_df <- data.frame("barri" = names(t_area_neighbourhood),
                        "area" = unname(t_area_neighbourhood))


# download and read layer w/ neighbourhoods

temp <- tempfile()
download.file(URLencode("https://opendata-ajuntament.barcelona.cat/data/dataset/808daafa-d9ce-48c0-925a-fa5afdb1ed41/resource/cd800462-f326-429f-a67a-c69b7fc4c50a/download"), temp)
unzip(temp, exdir = "data/adm_units")

bcn <- read_sf("data/adm_units/0301100100_UNITATS_ADM_POLIGONS.json")

nbs <- bcn[bcn$CONJ_DESCR == "Barris",]

# get total built area per neighbourhood

builds <- 
# join terrace area, get total area


nbs_terrace_area <- nbs |> 
  select(NOM, geometry) |> 
  mutate(NOM = case_when(NOM == "el Poble-sec" ~ "el Poble Sec",
                         TRUE ~ NOM)) |> 
  left_join(t_area_df, by = c("NOM"="barri")) |> 
  rename("terrace_area" = "area") |> 
  mutate("total_area" = as.numeric(st_area(geometry)),
         "pct_terraces" = (terrace_area/total_area)*100)


palette <- c("#00843E", "#ebc246", "#dc143c","#654321","#000000")


# plot

tm_shape(nbs_terrace_area) +
  tm_polygons(col = "pct_terraces",
           palette = palette,
           title = "% of surface",
           style = "jenks",
           textNA = "Sense dades") +
  tm_compass() +
  tm_scale_bar() +
  tm_layout("Dia 03: Polígons\n% de la superfície dels barris\nocupada per terrasses a Barcelona",
            fontfamily = "Roboto condensed",
            bg.color = "antiquewhite2",
            legend.format=list(text.separator = "-",
                               fun=function(x) paste0(formatC(x, digits=2, format="f"), " %")))
