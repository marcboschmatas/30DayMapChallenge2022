# Daily travels between Barcelona neighbourhoods in 2021/10/20


library(sf)
library(tidyverse)
library(tidytransit)
library(sfnetworks)
library(stplanr)
library(tmap)

# read mobility cells
cells <- st_read("/home/marc/Documents/rodalies/data/celdas_marzo_2020.shp")

cells <- cells |>  
  st_transform("EPSG:25831")  |> 
  st_make_valid()


# filter for cells in BCN

cells_bcn <- filter(cells,str_detect(NOMBRE_CEL, "Barcelona"))




# read mobility fluxes
mob <- readxl::read_xlsx("/home/marc/Documents/rodalies/data/mobilitat_20211020.xlsx")


# filter for trips w/in Barcelona


mob_bcn <- filter(mob,`Código área de destino` %in% cells_bcn$ID_GRUPO & `Código área de residencia` %in% cells_bcn$ID_GRUPO)



# people who do not stay (pct over total population)



# transform mobility table into desire lines
filt_mob <- mob_bcn  |> 
  filter(`Código área de destino` != `Código área de residencia`) |> 
  select(`Código área de residencia`, `Código área de destino`, `Flujo origen-destino (nº de personas)`)

desire_lines <- od2line(filt_mob, cells_bcn[,c("ID_GRUPO", "geometry")])


# filter desire lines: 3 biggest for each origin


desire_lines <- desire_lines |> 
  group_by(`Código área de residencia`) |> 
  slice_max(`Flujo origen-destino (nº de personas)`,n =3) |> 
  ungroup() |> 
  rename("viatgers" = "Flujo origen-destino (nº de personas)")

preferred_destination <- desire_lines |> 
  st_drop_geometry() |> 
  group_by(`Código área de destino`) |> 
  summarise("Times in top 3" = n())


cells_bcn <- left_join(cells_bcn, preferred_destination, by = c("ID_GRUPO" = "Código área de destino")) |> 
  mutate("Times in top 3" = ifelse(is.na(`Times in top 3`),0,`Times in top 3`))

tm_shape(cells_bcn) +
  tm_polygons(col = "Times in top 3",
              alpha = 0.4,
              style = "equal",
              palette = "magma",
              title = "#Top 3") +
  tm_shape(desire_lines) +
  tm_lines(col = "viatgers",
           lwd = 1.5,
           style = "quantile",
           n = 5,
           palette = "viridis") + 
  tm_compass() + 
  tm_scale_bar() +
  tm_credits("#Top 3 indica, per a cada barri, el número de barris que el\ntenen al seu top-3 de destinacions diàries",
             size = 10,
             bg.color = "white",
             bg.alpha = 0.6) +
  tm_layout(title = "Dia 06: Xarxa\nViatges diaris entre barris a Barcelona\n(20/10/2021)",
            bg.color = "antiquewhite2",
            fontfamily = "Roboto condensed",
            legend.format = list(text.separator = "-"))
