library(sf)
library(stars)
library(tmap)
library(tmaptools)
library(tidyverse)
library(stplanr)
library(sfnetworks)
library(viridis)
# read elevation raster - downloaded w/ ICGC QGIS plug-in and clipped to Barcelona

elevation <- stars::read_stars("./data/elevation_bcn.tif")




# transform elevation cut to contour lines

elevation_contour <- st_contour(elevation, contour_lines = TRUE, breaks = seq(0,550,50))


elevation_contour


# read bike lanes & cycling streets


bl <- st_read("https://opendata-ajuntament.barcelona.cat/data/dataset/e3497ea4-0bae-4093-94a7-119df50a8a74/resource/4608cf0c-2f11-4a25-891f-c5afc3af82c5/download")


cs <- st_read("https://opendata-ajuntament.barcelona.cat/data/dataset/ae147deb-1b95-4b25-aa3a-b57067f000bc/resource/458356ad-a695-42b7-a104-be672652fb55/download")


total <- rbind(bl, cs)


## break network at vertices


total_segments = rnet_breakup_vertices(total)

# transform into network

totalnet <- as_sfnetwork(total_segments)

# get node elevation

nodes <- totalnet |> 
  activate(nodes) |> 
  st_as_sf()



nodes <- st_transform(nodes,"EPSG:25831")

nodes <- st_extract(elevation, nodes)

nodes <- nodes |> 
  rename("elevation" = "elevation_bcn.tif") |> 
  mutate("id" = seq_along(1:nrow(nodes)))


# get edges as sf layer


edges <- totalnet |> 
  activate(edges) |> 
  st_as_sf() |> 
  st_transform("EPSG:25831")



# get elevation


edges <- edges |> 
  left_join(st_drop_geometry(nodes), by = c("from" = "id")) |> # get elevation of from and to nodes
  rename("elevation_from" = "elevation") |> 
  left_join(st_drop_geometry(nodes), by = c("to" = "id")) |> 
  rename("elevation_to" = "elevation") |> 
  mutate(dif = elevation_to-elevation_from,
         len = as.numeric(st_length(geometry)),
         slope = abs((dif/len)*100)) |>  # calculate slope ((elevation_to-elevation_from)/segment length)
  filter(!is.na(slope))

# get basemap

bm <- tmaptools::read_osm(elevation_contour)

palredgreen = c("#267300", "#70A800", "#FFAA00", "#E60000", "#A80000", "#730000") #color palette
tm_shape(bm) +
  tm_rgb(alpha = 0.5) +
  tm_shape(elevation_contour) +
  tm_lines(alpha = 0.6) +
  tm_shape(edges) +
  tm_lines(col = "slope",
           breaks = c(0,3,5,8,10,20,max(edges$slope)),
           palette = palredgreen,
           lwd = 1.25) +
  tm_compass(type = "arrow") +
  tm_scale_bar() +
  tm_layout(title = "Dia 7: Raster\nPendent mitjà dels carrils bici\n i carrers ciclables a Barcelona",
            title.size = 1.2,
            fontfamily = "Roboto condensed",
            legend.position = c("left","bottom"),
            legend.frame = "black",
            legend.bg.color = "white",
            legend.bg.alpha = 0.5,
            legend.format = list(text.separator = "-",
                                 fun=function(x) paste0(formatC(x, digits=2, format="f"), " %")))

