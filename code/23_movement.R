# Day 23 Movement. Bus/Underground trips per two hour window in BCN
library(tidytransit)
library(tidyverse)
library(sf)
library(tmap)
library(OpenStreetMap)
library(tmaptools)
library(lubridate)

# read TMB data, downloaded from here https://www.tmb.cat/en/about-tmb/tools-for-developers/data-gtfs

tmb <- tidytransit::read_gtfs("data/gtfs.zip")

# create sf objects out of feed

sf_tmb <- gtfs_as_sf(tmb)

# extract stops and stop times

stops <- sf_tmb$stops
stop_times <- sf_tmb$stop_times

# download basemap

bcn <- read_osm(stops,)


# filter out trips that depart at invalid times (00:00:00 or larger than 24) and generate stop time windows every two hours

stop_times <- stop_times %>% 
  filter((departure_time != hms("00:00:00")) & 
           (departure_time < hms("24:00:00"))) %>%
  mutate("time_window" = case_when(departure_time <= hms("06:00:00") ~ "Dia 23: Moviment\n0h - 6h",
                                   departure_time <= hms("08:00:00") ~ "Dia 23: Moviment\n6h - 8h",
                                   departure_time <= hms("10:00:00") ~ "Dia 23: Moviment\n8h - 10h",
                                   departure_time <= hms("12:00:00") ~ "Dia 23: Moviment\n10h - 12h",
                                   departure_time <= hms("14:00:00") ~ "Dia 23: Moviment\n12h - 14h",
                                   departure_time <= hms("16:00:00") ~ "Dia 23: Moviment\n14h - 16h",
                                   departure_time <= hms("18:00:00") ~ "Dia 23: Moviment\n16h - 18h",
                                   departure_time <= hms("20:00:00") ~ "Dia 23: Moviment\n18h - 20h",
                                   departure_time <= hms("22:00:00") ~ "Dia 23: Moviment\n20h - 22h",
                                   TRUE ~ "Dia 23: Moviment\n22h - 0h"))



# group by time window and stop code

grouped_stops <- stop_times %>%
  group_by(stop_id, time_window) %>%
  summarise("trips_per_station" = n())

# join geometry

grouped_stops <- grouped_stops %>%
  left_join(select(stops, stop_id, geometry), by = "stop_id") %>%
  st_as_sf()

# animated maps


fmap <- tm_shape(bcn) + 
  tm_rgb() + 
  tm_shape(grouped_stops) + 
  tm_dots(size = "trips_per_station", 
          col = "trips_per_station",
          title = "viatges",
          palette = "viridis",
          style = "pretty")  + 
  tm_add_legend(type = "fill") + 
  tm_compass() + 
  tm_scale_bar() + 
  tm_credits("Viatges en bus/metro per estació i franja horària \nFont: TMB",
             size = 1,
             bg.color = "white",
             bg.alpha = 0.6) + 
  tm_facets(along = "time_window",
            free.coords = FALSE) + 
  tm_layout(legend.outside = TRUE,
            fontfamily = "Roboto condensed")

tmap_animation(fmap, "./plots/23_movement.gif", delay = 60)
