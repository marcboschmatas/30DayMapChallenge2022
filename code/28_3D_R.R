# Day 28: 3D Joyplot of elevations in BCN
# method from https://dieghernan.github.io/202205_Unknown-pleasures-R/

library(tidyverse)
library(sf)
library(terra)
library(ggridges)
library(add2ggplot)


el <- terra::rast("data/elevation_bcn.tif")

# get only 100 rows
factor <- round(nrow(el) / 90)

el_agg <- aggregate(el, factor)

# clean data and transform into data.frame

el_agg[el_agg < 0] <- 0
el_agg[is.na(el_agg)] <- 0

el_df <- as.data.frame(el_agg, xy = TRUE, na.rm = FALSE)

bcn <- st_read("data/adm_units/0301100100_UNITATS_ADM_POLIGONS.json")
bcn <- bcn[1,]


ggplot() +
  # Just for the scales, pass with NA arguments so it is not shown
  geom_sf(data = bcn, color = NA, fill = NA) +
  geom_ridgeline(
    data = el_df, aes(
      x = x, y = y,
      group = y,
      height = elevation_bcn
    ),
    scale = 5,
    fill = "antiquewhite2"
  ) +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(y = "",
       x = "",
       title = "Dia 28: 3D\nPerfil d'elevació de Barcelona") + 
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "antiquewhite2",
                                        color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(family = "Roboto condensed"))



ggsave("plots/28_3D.png")
