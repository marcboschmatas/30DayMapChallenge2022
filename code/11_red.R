# Red: number of times each county in Catalonia has been in high wildfire risk since 2000

library(tidyverse)
library(sf)
library(tmap)
library(tmaptools)
library(RSocrata)
library(httr)
library(jsonlite)
library(osmdata)


# read risk data - obtained through a transparency petition (FOA equivalent)


risk <- readxl::read_xlsx("/home/marc/Documents/catalunya_interes_incendis/CopiadadesPLA_ALFA_2017_2022_SAIP.xlsx",
                          col_types = c("numeric", "date", "numeric", "text", "text", "numeric", "text"))

risk_grouped <- risk |> 
  filter(is.na(`MUNICIPI,C,75`)) |> # drop individual municipality observation - leads to duplicates
  distinct() |> 
  rename("comarca" = "COMARCA,C,40",
         "nivell" = "NIVELL,C,10") |> 
  group_by(comarca) |> 
  summarise(total = n(),
            high_risk = length(nivell[nivell >= 2])) |> 
  ungroup() |> 
  mutate("high_risk_pct" = high_risk/total*100,
         "comarca" = str_to_title(comarca),
         "comarca" = ifelse(comarca == "Conca De Barberà", "Conca de Barberà", comarca),
         "comarca" = ifelse(comarca == "Ribera D'ebre", "Ribera d'Ebre", comarca))


# read county layer - downloaded from ICGC QGIS Plug-in


comarques <- st_read("data/divisions-administratives_20221026_092107/divisions-administratives-comarques-5000.shp")

# join risk data


comarques_risk <- left_join(comarques, risk_grouped, by = c("NOMCOMAR" = "comarca"))

comarques_risk$high_risk_pct <- ifelse(is.na(comarques_risk$high_risk_pct),0,comarques_risk$high_risk_pct)

tm_shape(comarques_risk) + 
  tm_polygons(col = "high_risk_pct",
              style = "equal",
              palette = "YlOrRd",
              title = "% d'observacions de risc alt",
              legend.format=list(text.separator = "-",
                                 fun=function(x) paste0(formatC(x, digits=2, format="f"), " %"))) +
  tm_compass() +
  tm_scale_bar() +
  tm_credits("~2050 observacions per comarca") +
  tm_layout("Dia 11: Vermell\nFreqüència d'observacions d'alt risc\nd'incendi forestal (2000-2022)",
            title.position = c("right","top"),
            bg.color = "antiquewhite2",
            fontfamily = "Roboto condensed")
