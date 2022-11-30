# DAY 25: TWO COLOURS.
# avg. income and Gini index bivariate map
# data downloaded from here https://www.ine.es/dynt3/inebase/index.htm?padre=7132
# Method by https://dominicroye.github.io/en/2021/bivariate-dasymetric-map/

library(sf)
library(tidyverse)
library(biscale)

# read and prepare data

income <- read_csv2("data/bcn_renta.csv")


income <- income |> 
  rename("ind" = "Indicadores de renta media y mediana") |> 
  filter(ind == "Renta neta media por hogar",
         Periodo == 2020,
         str_detect(Municipios,"Barcelona"),
         !is.na(Secciones)) |> 
  mutate(Total = as.numeric(gsub("\\.","",Total)),
         CUSEC = str_sub(Secciones,1,10)) |> 
  rename("renda" = "Total") |> 
  select(CUSEC,renda)


gini <- read_csv2("data/gini_bcn.csv")

gini <- gini |> 
  rename("ind" = "Índice de Gini y Distribución de la renta P80/P20") |> 
  filter(ind == "Índice de Gini",
         Periodo == 2020,
         str_detect(Municipios,"Barcelona"),
         !is.na(Secciones)) |> 
  mutate(Total = as.numeric(gsub(",","\\.",Total)),
         CUSEC = str_sub(Secciones,1,10)) |> 
  rename("gini" = "Total") |> 
  select(CUSEC,gini)



sc <- st_read("data/seccens/0301040100_SecCens_UNITATS_ADM.shp")

sc <- sc |> 
  mutate("CUSEC" = paste0("08019",as.character(DISTRICTE),as.character(SEC_CENS))) |> 
  select(CUSEC,geometry) |> 
  left_join(income, by = "CUSEC") |> 
  left_join(gini, by = "CUSEC")



# create bivariate classification
mapbivar <- bi_class(sc, gini, renda, style = "quantile", dim = 3) %>% 
  mutate(bi_class = ifelse(str_detect(bi_class, "NA"), NA, bi_class))


legend2 <- bi_legend(pal = "DkViolet",
                     dim = 3,
                     xlab = "+ desigualtat",
                     ylab = "+ renda",
                     size = 9)
ggplot(st_transform(mapbivar,"EPSG:4326")) + 
  geom_sf(aes(fill = bi_class), 
          colour = NA, 
          size = .1, 
          show.legend = FALSE) +
  geom_sf(colour = "gray2",
          fill = NA) +
  bi_scale_fill(pal = "DkViolet", 
                dim = 3, 
                na.value = "grey90") +
  annotation_custom(ggplotGrob(legend2),
                    xmin = 2.20, xmax = 2.25,
                    ymin = 41.3, ymax = 41.35) +
  labs(title = "Dia 25: Dos colors\nRenda i desigualtat per seccions censals",
       subtitle = "2020",
       x = "", y ="") +
  theme_void() +
  theme(plot.title = element_text(family = "Roboto Condensed")) +
  coord_sf(crs = 4326)
