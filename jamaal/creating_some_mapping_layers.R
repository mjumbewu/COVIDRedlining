#grabbing some additional layers for mapping
library(tigris)
library(sf)
library(tidycensus)
library(tidyverse)


bmore_streets <- roads(state = "MD", county = "Baltimore City")
bmore <- places(state = "MD") %>% 
  filter(NAMELSAD == "Baltimore city")

st_write(bmore_streets, here::here("jamaal/data/for_mapping/bmore_layers_tigris.gpkg"), layer = "streets")
st_write(bmore, here::here("jamaal/data/for_mapping/bmore_layers_tigris.gpkg"), layer = "city_boundary", append = TRUE)


chitown <- places(state = "IL") %>% 
  filter(NAMELSAD == "Chicago city")

chitown_streets <- roads(state = "IL", count = "Cook")

st_write(chitown, here::here("jamaal/data/for_mapping/chitown_layers.gpkg"), layer = "chi_boundary")
st_write(chitown_streets, here::here("jamaal/data/for_mapping/chitown_layers.gpkg"), 
         layer = "chi_streets", append = TRUE)

