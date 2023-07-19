###########################################################################
###########################################################################
###                                                                     ###
###              COMBINING SVI AND REDLINING AT ZCTA LEVEL              ###
###                                                                     ###
###########################################################################
###########################################################################

if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(tidyverse, sf, tmap, readxl, tigris)

source("jamaal/svi/svi_func2_state.R")

nyc_covid <- read_csv("jamaal/data/latrice_data/NYC_COVID_Data.csv", col_types = cols("MODIFIED_ZCTA" = col_character()))

nyc_svi <- build_svi_state(geography = "zcta", census_year = 2020) %>% 
  st_as_sf()

ny_State = states(year = 2020) %>% 
  filter(NAME == "New York")


nyc_covid_svi <- nyc_covid %>% 
  left_join(nyc_svi, by = c("MODIFIED_ZCTA" = "GEOID")) %>% 
  st_as_sf()

#################################################################
##             Brining In Redlining and Joining...             ##
#################################################################

redlining <- st_read("jamaal/data/redlining_u_richmond/fullshpfile/shapefile/holc_ad_data.shp") %>% 
  st_make_valid()

redlining <- redlining %>% 
  st_transform(crs = st_crs(nyc_covid_svi)) 

nyc_covid_svi <- nyc_covid_svi %>% 
  st_join(redlining, left = TRUE, largest = TRUE)

st_write(nyc_covid_svi, "jamaal/data/covid_redling_combined_files/nyc_covid_redlining.shp")

#nyc map check---------
nyc_red1 <- tm_shape(nyc_covid_svi) +
  tm_fill(col = "holc_grade", n = 4, style = "cat", palette = "viridis") +
  tm_borders(col = "gray")

nyc_svi1 <- tm_shape(nyc_covid_svi) +
  tm_fill(col = "RPL_THEMES", palette = "viridis") +
  tm_borders(col = "gray")

tmap_mode("view")
tmap_arrange(nyc_red1, nyc_svi1)

# prepping baltimore covid---------
baltimore_covid <- read_excel("jamaal/data/latrice_data/NEW_MD_COVID-19_-_Cases_by_ZIP_Code.xlsx", 
                              skip = 1) %>% 
  select(ZIP_CODE, md_total_cases = total04_27_2023)

baltimore_covid$ZIP_CODE <- as.character(baltimore_covid$ZIP_CODE)

baltimore_svi_covid <- nyc_svi %>% 
  inner_join(baltimore_covid, by = c("GEOID" = "ZIP_CODE"))

baltimore_city <- places(state = "MD")
baltimore_city <- baltimore_city %>% 
  filter(NAMELSAD == "Baltimore city")

baltimore_svi_covid <- baltimore_svi_covid[baltimore_city, ]


baltimore_svi_covid <- baltimore_svi_covid %>% 
  st_join(redlining, left = TRUE, largest = TRUE)

baltimore_svi_covid <- st_make_valid(baltimore_svi_covid)

st_write(baltimore_svi_covid, "jamaal/data/covid_redling_combined_files/baltimore_covid_cases_redlining.shp")
