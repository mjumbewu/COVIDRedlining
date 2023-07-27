#!/usr/bin/env Rscript


###########################################################################
###########################################################################
###                                                                     ###
###              COMBINING SVI AND REDLINING AT ZCTA LEVEL              ###
###                                                                     ###
###########################################################################
###########################################################################

if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(tidyverse, sf, tmap, readxl, tigris)
options(tigris_use_cache = TRUE)

source(here::here("jamaal/svi/svi_func2_state.R"))

nyc_covid <- read_csv(here::here("jamaal/data/latrice_data/NYC_COVID_Data.csv"), col_types = cols("MODIFIED_ZCTA" = col_character()))

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

if (file.exists(here::here("jamaal/data/redlining_u_richmond/fullshpfile/shapefile/holc_ad_data.shp"))) {

redlining <- st_read(here::here("jamaal/data/redlining_u_richmond/fullshpfile/shapefile/holc_ad_data.shp")) %>%
  st_make_valid()
} else {
  redlining <- st_read(here::here("jamaal/data/redlining_u_richmond/fullshpfile/fullshpfile/shapefile/holc_ad_data.shp")) %>%
    st_make_valid()
}

redlining <- redlining %>%
  st_transform(crs = st_crs(nyc_covid_svi))

nyc_covid_svi <- nyc_covid_svi %>%
  st_join(redlining, left = TRUE, largest = TRUE)

nyc_covid_svi <- nyc_covid_svi %>% 
  filter(!is.na(state)) %>% 
  st_make_valid()

st_write(nyc_covid_svi, here::here("jamaal/data/covid_redling_combined_files/full_svi_vars/nyc_covid_redlining_full_svi.shp"), 
         append = FALSE)

#breaking down to essential columns--------

nyc_covid_svi <- nyc_covid_svi %>%
  select(zcta = MODIFIED_ZCTA, covid_cases = COVID_CONFIRMED_CASE_COUNT, covid_deaths = COVID_DEATH_COUNT, 
         RPL_THEMES, holc_grade, E_TOTPOP)

nyc_covid_svi <- nyc_covid_svi %>%
  st_make_valid() %>% 
  mutate(city = "NYC")

nyc_covid_svi <- nyc_covid_svi %>% 
  select(zcta, covid_cases, covid_deaths, RPL_THEMES, holc_grade, E_TOTPOP, city)

st_write(nyc_covid_svi, here::here("jamaal/data/covid_redling_combined_files/nyc_covid_redlining.shp"), append = FALSE)



# prepping baltimore covid---------
baltimore_covid <- read_excel(here::here("jamaal/data/latrice_data/NEW_MD_COVID-19_-_Cases_by_ZIP_Code.xlsx"),
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


st_write(baltimore_svi_covid, here::here("jamaal/data/covid_redling_combined_files/full_svi_vars/baltimore_covid_redlining_full.shp"), 
         append = FALSE)

baltimore_svi_covid <- baltimore_svi_covid %>%
  select(GEOID, covid_cases = md_total_cases, RPL_THEMES, holc_grade, E_TOTPOP) %>%
  mutate(covid_deaths = NA)

baltimore_svi_covid <- baltimore_svi_covid %>%
  select(zcta = GEOID, covid_cases, covid_deaths, RPL_THEMES, holc_grade, E_TOTPOP) %>% 
  mutate(city = "Baltimore")

st_write(baltimore_svi_covid, here::here("jamaal/data/covid_redling_combined_files/baltimore_covid_cases_redlining.shp"), 
         append = FALSE)

# prepping chicago covid---------


#################################################################
##                        Chicago COVID                        ##
#################################################################

chicago <- read_xlsx(path = here::here("jamaal/data/latrice_data/Chicago_COVID-19_Cases__Tests__and_Deaths_by_ZIP_Code-2.xlsx"), 
                     skip = 1)

chicago <- chicago %>% 
  mutate(week_start = mdy(`Week Start`), 
         week_end = mdy(`Week End`))

chi_last_week <- chicago %>% 
  filter(week_start == '2023-04-30')

#drop unknown zip codes and subset columns

chi_last_week <- chi_last_week %>% 
  filter(`ZIP Code` != "Unknown")


chi_last_week <- chi_last_week %>% 
  select(zcta = `ZIP Code`, covid_cases = `Cases - Cumulative`, covid_deaths = `Deaths - Cumulative`)

chi_covid_svi <- chi_last_week %>% 
  left_join(nyc_svi, by = c("zcta" = "GEOID")) %>% 
  select(zcta, covid_cases, covid_deaths, RPL_THEMES, geometry, E_TOTPOP) %>% 
  st_as_sf()

chi_covid_svi_holc <- chi_covid_svi %>% 
  st_join(redlining, left = TRUE, largest = TRUE)

st_write(chi_covid_svi_holc, here::here("jamaal/data/covid_redling_combined_files/full_svi_vars/chi_covid_redlining_full.shp"),
         append = FALSE)

chi_covid_svi_holc <- chi_covid_svi_holc %>% 
  select(zcta:RPL_THEMES, holc_grade, E_TOTPOP) %>% 
  mutate(city = "Chicago")

chi_covid_svi_holc <- chi_covid_svi_holc %>% 
  select(zcta:RPL_THEMES, holc_grade, E_TOTPOP, city)

#################################################################
##                    Combine NYC and Bmore                    ##
#################################################################

nyc_bmore_chi <- rbind(nyc_covid_svi, baltimore_svi_covid, chi_covid_svi_holc)
nyc_bmore_chi <- nyc_bmore_chi %>% 
  mutate(cases_per_1000 = (covid_cases/E_TOTPOP)*1000,
         deaths_per_1000 = (covid_deaths/E_TOTPOP)*1000) %>% 
  filter(zcta != "60666")


st_geometry(nyc_bmore_chi) <- "MULTIPOLYGON"

st_write(nyc_bmore_chi, here::here("jamaal/data/covid_redling_combined_files/baltimore_nyc_chicago_combined.gpkg"), append = FALSE)


