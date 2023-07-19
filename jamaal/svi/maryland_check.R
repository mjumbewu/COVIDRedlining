library(tidyverse)
library(tidycensus)
library(sf)


md_svi_cty <- st_read("jamaal/data/cdc_svi/SVI2020_MARYLAND_COUNTY.gdb/")

md_svi_cty_mjumbe <- build_svi_state(geography = "county", census_year = 2020, state = "MD")
# md_svi_cty_mjumbe <- md_svi_cty_mjumbe %>% 
#   mutate(state_fips = str_sub(GEOID, 1, 2)) %>% 
#   filter(state_fips == "24")

md_mjumbe_rpl <- md_svi_cty_mjumbe %>% 
  select(GEOID, starts_with("RPL"))

md_svi_cty_rpl <- md_svi_cty %>% 
  select(FIPS, starts_with("RPL"))

md_cty_combined <- md_svi_cty_rpl %>% 
  inner_join(md_mjumbe_rpl, by = c("FIPS" = "GEOID"), 
             suffix = c("_cdc", "_mjumbe")) %>% 
  select(FIPS, RPL_THEMES_cdc, RPL_THEMES_mjumbe)


sqrt(mean((md_cty_combined$RPL_THEMES_cdc - md_cty_combined$RPL_THEMES_mjumbe)^2))

library(Metrics)

rmse(md_cty_combined$RPL_THEMES_cdc, md_cty_combined$RPL_THEMES_mjumbe)

############################################################################
############################################################################
###                                                                      ###
###                          CHECK TRACTS IN MD                          ###
###                                                                      ###
############################################################################
############################################################################

md_svi_tracts <- st_read("jamaal/data/cdc_svi/SVI2020_MARYLAND_tract.gdb/")

md_svi_tracts_mjumbe <- build_svi_state(geography = "tract", census_year = 2020, state = "MD")


md_mjumbe_tract_rpl <- md_svi_tracts_mjumbe %>% 
  select(GEOID, starts_with("RPL"))

md_svi_tract_rpl <- md_svi_tracts %>% 
  select(FIPS, starts_with("RPL"))

md_cty_combined <- md_svi_tract_rpl %>% 
  inner_join(md_mjumbe_tract_rpl, by = c("FIPS" = "GEOID"), 
             suffix = c("_cdc", "_mjumbe")) %>% 
  select(FIPS, RPL_THEMES_cdc, RPL_THEMES_mjumbe) %>% 
  mutate(RPL_THEMES_mjumbe = if_else(is.na(RPL_THEMES_mjumbe), -999.0000, RPL_THEMES_mjumbe))

rmse(md_cty_combined$RPL_THEMES_cdc, md_cty_combined$RPL_THEMES_mjumbe)


md_svi_zcta_mjumbe <- build_svi_state(geography = "zcta", census_year = 2020)
