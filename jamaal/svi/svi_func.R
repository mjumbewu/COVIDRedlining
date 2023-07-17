if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(tidyverse, tidycensus, sf)

#' Get basic demographic data from the ACS.
#'
#' @param geography The geographic unit to get data for.
#' @param census_year The year of the ACS data to get.
#'
#' @return A simple features object with the basic demographic data.
#'
#' @importFrom tidycensus get_acs
get_basic_demo <- function(geography, census_year) {
  basic_demo <- get_acs(
    geography = geography,
    variables = c(
      E_TOTPOP = "S0601_C01_001E", # Total Population
      E_HU     = "DP04_0001E",     # Housing Units
      E_HH     = "DP02_0001E"),    # Households
    year = census_year,
    output = "wide",
    geometry = TRUE)

  # Find all rows where the E_TOPOP is not NA
  basic_demo <- basic_demo %>% filter(!is.na(E_TOTPOP))

  basic_demo %>%
    select(c("GEOID", "NAME", starts_with("E_"), "geometry"))
}

#' Build a dataframe with the RPL1 socioeconomic status variables.
#'
#' @param geography The geographic unit to get data for.
#' @param census_year The year of the ACS data to get.
#' @param basic_demo The basic demographic data.
#'
#' @return A dataframe with the RPL1 variables.
#'
#' @importFrom tidycensus get_acs
#' @importFrom dplyr mutate
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom dplyr as_tibble
#' @importFrom dplyr percent_rank
#' @importFrom dplyr %>%
build_rpl1 <- function(geography, census_year, basic_demo) {
  rpl1 <- get_acs(
    geography = geography,
    variables = c(
      E_POV150       = "S1701_C01_040E",  # <150% poverty
      E_UNEMP        = "DP03_0005E",      # Civilians age 16+ unemployed
      E_HBURD_20k    = "S2503_C01_028E",  # Housing cost burdened <20k
      E_HBURD_20_35k = "S2503_C01_032E",  # Housing cost burdened 20-35k
      E_HBURD_35_49k = "S2503_C01_036E",  # Housing cost burdened 35-49k
      E_HBURD_50_75k = "S2503_C01_040E",  # Housing cost burdened 50-75k
      E_NOHSDP       = "B06009_002E",     # No high school diploma
      E_UNINSUR      = "S2701_C04_001E"), # No health insurance
    year = census_year,
    output = "wide")

  # Total Counts
  rpl1 <- rpl1 %>%
    select(c("GEOID", starts_with("E_"))) %>%
    mutate(E_HBURD = (E_HBURD_20k +
                      E_HBURD_20_35k +
                      E_HBURD_35_49k +
                      E_HBURD_50_75k)) %>%
    select(-c(starts_with("E_HBURD_")))

  # Percentages
  rpl1 <- rpl1 %>%
    left_join(
      basic_demo %>% select(GEOID, E_TOTPOP), by = "GEOID") %>%
    mutate(
      EP_POV150 = (E_POV150 / E_TOTPOP) * 100,
      EP_UNEMP = (E_UNEMP / E_TOTPOP) * 100,
      EP_HBURD = (E_HBURD / E_TOTPOP) * 100,

      # docs say to use S0601_C01_033E, but don't call it out
      # in the data collection phase.
      EP_NOHSDP = (E_NOHSDP / E_TOTPOP) * 100,

      # docs say to use S2701_C05_001E, but don't call it out
      # in the data collection phase.
      EP_UNINSUR = (E_UNINSUR / E_TOTPOP) * 100) %>%
    as_tibble() %>%
    select(-c("E_TOTPOP", "geometry"))

  # Percentile Ranks
  rpl1 <- rpl1 %>%
    mutate(
      EPL_POV150 = percent_rank(EP_POV150),
      EPL_UNEMP = percent_rank(EP_UNEMP),
      EPL_HBURD = percent_rank(EP_HBURD),
      EPL_NOHSDP = percent_rank(EP_NOHSDP),
      EPL_UNINSUR = percent_rank(EP_UNINSUR))

  # Final RPL1 Calculations
  rpl1 <- rpl1 %>%
    mutate(
      SPL_THEME1 = (EPL_POV150 +
                    EPL_UNEMP +
                    EPL_HBURD +
                    EPL_NOHSDP +
                    EPL_UNINSUR)) %>%
    mutate(RPL_THEME1 = percent_rank(SPL_THEME1))

  rpl1
}

#' Build a dataframe with the RPL2 household charachteristic variables.
#'
#' @param geography The geographic unit to get data for.
#' @param census_year The year of the ACS data to get.
#' @param basic_demo The basic demographic data.
#'
#' @return A dataframe with the RPL2 variables.
#'
#' @importFrom tidycensus get_acs
#' @importFrom dplyr mutate
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom dplyr as_tibble
#' @importFrom dplyr percent_rank
#' @importFrom dplyr %>%
build_rpl2 <- function(geography, census_year, basic_demo) {
  rpl2 <- get_acs(
    geography = geography,
    variables = c(
      E_AGE65      = "S0101_C01_030E", # Age 65+
      EP_AGE65     = "S0101_C02_030E", # % age 65+
      E_AGE17      = "B09001_001E",    # Age 17 and below
      E_DISABL     = "DP02_0072E",     # With disability
      EP_DISABL    = "DP02_0072PE",    # % with disability
      E_SNGPNT_mom = "B11012_015E",    # Single mom
      E_SNGPNT_dad = "B11012_010E",    # Single dad
      E_AGE5       = "B16005_001E"),   # Age 5+
    year = census_year,
    output = "wide")

  limited_english <- get_acs(
    geography = geography,
    variables = c("B16005_007", "B16005_008", "B16005_012", "B16005_013",
                  "B16005_017", "B16005_018", "B16005_022", "B16005_023",
                  "B16005_029", "B16005_030", "B16005_034", "B16005_035",
                  "B16005_039", "B16005_040", "B16005_044", "B16005_045"),
    year = census_year,
    output = "wide")

  rpl2 <- rpl2 %>%
    select(c("GEOID", starts_with("E_"), starts_with("EP_"))) %>%
    left_join(basic_demo %>% select(GEOID, E_TOTPOP, E_HH), by = "GEOID") %>%
    mutate(
      EP_AGE17 = (E_AGE17 / E_TOTPOP) * 100,
      EP_SNGPNT = ((E_SNGPNT_dad + E_SNGPNT_mom) / E_HH) * 100) %>%
    as_tibble() %>%
    select(-c("E_TOTPOP", "E_HH", "geometry"))

  limited_english <- limited_english %>%
    mutate(E_LIMENG = B16005_007E + B16005_008E +
            B16005_012E + B16005_013E +
            B16005_017E + B16005_018E +
            B16005_022E + B16005_023E +
            B16005_029E + B16005_030E +
            B16005_034E + B16005_035E +
            B16005_039E + B16005_040E +
            B16005_044E + B16005_045E) %>%
    select(GEOID, E_LIMENG)

  rpl2 <- rpl2 %>%
    left_join(limited_english) %>%
    mutate(EP_LIMENG = (E_LIMENG / E_AGE5) * 100)

  rpl2 <- rpl2 %>%
    mutate(
      EPL_AGE65 = percent_rank(EP_AGE65),
      EPL_AGE17 = percent_rank(EP_AGE17),
      EPL_DISAB = percent_rank(EP_DISABL),
      EPL_SINGPNT = percent_rank(EP_SNGPNT),
      EPL_LIMENG = percent_rank(EP_LIMENG),
      SPL_THEME2 = EPL_AGE65 + EPL_AGE17 + EPL_DISAB + EPL_SINGPNT + EPL_LIMENG,
      RPL_THEME2 = percent_rank(SPL_THEME2))

  rpl2
}

#' Build a dataframe with the RPL3 Racial/Ethnic Minority Status variables
#'
#' @param geography The geographic unit to get data for.
#' @param census_year The year of the ACS data to get.
#' @param basic_demo The basic demographic data.
#'
#' @return A dataframe with the RPL3 variables.
#'
#' @importFrom tidycensus get_acs
#' @importFrom dplyr mutate
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom dplyr as_tibble
#' @importFrom dplyr percent_rank
#' @importFrom dplyr %>%
build_rpl3 <- function(geography, census_year, basic_demo) {
  rpl3 <- get_acs(
    geography = geography,
    variables = c("DP05_0071", "DP05_0078", "DP05_0079", "DP05_0080",
                  "DP05_0081", "DP05_0082", "DP05_0083"),
    year = census_year,
    output = "wide")

  # Total Counts
  rpl3 <- rpl3 %>%
    mutate(E_MINRTY = DP05_0071E + DP05_0078E + DP05_0079E + DP05_0080E +
                      DP05_0081E + DP05_0082E + DP05_0083E) %>%
    select(GEOID, E_MINRTY)

  # Percentages
  rpl3 <- rpl3 %>%
    left_join(basic_demo %>% select(GEOID, E_TOTPOP), by = "GEOID") %>%
    mutate(EP_MINRTY = (E_MINRTY / E_TOTPOP) * 100) %>%
    as_tibble() %>%
    select(-c("E_TOTPOP", "geometry"))

  # Percentile Ranks
  rpl3 <- rpl3 %>%
    mutate(EPL_MINRTY = percent_rank(EP_MINRTY))

  # Final RPL3 Calculations
  rpl3 <- rpl3 %>%
    mutate(SPL_THEME3 = (EPL_MINRTY))

  rpl3 <- rpl3 %>%
    mutate(RPL_THEME3 = percent_rank(SPL_THEME3))

  rpl3
}

#' Build a dataframe with the RPL4 Housing and Transportation Cost Burden variables
#'
#' @param geography The geographic unit to get data for.
#' @param census_year The year of the ACS data to get.
#' @param basic_demo The basic demographic data.
#'
#' @return A dataframe with the RPL4 variables.
#'
#' @importFrom tidycensus get_acs
#' @importFrom dplyr mutate
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom dplyr as_tibble
#' @importFrom dplyr percent_rank
#' @importFrom dplyr %>%
build_rpl4 <- function(geography, census_year, basic_demo) {
  rpl4 <- get_acs(
    geography = geography,
    variables = c(
      E_MUNIT_10_19 = "DP04_0012E",  # Housing units in multi-unit (10-19)
      E_MUNIT_20    = "DP04_0013E",  # Housing units in multi-unit (20+)
      EP_MOBILE     = "DP04_0014PE", # Percent of housing units in mobile homes
      E_MOBILE      = "DP04_0014E",  # Housing units in mobile homes
      E_CROWD_1     = "DP04_0078E",  # Housing units, 1.01-1.5 persons/room
      E_CROWD_2     = "DP04_0079E",  # Housing units, >1.5 persons/room
      EP_NOVEH      = "DP04_0058PE", # Percent of housing units with no vehicle
      E_NOVEH       = "DP04_0058E",  # Housing units with no vehicle
      E_GROUPQ      = "B26001_001E", # Total group quarters population
      E_UNIT        = "DP04_0002E"), # Total housing units
    year = census_year,
    output = "wide")

  rpl4 <- rpl4 %>%
    select(c("GEOID", starts_with("E_"), starts_with("EP_"))) %>%
    left_join(basic_demo %>% select(GEOID, E_TOTPOP, E_HU), by = "GEOID") %>%
    mutate(EP_MUNIT  = ((E_MUNIT_10_19 + E_MUNIT_20) / E_HU) * 100,
          EP_CROWD   = ((E_CROWD_1 + E_CROWD_2) / E_UNIT) * 100,
          EP_GROUPQ  = (E_GROUPQ / E_TOTPOP) * 100) %>%
    as_tibble() %>%
    select(-c("E_TOTPOP", "E_HU", "geometry"))

  rpl4 <- rpl4 %>%
    mutate(EPL_MUNIT = percent_rank(EP_MUNIT),
          EPL_MOBILE = percent_rank(EP_MOBILE),
          EPL_CROWD  = percent_rank(EP_CROWD),
          EPL_NOVEH  = percent_rank(EP_NOVEH),
          EPL_GROUPQ = percent_rank(EP_GROUPQ),
          SPL_THEME4 = (EPL_MUNIT + EPL_MOBILE + EPL_CROWD +
                        EPL_NOVEH + EPL_GROUPQ),
          RPL_THEME4 = percent_rank(SPL_THEME4))

  rpl4
}

#' Build a dataframe with all RPL variables, and the final SVI.
#'
#' @param geography The geographic unit to get data for.
#' @param census_year The year of the ACS data to get.
#'
#' @return A dataframe with all RPL variables, and the final SVI.
#'
#' @importFrom tidycensus get_acs
#' @importFrom dplyr mutate
#' @importFrom dplyr left_join
#' @importFrom dplyr %>%
build_svi <- function(geography, census_year) {

  ###########################################################################
  ###########################################################################
  ###                                                                     ###
  ###                           DATA COLLECTION                           ###
  ###                                                                     ###
  ###########################################################################
  ###########################################################################

  basic_demo <- get_basic_demo(geography, census_year)

  rpl1 <- build_rpl1(geography, census_year, basic_demo)
  rpl2 <- build_rpl2(geography, census_year, basic_demo)
  rpl3 <- build_rpl3(geography, census_year, basic_demo)
  rpl4 <- build_rpl4(geography, census_year, basic_demo)

  ###########################################################################
  ###########################################################################
  ###                                                                     ###
  ###                           DATA COMBINATION                          ###
  ###                                                                     ###
  ###########################################################################
  ###########################################################################

  svi <- rpl1 %>%
    left_join(rpl2, by = "GEOID") %>%
    left_join(rpl3, by = "GEOID") %>%
    left_join(rpl4, by = "GEOID") %>%
    left_join(basic_demo, by = "GEOID") %>%
    mutate(SPL_THEMES = (SPL_THEME1 + SPL_THEME2 +
                        SPL_THEME3 + SPL_THEME4)) %>%
    mutate(RPL_THEMES = percent_rank(SPL_THEMES))

  svi
}
