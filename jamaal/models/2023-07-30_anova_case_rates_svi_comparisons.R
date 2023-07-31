if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(tidyverse, sf, multcomp)

# Using the combined file, run a random forest model to predict the covid_cases
# based on the SVI scores (RPL_THEMES) and the HOLC grade.
#
input_df <- read_sf("jamaal/data/covid_redling_combined_files/baltimore_nyc_chicago_combined.gpkg")
input_df <- input_df %>%
  filter(zcta != "60666") %>%
  filter(!is.na(holc_grade)) %>%
  filter(!is.na(RPL_THEMES))

# Add a column representing high cases, where "high" is defined as being in the
# top quartile.
#


input_df <- input_df %>%
  mutate(high_cases = as.factor(ifelse(cases_per_1000 > quantile(cases_per_1000, 0.5), 1, 0)), 
         case_quantiles = ntile(x = cases_per_1000, n = 5), 
         high_quant = as.factor(if_else(case_quantiles == 5, 1, 0)), 
         nyc = if_else(city == "NYC", 1, 0), 
         holc_grade = as.factor(input_df$holc_grade))

aov_test <- aov(cases_per_1000 ~ holc_grade, data = input_df)
summary(aov_test)

#multiple anova comparisons
#source: https://www.r-bloggers.com/2021/07/how-to-perform-ancova-in-r/

aov_mult_comp <- glht(aov_test, linfct = mcp(holc_grade = "Tukey"))
summary(aov_mult_comp)


#aov test for SVI

avo_svi <- aov(RPL_THEMES ~ holc_grade, data = input_df)
svi_multcomp <- glht(avo_svi, linfct = mcp(holc_grade = "Tukey"))
summary(svi_multcomp)
