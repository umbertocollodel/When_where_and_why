# Prepare the datasets: ----

rm(list = ls())

# Load data:


paths=c("../IEO_forecasts_material/intermediate_data/rgdp_cleaned.RDS",
        "../IEO_forecasts_material/intermediate_data/inflation_cleaned.RDS")

name_vars <- c("growth",
               "inflation")


final_medium <- paths %>% 
  map(~ readRDS(.x)) %>% 
  map(~ .x %>% select(country_code, country, year, targety_first, 
                               variable7, variable8, variable9,
                               variable10, variable11, variable12))

names(final_medium) <- name_vars

# Load geo. group:

geo_group <- readRDS("../IEO_forecasts_material/intermediate_data/country_group_geography_clean.RDS")


# Source and produce output: ----


list.files() %>% 
  str_subset("^4.\\d") %>%
  walk(~ source(.x))