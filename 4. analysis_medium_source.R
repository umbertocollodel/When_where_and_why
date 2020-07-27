load("../IEO_forecasts_material/intermediate_data/rgdp_cleaned.RData")

gdp_medium <- x %>% 
  select(country_code, country, year, targety_first, 
         variable7, variable8, variable9,
         variable10, variable11, variable12) 
