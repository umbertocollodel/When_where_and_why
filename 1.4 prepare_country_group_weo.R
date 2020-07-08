# Prepare country group:

groups <- list.files("../IEO_forecasts_material/raw_data/country_group_geography/") %>% 
  str_remove(".xlsx")

country_group_geography <- list.files("../IEO_forecasts_material/raw_data/country_group_geography/") %>% 
  map_chr(~ paste0("~/Dropbox/IMF/IEO_forecasts_material/raw_data/country_group_geography/",.x)) %>% 
  map(~ read_excel(.x)) %>% 
  map(~ .x %>% rename(country_code= 1))

names(country_group_geography) <- groups

# Export:

country_group_geography <- country_group_geography %>% 
  bind_rows(.id = "group") %>% 
  select(country_code, group) 


rio::export(country_group_geography, "..//IEO_forecasts_material/intermediate_data/country_group_geography_clean.RData")
  
  
  


  
