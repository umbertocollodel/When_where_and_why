######## Script to prepare a dataframe with imf country code and geographic group
# Note: advanced economies apart from Euro area not included.


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
  select(country_code, group) %>% 
  mutate(group = case_when(group == "africa" ~ "Africa",
                           group == "emerging_asia" ~ "Emerging Asia",
                           group == "europe" ~ "Europe",
                           group == "emerging_europe"~ "Emerging Europe",
                           group == "latin_america" ~ "Latin America",
                           T ~ "Middle East"))



saveRDS(country_group_geography, file = "..//IEO_forecasts_material/intermediate_data/country_group_geography_clean.RDS")
  
  
  


  
