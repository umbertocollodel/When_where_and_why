# Comparison between WEO and Consensus foecasts 

# Prepare the data ----

load("../IEO_forecasts_material/intermediate_data/consensus/gdp_consensus_cleaned.RData")

consensus <- x %>% 
  rename_at(vars(matches("variable")), funs(str_replace(.,"variable","consesus"))) %>% 
  select(-country)

comparison_consensus <- final_sr$gdp %>%
  merge(consensus, by = c("country_code","year")) %>% 
  as_tibble()


# Table with list countries ----

