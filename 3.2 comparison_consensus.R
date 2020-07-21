# Comparison between WEO and Consensus foecasts 

# Prepare the data ----

load("../IEO_forecasts_material/intermediate_data/consensus/gdp_consensus_cleaned.RData")

consensus <- x %>% 
  rename_at(vars(matches("variable")), funs(str_replace(.,"variable","consensus"))) %>% 
  select(-country)

comparison_consensus <- final_sr$gdp %>%
  merge(consensus, by = c("country_code","year")) %>% 
  as_tibble()


# Table with list countries ----

comparison_consensus %>% 
  group_by(country_code) %>% 
  slice(1) %>% 
  mutate(country_name = countrycode(country_code, "imf","country.name")) %>% 
  arrange(country_name) %>% 
  mutate(group = case_when(group == "emerging_asia" ~ "Emerging Asia",
                           group == "emerging_europe" ~ "Emerging Europe",
                           group == "latin_america" ~ "Latin America",
                           group == "middle_east" ~ "Middle East",
                           T ~ "Advanced Economies")) %>% 
  ungroup() %>% 
  select(country_name, group) %>%
  stargazer(summary = F,
            rownames = F,
            out = "../IEO_forecasts_material/output/figures/comparison/consensus/list_countries.tex")


# Compare RMSE by country ----

comparison_consensus %>% 
  filter(forecaster == "Consensus (Mean)") %>% 
  group_by(country_code) %>%
  select(country_code, country, year, targety_first, variable1:variable4, consensus1:consensus4, group) %>% 
  summarise_at(vars(variable1:consensus4), funs(hydroGOF::rmse(.,targety_first))) %>% 
  mutate(country = countrycode(country_code,"imf","country.name")) %>% 
  mutate(ratio1 = variable1/consensus1 - 1,
         ratio2 = variable2/consensus2 - 1,
         ratio3 = variable3/consensus3 - 1,
         ratio4 = variable4/consensus4 - 1) %>% 
  select(country, matches("ratio")) 
  
  
