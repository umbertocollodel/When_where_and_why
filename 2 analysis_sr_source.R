# Prepare the dataset: ----

# Load economic and geographical groups:

country_group <- read_xlsx("../IEO_forecasts_material/raw_data/country_group.xlsx") %>% 
  rename(country_code = ifscode) %>% 
  select(country_code, adv, eme, lidc)

load("../IEO_forecasts_material/intermediate_data/country_group_geography_clean.RData")


final_sr <- final %>% 
  map(~ .x %>% select(country_code,country,year,targety_first,targety_last,matches("variable[1-4]$"))) %>% 
  map(~ .x %>% merge(country_group, by = c("country_code"))) %>%
  map(~ .x %>% merge(x, by = c("country_code"),all.x = T)) %>% 
  map(~ .x %>% mutate(period = case_when(year <= "2001" ~ "1990:2001",
                                         year > "2001" & year <= "2011" ~ "2002:2011",
                                         TRUE ~ "2012:2018"))) %>% 
  map(~ .x %>% as.tibble())


# Source and produce output: ----


list.files() %>% 
  str_subset("^2.\\d") %>%
  walk(~ source(.x))