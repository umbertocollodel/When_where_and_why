# Prepare the datasets: ----

rm(list = ls())

# Load economic and geographical groups:

country_group <- read_xlsx("../When_where_and_why_material/raw_data/country_group.xlsx") %>% 
  rename(country_code = ifscode) %>% 
  select(country_code, adv, eme, lidc)

geo_group <- readRDS("../When_where_and_why_material/intermediate_data/country_group_geography_clean.RDS")

# Load dataset:

paths=c("../When_where_and_why_material/intermediate_data/rgdp_cleaned.RDS",
        "../When_where_and_why_material/intermediate_data/inflation_cleaned.RDS")

names=c("growth",
        "inflation")

final_sr <- paths %>%
  map(~ readRDS(.x)) %>% 
  map(~ .x %>% select(country_code,country,year,targety_first,targety_last,matches("variable[1-4]$"))) %>% 
  map(~ .x %>% merge(country_group, by = c("country_code"))) %>%
  map(~ .x %>% merge(geo_group, by = c("country_code"),all.x = T)) %>% 
  map(~ .x %>% as_tibble())


names(final_sr) <- names

# Years of recession:

years_recession <- final_sr[["growth"]] %>% 
  mutate(recession = case_when(targety_first <= 0 ~ 1,
                                      TRUE ~ 0)) %>% 
  select(country_code, year, recession)


# Source and produce output: ----


list.files() %>% 
  str_subset("^2.\\d") %>%
  walk(~ source(.x))