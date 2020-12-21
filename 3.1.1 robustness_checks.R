# First robustness check: are year-ahead WEO Fall forecasts different from WEO current-year January forecasts? -----

# Fall-spring data

load("../When_where_and_why_material/intermediate_data/rgdp_cleaned.RData")
rgdp_fall_spring <- x

load("../When_where_and_why_material/intermediate_data/rgdp_jan_update.RData")
rgdp_jan <- x %>% select(-variable2)

rgdp_fall_spring_clean1 <- rgdp_fall_spring %>% 
  select(country_code, year, variable3)


merge(rgdp_jan,rgdp_fall_spring_clean1) %>% 
  as_tibble() %>% 
  mutate(no_change = case_when(variable1 != variable3 ~ 0,
                               T ~ 1)) %>%
  summarise(mean_no_change = mean(as.numeric(no_change), na.rm = T))


# Second robustness check: are current-year WEO Spring forecasts different from WEO current-year July forecasts? -----


update <- readRDS("../When_where_and_why_material/intermediate_data/rgdp_update_cleaned.RDS")

update_july_clean <- update %>% 
  select(country_code, year, variable1)

load("../When_where_and_why_material/intermediate_data/rgdp_cleaned.RData")
rgdp_fall_spring <- x

rgdp_fall_spring_clean2 <- rgdp_fall_spring %>% 
  select(country_code, year, variable2)

merge(update_july_clean,rgdp_fall_spring_clean2) %>% 
  as_tibble() %>% 
  mutate(no_change = case_when(variable1 != variable2 ~ 0,
                               T ~ 1)) %>%
  summarise(mean_no_change = mean(as.numeric(no_change), na.rm = T))

