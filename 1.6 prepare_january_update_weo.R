# Script to clean the January update of WEO, instrumental for comparison
# with World Bank data:

path ="../IEO_forecasts_material/raw_data/weo_january_update.xlsx"

sheets_name <- getSheetNames(path)

sheets_year <- getSheetNames(path) %>%
  str_extract("\\d{4}")

forecasts <- sheets_name %>% 
  map(~ read_xlsx(path,sheet = .x)) %>%
  map(~ .x %>% slice(1:which(Country == "Zimbabwe"))) %>% 
  map(~ .x %>% gather("year_forecasted","variable",8:ncol(.)))


forecasts <- forecasts %>% 
  map(~ .x %>% mutate(Series_code = str_extract(Series_code, "\\d{3}"))) %>%  
  map(~ .x %>% select(Series_code, year_forecasted, variable)) %>% 
  map2(sheets_name, ~ .x %>% mutate(date_publication = .y)) %>% 
  map2(sheets_year, ~ .x %>% mutate(year_publication = .y)) %>% 
  map(~ .x %>% filter(year_forecasted >= year_publication & year_forecasted <= as.character(as.numeric(year_publication) + 1))) %>%
  map(~ .x %>% filter(complete.cases(Series_code))) %>% 
  bind_rows() %>%
  group_split(year_forecasted) %>% 
  map(~ .x %>% select(-year_publication)) %>% 
  map(~ .x %>% spread(date_publication,variable))


# Order names of the columns:

for(i in 1:length(forecasts)){
  forecasts[[i]] <- forecasts[[i]][,order(names(forecasts[[i]]))]
}

# Discard years for which no actual value and ones before start comparison:


forecasts <- forecasts %>% 
  discard(~ unique(.x$year_forecasted) > 2019 | unique(.x$year_forecasted) < 2010)

# Revert column order for first dataframe: (later on improve this part of the code, not elegant)

forecasts[[1]] <- forecasts[[1]] %>% 
  select(Series_code, jan2009, everything())


# Naming similar to Zidong and bind together:


final_forecasts <-  forecasts %>% 
  map(~ if(length(names(.x)) == 4){
    .x %>% setNames(c("country_code", rev(paste0("variable",seq(2:1))),"year"))
  })  %>%  
  bind_rows() %>% 
  mutate(country = countrycode(country_code,"imf","country.name")) %>% 
  filter(complete.cases(country)) %>% 
  select(country_code, country, year, variable1, variable2) %>% 
  arrange(country)


rio::export(final_forecasts, file = "../IEO_forecasts_material/intermediate_data/rgdp_jan_update.RData")

