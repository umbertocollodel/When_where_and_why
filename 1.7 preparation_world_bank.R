##### Script to prepare World Bank forecast data ----
# Note: sample goes from 110 to 130 form the first to last year. Should be cleaned
# further to remove undesirable features.

original_wb <- read_xlsx("../IEO_forecasts_material/raw_data/world bank/GEP_forecast_vintages_06232020.xlsx",
          sheet = "Table_long") %>% 
  select(1:4) %>% 
  setNames(c("date_publication","country","year_forecasted","wb")) %>%
  filter(country != "Euro Area" & country != "United States" & country != "Japan") %>% 
  mutate(wb = as.numeric(wb)) %>% 
  mutate(year_publication = str_extract(date_publication,"\\d{4}")) %>% 
  group_by(country) %>% 
  filter(year_publication == year_forecasted | as.numeric(year_publication) == as.numeric(year_forecasted) -1)

forecasts <- original_wb %>% 
  split(.$year_forecasted) %>% 
  map(~ .x %>% select(-year_publication)) %>% 
  map(~ .x %>% spread(date_publication, wb)) %>% 
  map(~ .x %>% rename_at(vars(starts_with("January")), ~ paste0(.,"jan"))) %>% 
  map(~ .x %>% rename_at(vars(starts_with("January")), ~ str_remove(.,"^January"))) %>% 
  map(~ .x %>% rename_at(vars(starts_with("June")), ~ paste0(.,"jun"))) %>% 
  map(~ .x %>% rename_at(vars(starts_with("June")), funs(str_remove(.,"^June"))))


# Discard years for which no actual value:

forecasts <- forecasts %>% 
  discard(~ unique(.x$year_forecasted) > 2019)


# Order names of the columns:

for(i in 1:length(forecasts)){
  forecasts[[i]] <- forecasts[[i]][,order(names(forecasts[[i]]))]
}


# Naming similar to Zidong and bind together:


final_forecasts <-  forecasts %>% 
  map(~ if(length(names(.x)) == 6){
    .x %>% setNames(c(rev(paste0("wb",seq(4:1))),"country","year"))
  } else if(length(names(.x)) == 4){
    .x %>% setNames(c(rev(paste0("wb",seq(2:1))),"country","year"))
    })  %>%  
  bind_rows() %>% 
  mutate(country_code = countrycode(country,"country.name","imf")) %>%
  filter(complete.cases(country_code)) %>% 
  select(country_code, country, year, wb1, wb2, wb3, wb4) %>% 
  arrange(country,year)



# Export:

saveRDS(final_forecasts, file ="../IEO_forecasts_material/intermediate_data/world bank/gdp_wb_cleaned.rds")

cat(crayon::green("World Bank forecasts succesfully cleaned.\nExported intermediate data in directory:../IEO_forecasts_material/intermediate_data/world bank"))




