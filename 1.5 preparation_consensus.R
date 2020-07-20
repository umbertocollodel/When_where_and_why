# Script to prepare consensus forecast data ----
# Note: for some countries and survey dates, forecast are for the previous and current year
# instead of current and year-ahead

  
  path = "../IEO_forecasts_material/raw_data/consensus/gdp_2008_2019.xlsx"
  
  
  sheets_name <- getSheetNames(path)
  sheets_year <- getSheetNames(path) %>%
    str_extract("\\d{4}")
  

  forecasts <- sheets_name %>% 
      map(~ read_xlsx(path,sheet = .x)) %>%
      map(~ .x %>% filter(Country != "Euro Area")) %>%
      map(~ .x %>% split(.$`Forecast Length`)) %>% 
      modify_depth(2, ~ .x %>% remove_empty("cols")) %>% 
      modify_depth(2, ~ .x %>% filter(complete.cases(variable)))
  }
  
  forecasts <- forecasts %>% 
    map(~ .x %>% mutate(Series_code = str_extract(Series_code, "\\d{3}"))) %>%  
    map(~ .x %>% select(Series_code, year_forecasted, variable)) %>% 
    map2(sheets_name, ~ .x %>% mutate(date_publication = .y)) %>% 
    map2(sheets_year, ~ .x %>% mutate(year_publication = .y)) %>% 
    map(~ .x %>% filter(year_forecasted >= year_publication & year_forecasted <= as.character(as.numeric(year_publication) + 5))) %>%
    map(~ .x %>% filter(complete.cases(Series_code))) %>% 
    bind_rows() %>% 
    group_split(year_forecasted) %>% 
    map(~ .x %>% select(-year_publication)) %>% 
    map(~ .x %>% spread(date_publication,variable)) %>% 
    map(~ .x %>% rename_at(vars(starts_with("apr")), ~ paste0(.,"apr"))) %>% 
    map(~ .x %>% rename_at(vars(starts_with("apr")), ~ str_remove(.,"^apr"))) %>% 
    map(~ .x %>% rename_at(vars(starts_with("oct")), ~ paste0(.,"oct"))) %>% 
    map(~ .x %>% rename_at(vars(starts_with("oct")), funs(str_remove(.,"^oct"))))
  
  # Order names of the columns:
  
  for(i in 1:length(forecasts)){
    forecasts[[i]] <- forecasts[[i]][,order(names(forecasts[[i]]))]
  }
  
  # Discard years for which no actual value:
  
  
  forecasts <- forecasts %>% 
    discard(~ unique(.x$year_forecasted) > 2019)
  
  
  # Naming similar to Zidong and bind together:
  
  
  final_forecasts <-  forecasts %>% 
    map(~ if(length(names(.x)) == 14){
      .x %>% setNames(c(rev(paste0("variable",seq(12:1))),"country_code","year"))
    } else if(length(names(.x)) == 12){
      .x %>% setNames(c(rev(paste0("variable",seq(10:1))),"country_code","year"))
    } else if(length(names(.x)) == 10){
      .x %>% setNames(c(rev(paste0("variable",seq(8:1))),"country_code","year"))
    } else if(length(names(.x)) == 8){
      .x %>% setNames(c(rev(paste0("variable",seq(6:1))),"country_code","year"))
    } else if(length(names(.x)) == 6){
      .x %>% setNames(c(rev(paste0("variable",seq(4:1))),"country_code","year"))
    } else if(length(names(.x)) == 4){
      .x %>% setNames(c(rev(paste0("variable",seq(2:1))),"country_code","year"))
    }
    ) %>%  
    bind_rows() %>% 
    mutate(country = countrycode(country_code,"imf","country.name")) %>% 
    filter(complete.cases(country)) %>% 
    select(country_code, country, year, everything()) %>% 
    arrange(country)
  
  return(final_forecasts)
  
}