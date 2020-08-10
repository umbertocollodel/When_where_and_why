#### Script to clean the January and July updates of WEO, instrumental for comparison
#### with World Bank data


# Creation function: ----

wrangle_weo_updates <- function(path){
  
#' Wrangling weo January and July updates.
#' 
#' Wrangling weo forecasts from excel sheet format for every issue (single variable) to a 
#' more user-friendly database.
#' 
#' @param path path to the xlsx workbook in the locale.
#' @param year_exclude character string. Not considering forecasts above this year.
#' 
#' @return tibble with three identifiers (imf code, country name and year forecasted) and
#' variable1 to variable2, indicating respectively current-year and year-ahead forecast.
#' 
#' @details Format sheets: every sheet must have observations starting from 7th column, otherwise problems
#' with gather. 
#' @details Aggregate areas forecasts excluded.
#' @details Missing obs for first year forecasted because for first year no forecasts other than same year issues
#' and so on...
#' 
  
  
  
path = path

sheets_name <- getSheetNames(path)

sheets_year <- getSheetNames(path) %>%
  str_extract("\\d{4}")

forecasts <- sheets_name %>% 
  map(~ read_xlsx(path,sheet = .x)) %>%
  map(~ .x %>% slice(1:which(Country == "Zimbabwe"))) %>% 
  map(~ .x %>% select(-matches("Q\\d"))) %>% 
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



# Revert column order for for some df depending on path: (later on improve this part of the code, not elegant)

if(path == "../IEO_forecasts_material/raw_data/weo_january_update.xlsx"){
forecasts[[1]] <- forecasts[[1]] %>% 
  select(Series_code, jan2009, everything())
} else if(path == "../IEO_forecasts_material/raw_data/weo_july_update.xlsx"){
  forecasts[[3]] <- forecasts[[3]] %>% 
    select(Series_code,sep2011, everything())
}


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

  return(final_forecasts)
}


# Set parameters and run: ----

paths = c("../IEO_forecasts_material/raw_data/weo_january_update.xlsx",
          "../IEO_forecasts_material/raw_data/weo_july_update.xlsx")


updates <- paths %>% 
  map(~ wrangle_weo_updates(.x))  
  
# Change names columns:
# Note: be careful about forecast horizon

updates[[1]] <- updates[[1]] %>% 
  rename(variable2 = variable1, variable4 = variable2)

updates[[2]] <- updates[[2]] %>% 
  rename(variable3 = variable2)

# Combine:

weo_updates <- updates %>% 
  reduce(merge, by=c("country_code","country","year")) %>% 
  select(country_code, country, year, variable1, variable2, variable3, variable4) %>% 
  as_tibble()


# Export:


saveRDS(weo_updates, file = "../IEO_forecasts_material/intermediate_data/rgdp_update_cleaned.RDS")



