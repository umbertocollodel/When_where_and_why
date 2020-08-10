# DESCRIPTION: the script produces the final dataframe for the comparison between forecasted values in WEO
# and actual values and saves it in the intermediate data directory of the project.


# Wrangling weo forecasts function ----

#' Wrangling weo forecasts
#' 
#' Wrangling weo forecasts from excel sheet format for every issue (single variable) to a 
#' more user-friendly database.
#' 
#' @param path path to the xlsx workbook in the locale.
#' @param year_exclude character string. Not considering forecasts above this year.
#' 
#' @return tibble with three identifiers (imf code, country name and year forecasted) and
#' variable1 to 12 that correspond to the five years horizon forecast i.e. from Oct at t
#' to Apr at t-5
#' 
#' @details Format sheets: every sheet must have observations starting from 7th column, otherwise problems
#' with gather. 
#' @details Aggregate areas forecasts excluded.
#' @details Missing obs for first year forecasted because for first year no forecasts other than same year issues
#' and so on...
#' 




wrangle_weo_forecasts <- function(path = "../IEO_forecasts_material/raw_data/weo_rgdp.xlsx", year_exclude = "2019") {

path = path


sheets_name <- getSheetNames(path)
sheets_year <- getSheetNames(path) %>%
  str_extract("\\d{4}")

  # Calculate growth rates only for real Gdp and inflation:

if(str_detect(path, paste(c("rgdp","pcpi"),collapse = "|"))){

  forecasts <- sheets_name %>% 
    map(~ read_xlsx(path,sheet = .x)) %>%
    map(~ .x %>% slice(1:which(Country == "Zimbabwe"))) %>% 
    map(~ .x %>% gather("year_forecasted","variable",7:ncol(.))) %>% 
    map(~ .x %>% group_by(Series_code) %>% mutate(variable = as.numeric(variable)) %>% mutate(variable = ((variable - dplyr::lag(variable,1))/dplyr::lag(variable,1))*100)) %>% 
    map(~ .x %>% ungroup())
  } else {
    forecasts <- sheets_name %>% 
      map(~ read_xlsx(path,sheet = .x)) %>%
      map(~ .x %>% slice(1:which(Country == "Zimbabwe"))) %>% 
      map(~ .x %>% gather("year_forecasted","variable",7:ncol(.)))
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
  discard(~ unique(.x$year_forecasted) > year_exclude)


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



# Getting weo actual value (last edition) function -----

#' Getting weo actual values (from last edition)
#' 
#' From excel sheets with all issues of weo publication, gets a specific sheet and manipulates it
#' in user-friendly format.
#' 
#' @param path path to the xlsx workbook in the locale.
#' @param last_edition name of the sheet in the workbook.
#' 
#' @return tibble with two identifiers (imf code and year) and actual value of variable
#' 
#' @details Format sheets: every sheet must have observations starting from 7th column, otherwise problems
#' with gather. 
#' @details Aggregate areas forecasts excluded.
#' 



get_last_weo <- function(path = "../IEO_forecasts_material/raw_data/weo_rgdp.xlsx", last_edition = "apr2020"){
  
  last_actual <- read_xlsx(path, sheet = last_edition) %>% 
  slice(1: which(Country == "Zimbabwe")) %>% # remove composite indicators
  select(-EcDatabase, -Country, -Indicator, -Frequency, -Scale, -`2020`:-ncol(.)) %>%
  gather("year","targety_last",2:`2019`) %>% 
  filter(complete.cases(Series_code)) %>% # sometimes NA's at the end of sheet 
  rename(country_code = Series_code) %>% 
  mutate(country_code = str_extract(country_code,"\\d{3}")) %>% 
  mutate(targety_last = as.numeric(targety_last)) %>% 
  arrange(country_code, year)

  
  if(str_detect(path, paste(c("pcpi"),collapse = "|"))){
    last_actual <- last_actual %>% 
      group_by(country_code) %>%
      mutate(targety_last = ((targety_last - dplyr::lag(targety_last,1))/dplyr::lag(targety_last,1))*100) %>% 
      filter(complete.cases(targety_last)) %>% 
      arrange(country_code, year)
  }
  
  return(last_actual)
    
}



# Getting weo actual value (first settled) function ----

get_first_settled_weo <- function(path){
  
#' Getting weo actual values (October WEO issue of the following year)
#' 
#' From excel sheets with all issues of weo publication, gets all the first settled actual values.
#' 
#' @param path path to the xlsx workbook in the locale.
#' 
#' @return tibble with two identifiers (imf code and year) and actual value of variable.
#' 
#' @details Format sheets: every sheet must have observations starting from 7th column, otherwise problems
#' with gather. 
#' @details Aggregate areas forecasts excluded.
#' 


path = path


sheets_name <- getSheetNames(path) %>% 
  str_subset("oct")

sheets_year <- getSheetNames(path) %>%
  str_subset("oct") %>% 
  str_extract("\\d{4}")
              
first_actual <- sheets_name %>% 
  map(~ read_xlsx(path, sheet = .x)) %>% 
  map(~ .x %>% slice(1: which(Country == "Zimbabwe"))) %>%  # solve problem of composite countries
  map(~ .x %>% gather("year_forecasted","variable",7:ncol(.))) %>% 
  map(~ .x %>% mutate(Series_code = str_extract(Series_code, "\\d{3}"))) %>%  
  map(~ .x %>% select(Series_code, year_forecasted, variable)) %>% 
  map2(sheets_year, ~ .x %>% mutate(year_publication = .y))


if(str_detect(path, paste(c("rgdp","pcpi"),collapse = "|"))){
  
  first_actual <- first_actual %>%
    map(~ .x %>% filter(as.numeric(year_forecasted) == as.numeric(year_publication) -1 | as.numeric(year_forecasted) == as.numeric(year_publication) - 2)) %>% 
    map(~ .x %>% group_by(Series_code) %>% mutate(variable = ((variable - dplyr::lag(variable,1))/dplyr::lag(variable,1))*100)) %>% 
    map(~ .x %>% filter(as.numeric(year_publication) - as.numeric(year_forecasted) == 1)) %>% 
    bind_rows() %>% 
    select(-year_publication) %>% 
    rename(country_code = Series_code, year = year_forecasted, targety_first = variable) %>% 
    mutate(targety_first = as.numeric(targety_first)) %>% 
    arrange(country_code, year)
  }

else {
  
  first_actual <- first_actual %>% 
    map(~ .x %>% filter(as.numeric(year_forecasted) == as.numeric(year_publication) -1)) %>% 
    bind_rows() %>% 
    select(-year_publication) %>% 
    rename(country_code = Series_code, year = year_forecasted, targety_first = variable) %>% 
    mutate(targety_first = as.numeric(targety_first)) %>% 
    arrange(country_code, year)

  }
return(first_actual)

}



# Create final dataframes ----

# Set paramaters:

paths = c("../IEO_forecasts_material/raw_data/weo_rgdp.xlsx",
          "../IEO_forecasts_material/raw_data/weo_pcpi.xlsx",
          "../IEO_forecasts_material/raw_data/cagdp.xlsx")

last_edition = c(rep("apr2020",3))

name_variables = c("growth","inflation","cagdp")

export_names = c("../IEO_forecasts_material/intermediate_data/rgdp_cleaned.RData",
                 "../IEO_forecasts_material/intermediate_data/inflation_cleaned.RData",
               "../IEO_forecasts_material/intermediate_data/cagdp_cleaned.RData")

               

# Run:

forecasts <- paths %>% 
  map(~ wrangle_weo_forecasts(.x))
  
last_actual <- paths %>% 
  map2(last_edition, ~ get_last_weo(.x,.y))

first_actual <- paths %>% 
  map(~ get_first_settled_weo(.x))



# Merge:

final <- first_actual %>% 
  map2(last_actual, ~ merge(.x,.y, by = c("country_code","year"))) %>% 
  map2(forecasts, ~ merge(.x,.y, by = c("country_code","year"))) %>% 
  map(~ .x %>% arrange(country,year)) %>% 
  map(~ .x %>% select(country_code, country,year, targety_first,targety_last,
                      variable1, variable2, variable3, variable4, variable5,
                      variable6,variable7,variable8,variable9,variable10,
                      variable11, variable12)) %>% 
  map(~ .x %>% as.tibble())

names(final) <- name_variables


# Export:

final %>% 
  walk2(export_names, ~ rio::export(.x, file = .y))

# If I want a thruthful comparison between forecast errors using different target, should add complete.cases(targety_last).
# sometimes revised series start much later (data quality before not considered good enough).


####################################################


# The first actual seems always worse for recessions compared to the last actual



# Comment on European commission AMECO historical: gross domestic product at 2000 market prices in the 6th sheet

