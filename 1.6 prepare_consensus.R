##### Script to prepare consensus forecast data ----

## Note1: Latin American countries are not available in the first week of Consensus. 
# Likewise, advanced and Asian economies are not available in the second week. 
# Overlapping in 2007/2008 for some countries (Switzerland, Taiwan, Thailand, US and Vietnam) with forecasts released both first and second week.
# Kept first week.

## Note2: for India, forecast are for the previous and current year instead of current and year-ahead in April 2008 and April 2010. We filter those occurences.
# This is included in the correction parameter of the function.

## Note3: for 2007 and 2008, we are missing some horizons given that previous report are not
# available in this format.


# Wrangling consensus forecasts function: ----

#' Wrangling consensus forecasts from excel sheet format for every issue (single variable) to a 
#' more user-friendly database.
#' 
#' @param path path to the xlsx workbook in the locale.
#' @param correction see Note2.
#' @param n_col numeric, number of columns before forecasts value.
#' 
#' @return tibble with four identifiers (imf code, country name, year forecasted and name of forecaster) and
#' variable1 to 4 that correspond from current-year sept. to year-ahead apr. forecasts.
#' 
#' @details Aggregate 'Euro Area' excluded.
#' @details Missing obs for first year forecasted because for first year no forecasts other than same year issues
#' and so on...
#' 
  
wrangle_consensus_forecasts <- function(path = "../IEO_forecasts_material/raw_data/consensus/gdp_2008_2019_eme.xlsx",
                                        correction = FALSE,
                                        n_col = 12){

  path = path
  
  sheets_name <- getSheetNames(path)
  sheets_year <- getSheetNames(path) %>%
    str_extract("\\d{4}")
  

  forecasts <- sheets_name %>% 
      map(~ read_xlsx(path,sheet = .x)) %>%
      map(~ .x %>% filter(Country != "Euro Area")) %>%
      map(~ .x %>% remove_empty("cols"))
  
  ### Correct problematic features of dataframe (see Note1):
  if(correction == TRUE){
  forecasts <- forecasts %>% 
      map(~ if(any(.x$`Survey Date` == "2008 Apr 14" | .x$`Survey Date` == "2010 Apr 12")){
        .x %>% filter(Country != "India")
      } else{
        .x
      }) %>% 
      map(~ if(any(.x$`Survey Date` == "2015 Sep 07")){
        .x %>% 
          mutate(iso3 = countrycode(Country, "country.name","iso3c")) %>% 
          select(EcDatabase, Series_code, Country, iso3, everything())
      } else {
        .x
      })
  ###
  }
  
  forecasts <- forecasts %>% 
      map(~ .x %>% split(.$`Forecast Length`)) %>%
      modify_depth(2, ~ .x %>% remove_empty("cols")) %>% 
      modify_depth(2, ~ .x %>% gather("year_forecasted","variable",n_col:ncol(.))) %>% 
      map(~ .x %>% bind_rows())
  
  forecasts <-  forecasts %>% 
    map(~ .x %>% mutate(Series_code = str_extract(Series_code, "\\d{3}"))) %>%  
    map(~ .x %>% select(Series_code, `Data Type/ Forecaster`,year_forecasted, variable)) %>% 
    map2(sheets_name, ~ .x %>% mutate(date_publication = .y)) %>% 
    map2(sheets_year, ~ .x %>% mutate(year_publication = .y)) %>% 
    bind_rows() %>% 
    group_split(year_forecasted) %>% 
    map(~ .x %>% select(-year_publication)) %>% 
    map(~ .x %>% spread(date_publication, variable)) %>% 
    map(~ .x %>% rename_at(vars(starts_with("apr")), ~ paste0(.,"apr"))) %>% 
    map(~ .x %>% rename_at(vars(starts_with("apr")), ~ str_remove(.,"^apr"))) %>% 
    map(~ .x %>% rename_at(vars(starts_with("sep")), ~ paste0(.,"sep"))) %>% 
    map(~ .x %>% rename_at(vars(starts_with("sep")), funs(str_remove(.,"^sep"))))
  
  # Order names of the columns:
  
  for(i in 1:length(forecasts)){
    forecasts[[i]] <- forecasts[[i]][,order(names(forecasts[[i]]))]
  }
  
  # Discard years for which no actual value:
  
  
  forecasts <- forecasts %>% 
    discard(~ unique(.x$year_forecasted) > 2019)
  
  
  # Naming similar to Zidong and bind together:
  
  
  final_forecasts <-  forecasts %>% 
    map(~ if(length(names(.x)) == 7){
      .x %>% setNames(c(rev(paste0("variable",seq(4:1))),"forecaster","country_code","year"))
    } else if(length(names(.x)) == 6){
      .x %>% setNames(c(rev(paste0("variable",seq(3:1))),"forecaster","country_code","year"))
    } else if(length(names(.x)) == 4){
      .x %>% setNames(c(paste0("variable",1),"forecaster","country_code","year"))
    })  %>%  
    bind_rows() %>% 
    mutate(country = countrycode(country_code,"imf","country.name")) %>%
    filter(complete.cases(country)) %>% 
    select(country_code, country, year, forecaster, everything()) %>% 
    arrange(country,year)
  
  
  # Final step - remove unnecessary summary stats:
  
  unnecessary_stat <- c("High","Low","Number of Forecasts","No of Forecasts","Standard Deviation",
                        "Total","Ref.")
  
  final <- final_forecasts %>% 
    filter(!forecaster %in% unnecessary_stat) 
  
  return(final)
}

# Create and export final dataframe: -----


advanced <- wrangle_consensus_forecasts("../IEO_forecasts_material/raw_data/consensus/gdp_2008_2019_firstweek.xlsx",T,12) 
emerging <- wrangle_consensus_forecasts("../IEO_forecasts_material/raw_data/consensus/gdp_2008_2019_secondweek.xlsx",F,11)


consensus_clean <- rbind(advanced, emerging) %>% 
  arrange(country, year) %>% 
  filter(year < 2019)


# Problem of duplicates:

consensus_clean <- consensus_clean %>% 
  distinct(country, year, forecaster, .keep_all = T) 



# Export:


saveRDS(consensus_clean, file = "../IEO_forecasts_material/intermediate_data/consensus/gdp_consensus_cleaned.RDS")

cat(crayon::green(paste0("Consesus data succesfully cleaned.\nExported RData in directory:"
                         ," ../IEO_forecasts_material/intermediate_data/consensus")))

