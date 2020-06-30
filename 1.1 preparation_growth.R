# DESCRIPTION: the script produces the final dataframe for the comparison between forecasted values in WEO
# and actual values and saves it in the output directory of the project.

path = "../IEO_forecasts_material/raw_data/weo_rgdp.xlsx"
  
  
# Wrangle orecasts -----


sheets_name <- getSheetNames(path)
sheets_year <- getSheetNames(path) %>%
  str_extract("\\d{4}")



forecasts <- sheets_name %>% 
  map(~ read_xlsx(path,sheet = .x)) %>%
  map(~ .x %>% gather("year_forecasted","rgdp",`1989`:`2030`)) %>% 
  map(~ .x %>% select(Country, year_forecasted, rgdp)) %>% 
  map2(sheets_name, ~ .x %>% mutate(date_publication = .y)) %>% 
  map2(sheets_year, ~ .x %>% mutate(year_publication = .y)) %>% 
  map(~ .x %>% filter(year_forecasted >= year_publication & year_forecasted <= as.character(as.numeric(year_publication) + 5))) %>%
  map(~ .x %>% filter(complete.cases(Country))) %>% 
  bind_rows() %>% 
  group_split(year_forecasted) %>% 
  map(~ .x %>% select(-year_publication)) %>% 
  map(~ .x %>% spread(date_publication,rgdp)) %>% 
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
  discard(~ unique(.x$year_forecasted) >= 2019)


# Naming similar to Zidong and bind together:


final_forecasts <-  forecasts %>% 
  map(~ if(length(names(.x)) == 14){
    .x %>% setNames(c(paste0("gdp",seq(12:1)),"country","year_forecasted"))
  } else if(length(names(.x)) == 12){
    .x %>% setNames(c(paste0("gdp",seq(10:1)),"country","year_forecasted"))
  } else if(length(names(.x)) == 10){
    .x %>% setNames(c(paste0("gdp",seq(8:1)),"country","year_forecasted"))
  } else if(length(names(.x)) == 8){
    .x %>% setNames(c(paste0("gdp",seq(6:1)),"country","year_forecasted"))
  } else if(length(names(.x)) == 6){
    .x %>% setNames(c(paste0("gdp",seq(4:1)),"country","year_forecasted"))
  } else if(length(names(.x)) == 4){
    .x %>% setNames(c(paste0("gdp",seq(2:1)),"country","year_forecasted"))
  }
  ) %>% 
  bind_rows() %>% 
  select(country, year_forecasted, everything())
  

# Now just need to re-name columns and bind!

duplicated(final_forecasts[,1:2]) %>% 
  as.tibble() %>% 
  print(n = Inf)


# Actual -----

actual <- read_xlsx(path, sheet = "apr2020gr") %>% 
  select(-EcDatabase, -Series_code, -Indicator, -Frequency, -Scale, -`2020`:-`2030`) %>% 
  gather("year","targety",`1989`:`2019`) %>% 
  filter(complete.cases(Country)) 

# Combine ------


merge(actual, forecasts)






paths = c("../IEO_forecasts_material/raw_data/cagdp.xlsx","../IEO_forecasts_material/raw_data/weo_pcpi.xlsx","../IEO_forecasts_material/raw_data/weo_ggxcnl_ngdp_Post2010.xlsx")
destination_paths = c("../IEO_forecasts_material/raw_data/cagdp.xlsx","../IEO_forecasts_material/raw_data/weo_pcpi.xlsx","../IEO_forecasts_material/raw_data/weo_ggxcnl_ngdp_Post2010.xlsx")


sheets <- getSheetNames("../IEO_forecasts_material/raw_data/weo_pcpi.xlsx")

sheets %>% 
  map(~ read_xlsx("../IEO_forecasts_material/raw_data/weo_pcpi.xlsx",sheet = .x)) %>% 
  map(~ .x %>% select(-ends_with("Q\\d")))

paths %>% 
  map(~ getSheetNames(.x)) %>% 
  map(~ read_xlsx(paths[1], sheet = .x)) %>% 
  map2(destination_paths,~ rio::export(.x, file = .y))

library(rio)
?rio::export



