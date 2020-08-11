####### Script to prepare the MONA database


# Clean MONA with macro-variables 2002-2020 ----
# Note: for the moment we focus only on board approval forecasts
# in the future work also on correction of forecasts with reviews.

mona_macro <- read_excel("../IEO_forecasts_material/raw_data/mona/mona_2002-2020_macro.xlsx") %>%
  filter(`Review Type` == "R0") %>% 
  filter(Description == "Gross domestic product, constant prices") %>% 
  select(`Arrangement Number`,`Country Name`,`Arrangement Type`,`Approval Date`,
         `Approval Year`,`T-1`,`T`,`T+1`) %>% 
  mutate(`T+1` = ((`T+1` - `T`)/`T`)*100,
         `T` = ((`T` - `T-1`)/`T-1`)*100) %>% 
  select(-`T-1`) %>% 
  setNames(c("program_id","country","program_type","date","year","variable1","variable2")) %>%
  mutate(country = str_to_sentence(tolower(country))) %>% 
  mutate(country_code = countrycode(country,"country.name","imf")) %>% 
  mutate(date = as.Date(date)) %>% 
  select(country_code, country, program_id,date, year, program_type, variable1, variable2)

