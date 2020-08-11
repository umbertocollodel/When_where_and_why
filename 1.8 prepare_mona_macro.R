####### Script to prepare the MONA database


# Clean MONA with macro-variables 2002-2020 ----
# Note: for the moment we focus only on board approval forecasts
# in the future work also on correction of forecasts with reviews.

mona_macro_raw <- read_excel("../IEO_forecasts_material/raw_data/mona/mona_2002-2020_macro.xlsx") %>%
  filter(`Review Type` == "R0") %>% 
  filter(Description == "Gross domestic product, constant prices") %>% 
  select(`Arrangement Number`,`Country Name`,`Arrangement Type`,`Approval Date`,
         `Approval Year`,`T-1`,`T`,`T+1`) %>% 
  mutate(`T+1` = ((`T+1` - `T`)/`T`)*100,
         `T` = ((`T` - `T-1`)/`T-1`)*100) %>% 
  select(-`T-1`) %>%
  setNames(c("program_id","country","program_type","date","year","variable1","variable2")) %>%
  mutate(year_ahead = year +1) %>% 
  mutate(country = str_to_sentence(tolower(country))) %>% 
  mutate(country_code = countrycode(country,"country.name","imf")) %>% 
  mutate(date = as.Date(date)) 


mona_macro <- list(current = mona_macro_raw %>% select(program_id, country_code, country, program_type, date, year, variable1),
     year_ahead = mona_macro_raw %>% select(program_id, country_code, country, program_type, date, year_ahead, variable2)) %>% 
  map(~ .x %>% rename_at(vars(contains("year")),funs(str_remove(.,"_.*")))) %>% 
  reduce(merge, all = T) %>% 
  as_tibble() 




# Clean sheet with dummy for exceptional access: ----
# Note: origin for date format changes according to machine used.

mona_amount <- read_excel("../IEO_forecasts_material/raw_data/mona/mona_amounts.xlsx") %>% 
  select(-c(1, 7, 18, 20, 22, 24, 26, 28, 30, 35, 37, 40)) %>% 
  slice(-1) %>% 
  row_to_names(1) %>%
  select(`IFS Code`,Year,`Date of Arrangement`,`Precautionary at program approval`,`Exceptional Access`,`Original Duration (Months)`,`Total amount disbursed`,`Total Approved Amount (percent of current quota)`) %>%
  setNames(c("country_code","year","date","precautionary","exceptional_access","original_duration","amount_drawn","amount_percent_quota")) %>% 
  mutate(country = countrycode(country_code,"imf","country.name")) %>%
  mutate_at(vars(contains("amount")),funs(as.numeric(.))) %>%
  mutate(date = as.Date(as.numeric(date), origin = "1899-12-30")) %>% 
  filter(year >= 2002) %>% 
  select(country_code, country, everything()) 

# Combine the two and export: ----
# Note: check why duplicates are formed in first place.


final_mona <- merge(mona_macro, mona_amount, all.x = T) %>% 
  as_tibble() %>%
  arrange(country) %>%
  group_by(country) %>% 
  mutate_at(vars(precautionary:amount_percent_quota), 
            funs(case_when(dplyr::lag(program_id,1) == program_id ~ dplyr::lag(.,1),
                 T ~ .))) %>% 
  distinct(country,year,program_id, .keep_all = T) %>% 
  mutate_at(vars(precautionary:exceptional_access),funs(case_when(. == "n.a." ~ NA_character_,
                                                                  . == ".." ~ NA_character_,
                                                                  T ~ .))) %>% 
  select(country_code, country, everything())

# Export:


final_mona %>% 
saveRDS("../IEO_forecasts_material/intermediate_data/mona/mona_macro_clean.RDS")
