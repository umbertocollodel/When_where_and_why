####### Script to prepare the MONA database


# Clean MONA with macro-variables 2002-2020 ----
# Note1: for the moment we focus only on board approval forecasts
# in the future work also on correction of forecasts with reviews.
# Note2: variable2 is the next year year-ahead forecast! Correction in last paragraph, but
# dataframe still useful for summary stats.


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
  mutate(date = as.Date(date)) %>% 
  select(-country)



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
  mutate_at(vars(precautionary:exceptional_access),funs(case_when(. == "n.a." ~ NA_character_,
                                                                  . == ".." ~ NA_character_,
                                                                  T ~ .))) %>%
  mutate(date = as.Date(as.numeric(date), origin = "1899-12-30")) %>%
  filter(year >= 2002) %>% 
  select(country_code, country, everything()) 

# Combine and export: ----
# Note: check why duplicates are formed in first place.


final_mona_raw <- merge(mona_macro_raw, mona_amount, by=c("country_code","year","date")) %>% 
  as_tibble() %>%
  arrange(country) %>%
  distinct(country,year,program_id, .keep_all = T) %>% 
  select(country_code, country, everything())



# Correction for year-ahead forecasts ----

final_mona <- list(current = final_mona_raw %>% select(-year_ahead,-variable2),
                   year_ahead = final_mona_raw %>% select(-year,-variable1)) %>% 
  map(~ .x %>% rename_at(vars(contains("year")),funs(str_remove(.,"_.*")))) %>% 
  reduce(merge, all = T) %>% 
  as_tibble() %>% 
  arrange(country)

# Export: ----

final_mona_raw %>% 
  select(-year_ahead) %>% 
  saveRDS("../IEO_forecasts_material/intermediate_data/mona/mona_summary_stat.RDS")

final_mona %>% 
  saveRDS("../IEO_forecasts_material/intermediate_data/mona/mona_macro_clean.RDS")

