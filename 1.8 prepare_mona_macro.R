####### Script to prepare the MONA database
# Note1: for few programs there is a discrepancy between the date of approval
# in MONA data and the SPR sheet, that is why when merging the number of obs decreases.
# Note2: programs in the MONA macroeconomic indicators database start in September 2020
# Note3: not for all programs are available macroeconomic data


# Clean MONA with macro-variables 2002-2020 ----
# Note1: we focus on board approval, first and second review.

reviews=c("R0","R1","R2")

# Note2: variable2 is the next year year-ahead forecast, not current year year-ahead forecast! 
# Correction in last subsection, but dataframe still useful for summary stats.


mona_macro_raw <- read_excel("../IEO_forecasts_material/raw_data/mona/mona_2002-2020_macro.xlsx") %>%
  filter(Description == "Gross domestic product, constant prices") %>% 
  filter(`Approval Year` <= 2018) %>% 
  split(.$`Review Type`) %>% 
  keep(~ .x %>% .$`Review Type` %>% unique() %in% reviews) %>%
  map(~ .x %>% select(`Arrangement Number`,`Country Name`,`Arrangement Type`,`Approval Date`,`Board Action Date`,
                      `T-1`,`T`,`T+1`)) %>% 
  map(~ .x %>% mutate(`T+1` = ((`T+1` - `T`)/`T`)*100,
         `T` = ((`T` - `T-1`)/`T-1`)*100)) %>% 
  map(~ .x %>% select(-`T-1`)) %>%
  map(~ .x %>% setNames(c("program_id","country","program_type","date_approval","date_action","variable1","variable2"))) %>%
  map(~ .x %>% mutate(year = lubridate::year(date_approval))) %>% 
  map(~ .x %>% mutate(year_ahead = year +1)) %>% 
  map(~ .x %>% mutate(country = str_to_sentence(tolower(country)))) %>% 
  map(~ .x %>% mutate(country_code = countrycode(country,"country.name","imf"))) %>% 
  map(~ .x %>% mutate_at(vars(starts_with("date")),funs(as.Date(.)))) %>% 
  map(~ .x %>% select(-country))


# Clean sheet with details about program (amount approved, exceptional access etc.): ----
# Note: origin for date format changes according to machine used.

mona_amount <- read_excel("../IEO_forecasts_material/raw_data/mona/mona_amounts.xlsx") %>% 
  select(-c(1, 7, 18, 20, 22, 24, 26, 28, 30, 35, 37, 40)) %>% 
  slice(-1) %>% 
  row_to_names(1) %>%
  select(`IFS Code`,`Date of Arrangement`,`Precautionary at program approval`,`Exceptional Access`,`Original Duration (Months)`,`Total amount disbursed`,`Total Approved Amount (percent of quota)`) %>%
  setNames(c("country_code","date_approval","precautionary","exceptional_access","original_duration","amount_drawn","amount_percent_quota")) %>% 
  mutate(country = countrycode(country_code,"imf","country.name")) %>%
  mutate_at(vars(contains("amount")),funs(as.numeric(.))) %>%
  mutate_at(vars(precautionary:exceptional_access),funs(case_when(. == "n.a." ~ NA_character_,
                                                                  . == ".." ~ NA_character_,
                                                                  T ~ .))) %>%
  mutate(date_approval = as.Date(as.numeric(date_approval), origin = "1899-12-30")) %>%
  filter(lubridate::year(date_approval) >= 2002 & lubridate::year(date_approval) <= 2018) %>% 
  select(country_code, country, everything()) 

# Combine for dataframe with summary statistics: ----
# Note: check why duplicates are formed in first place.


final_mona_raw <- merge(mona_macro_raw[[1]], mona_amount, by=c("country_code","date_approval")) %>% 
  as_tibble() %>%
  arrange(country) %>%
  distinct(country,year,program_id, .keep_all = T) %>% 
  select(country_code, country, everything())



# Correction for year-ahead forecasts ----


final_mona <- mona_macro_raw %>% 
  map(~ list(current = .x %>% select(-year_ahead,-variable2),
             year_ahead = .x %>% select(-year,-variable1))) %>% 
  modify_depth(2, ~ .x %>% rename_at(vars(contains("year")),funs(str_remove(.,"_.*")))) %>% 
  map(~ .x %>% reduce(merge, all = T)) %>% 
  map(~ .x %>% as_tibble()) %>% 
  bind_rows(.id = "review") %>% 
  merge(mona_amount, by=c("country_code","date_approval")) %>% 
  as_tibble() %>% 
  arrange(country,date_approval, review, year) %>% 
  select(country_code, country, year, date_approval, review, everything())


# Export: ----

final_mona_raw %>% 
  select(-year_ahead) %>% 
  saveRDS("../IEO_forecasts_material/intermediate_data/mona/mona_summary_stat.RDS")

final_mona %>% 
  saveRDS("../IEO_forecasts_material/intermediate_data/mona/mona_macro_clean.RDS")

