# Script to prepare European Commission data 

# Scraping EC website and download the files: -----

urls <- read_html("https://ec.europa.eu/info/business-economy-euro/indicators-statistics/economic-databases/macro-economic-database-ameco/ameco-archive_en") %>%
  html_nodes("a") %>%
  html_attr("href") %>% 
  str_subset("ameco_.*\\.zip")


names <- urls %>% 
  str_extract("ameco_.*\\.zip")


urls %>% 
  walk2(names, ~ tryCatch(
                  expression = {
                    download.file(.x, paste0("../IEO_forecasts_material/raw_data/european commision/",.y))
                    cat(crayon::green(paste0(.y," succesfully downloaded.\n")))
                    },
                  error = function(e){
                   cat(crayon::red(paste0("Could not download file: ",.y,"\n")))
                 }
  ))


# Could create a function to get all EC forecasts!

# Unzip all files -----


names <- names %>% 
  str_remove("\\.zip") %>% 
  sort()


list.files("../IEO_forecasts_material/raw_data/european commision/") %>% 
  map_chr(~ paste0("../IEO_forecasts_material/raw_data/european commision/",.x)) %>% 
  map2(names, ~ unzip(.x, exdir = paste0("../IEO_forecasts_material/raw_data/european commision/",.y)))


cat(crayon::green("Unzipped downloaded files.\n"))


# Load all text files with gdp data (6th sheet) ----

list_df <- list.files("../IEO_forecasts_material/raw_data/european commision/") %>% 
  map_chr(~ paste0("../IEO_forecasts_material/raw_data/european commision/",.x)) %>% 
  str_subset("\\.zip",negate = T) %>% 
  map(~ list.files(.x)) %>% 
  map(~ .x %>% str_subset("AMECO6")) %>% 
  map2_chr(names, ~ paste0("../IEO_forecasts_material/raw_data/european commision/",.y,"/",.x)) %>% 
  map(~ read.csv(.x, sep = ";"))

# Assign names:

for(i in 1:length(list_df)){
  names(list_df)[i] <- names[i]
}



# Wrangle: ----

list_df_cleaned <- list_df %>% 
  map(~ .x %>% as_tibble()) %>%
  map(~ .x %>% select(-X)) %>% 
  map(~ .x %>% rename_all(funs(str_remove(.,"^X")))) %>%
  map(~ .x %>% filter(SUB.CHAPTER == "01 Gross domestic product")) %>% 
  map(~ .x %>% mutate(TITLE = as.character(TITLE))) %>% 
  map(~ .x %>% filter(TITLE == "Gross domestic product at 2005 market prices " |
                      TITLE == "Gross domestic product at 2010 reference levels "|
                      TITLE == "Gross domestic product at 2010 market prices " |
                      TITLE == "Gross domestic product at 2015 reference levels "|
                      TITLE == "Gross domestic product at 2000 market prices ")) %>% 
  map(~ .x %>% select(-COUNTRY,-SUB.CHAPTER, -UNIT)) %>% 
  map(~ .x %>% mutate(CODE = str_extract(CODE,"^[A-Z]{3}"))) %>% 
  map(~ .x %>% filter(complete.cases(CODE))) %>%
  map(~ .x %>% mutate(country_code = countrycode(CODE,"iso3c","imf"))) %>% 
  map(~ .x %>% rename(country = CODE)) %>% 
  map(~ .x %>% select(country_code, country, TITLE, everything()))
  


list_df_gathered <- list_df_cleaned %>% 
  map(~ .x %>% select(-TITLE)) %>% 
  map(~ .x %>% gather("year_forecasted","ec", `1960`:ncol(.))) %>%
  imap(~ .x %>% mutate(date_publication = .y)) %>% 
  map(~ .x %>% mutate(year_publication = str_extract(date_publication, "\\d{4}"))) %>% 
  map(~ .x %>% mutate(ec = as.numeric(ec))) %>% 
  map(~ .x %>% group_by(country) %>% mutate(ec = ((ec - dplyr::lag(ec,1))/dplyr::lag(ec,1))*100)) %>% 
  map(~ .x %>% filter(year_forecasted >= year_publication & year_forecasted <= as.character(as.numeric(year_publication) + 5))) %>% 
  map(~ .x %>% ungroup())




forecasts <- list_df_gathered %>% 
  bind_rows() %>% 
  split(.$year_forecasted) %>% 
  map(~ .x %>% select(-year_publication)) %>% 
  map(~ .x %>% spread(date_publication,ec)) %>% 
  map(~ .x %>% rename_at(vars(starts_with("ameco_autumn")), ~ paste0(.,"oct"))) %>% 
  map(~ .x %>% rename_at(vars(starts_with("ameco_autumn")), ~ str_remove(.,"^ameco_autumn"))) %>% 
  map(~ .x %>% rename_at(vars(starts_with("ameco_spring")), ~ paste0(.,"apr"))) %>% 
  map(~ .x %>% rename_at(vars(starts_with("ameco_spring")), funs(str_remove(.,"^ameco_spring"))))

# Order names of the columns:

for(i in 1:length(forecasts)){
  forecasts[[i]] <- forecasts[[i]][,order(names(forecasts[[i]]))]
}

# Discard years for which no actual value:


forecasts <- forecasts %>% 
  discard(~ unique(.x$year_forecasted) > 2019)


# Naming similar to Zidong and bind together:
# Note: opposit order compared to other datasets (autumn-spring compared to apr-oct)


final_forecasts <-  forecasts %>% 
  map(~ if(length(names(.x)) == 5){
    .x %>% setNames(c(rev(paste0("variable",seq(2:1))),"country","country_code","year"))
  } else if(length(names(.x)) == 7){
    .x %>% setNames(c(rev(paste0("variable",seq(4:1))),"country","country_code","year"))
  } else if(length(names(.x)) == 8){
    .x %>% setNames(c(rev(paste0("variable",seq(5:1))),"country","country_code","year"))
  }) %>%  
  bind_rows() %>% 
  mutate(country = countrycode(country,"iso3c","country.name")) %>% 
  select(country_code, country, year, everything()) %>% 
  arrange(country)


# Export: ----


saveRDS(final_forecasts, file = "../IEO_forecasts_material/intermediate_data/european commission/ec_gdp_cleaned.rds")


