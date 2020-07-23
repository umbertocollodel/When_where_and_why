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
  str_remove("\\.zip")


list.files("../IEO_forecasts_material/raw_data/european commision/") %>% 
  map_chr(~ paste0("../IEO_forecasts_material/raw_data/european commision/",.x)) %>% 
  map2(names, ~ unzip(.x, exdir = paste0("../IEO_forecasts_material/raw_data/european commision/",.y)))





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



# Gross domestic product at 2005 market prices

list_df %>% 
  map(~ .x %>% as_tibble()) %>%
  map(~ .x %>% select(-X)) %>% 
  map(~ .x %>% rename_all(funs(str_remove(.,"^X")))) %>%
  map(~ .x %>% mutate(TITLE = as.character(TITLE))) %>% 
  map(~ .x %>% filter(str_detect(TITLE,"Gross domestic product at 2005 market prices")))





