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


