############# Main script for the analysis of MONA forecasts:
# Cleans global environment, loads required packages for the section and runs
# individual scripts of section.


# Prepare the environment: -----

remove(list = ls())

packages <- c("openxlsx","purrr","tidyverse", "readxl","countrycode","roxygen2",
              "stargazer","hydroGOF","rio","forecast","janitor", "rvest",
              "pdftools","DescTools","lubridate","janitor") 


lapply(packages, function(x){
  do.call("require", list(x))
}
)


mona_summary <- readRDS("../When_where_and_why_material/intermediate_data/mona/mona_summary_stat.RDS")
mona_rgdp <- readRDS("../When_where_and_why_material/intermediate_data/mona/mona_macro_clean.RDS")


# Source the files of fifth section: ------


list.files() %>% 
  str_subset("^5.\\d") %>%
  walk(~ source(.x))

