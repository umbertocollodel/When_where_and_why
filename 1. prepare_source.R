############# Main script for the cleaning of raw data:
# Cleans global environment, loads required packages for the section and runs
# individual scripts of section.


# Prepare the environment: -----

remove(list = ls())

packages <- c("openxlsx","purrr","tidyverse", "readxl","countrycode","roxygen2",
              "stargazer","hydroGOF","rio","forecast","janitor", "rvest",
              "pdftools","lubridate","ggridges") 


lapply(packages, function(x){
  do.call("require", list(x))
}
  )

# Source the files of first section: ------


list.files() %>% 
  str_subset("^1.\\d") %>%
  walk(~ source(.x))
  



