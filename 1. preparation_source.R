# Main script for the cleaning of raw data:


##############Prepare the environment

remove(list = ls())

packages <- c("openxlsx","purrr","tidyverse", "readxl","countrycode","roxygen2",
              "stargazer","hydroGOF","rio","forecast","janitor", "rvest",
              "pdftools") 


lapply(packages, function(x){
  do.call("require", list(x))
}
  )

###############Source the files:

source("1.1 prepare_weo.R")



