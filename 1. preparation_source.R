# Main script for the cleaning of raw data:


##############Prepare the environment

remove(list = ls())

packages <- c("openxlsx","purrr","tidyverse", "readxl","countrycode","roxygen2",
              "stargazer","hydroGOF","rio") 


lapply(packages, function(x){
  do.call("require", list(x))
}
  )

###############Source the files:

source("1.1 preparation_growth.R")



