# Main script for the comparison of WEO forecasts with Consensus, EC and WB:


# Function to use pipe with ggsave: ----

save.plot <- function(x,filename){
  ggsave(filename,x)
}

# Source and produce output: ----


list.files() %>% 
  str_subset("^3.\\d") %>%
  walk(~ source(.x))