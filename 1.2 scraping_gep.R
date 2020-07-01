# Let's try to scrape the world bank shit:

expressions = c("East Asia and the Pacific country forecasts",
                "Europe and Central Asia country forecasts",
                "Latin America and the Caribbean country forecasts",
                "Middle East and North Africa country forecasts",
                "South Asia country forecasts",
                "Sub-Saharan Africa country forecasts")


expressions_rows = c("GDP at market prices (% annual growth)")


library(pdftools)


list_tables <- pdf_text("~/Downloads/GEP 2014 June.pdf") %>% 
  data.frame(pages = .) %>% 
  filter(str_detect(pages, paste(expressions, collapse = "|"))) %>%
  slice(-1,-2) %>% 
  mutate(pages = str_remove(pages, "\nSource: World Bank.*")) %>%
  pull(pages) %>% 
  str_split("\n") %>%
  # Remove undesired rows
  map(~ str_subset(.x, "Current account", negate = T)) %>%
  map(~ str_subset(.x, "Calendar year basis", negate = T)) %>%
  map(~ str_subset(.x, "^\\s*[a-z]", negate = T)) %>% 
  map(~ str_subset(.x, paste(expressions,collapse = "|"), negate = T)) %>% 
  map(~ as_tibble(.x, .name_repair = make.names))


# Problem with white spaces first row, keep subset of tables with same structure: -----

# Europe and central Asia, Latin America and Sub-saharan Africa

list_tables[c(2,3,6)] %>% 
  map(~ .x %>% mutate(country = str_sub(value, 0, 39),
                      year1 =str_sub(value, 46,52),
                      year2 = str_sub(value, 55,61),
                      year3 = str_sub(value, 62, 68)))
  
  

