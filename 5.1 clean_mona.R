
series=c("RGDPC","PCPIC","BCAX","GPB_Y")

list_df <- series %>% 
  map(~ read.csv("../IEO_forecasts_material/raw_data/mona/ArchMecon.csv") %>% filter(str_detect(Mneumonic,.x))) %>%
  map(~ .x %>% as_tibble())


names(list_df) <- series
