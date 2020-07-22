##### Script to prepare World Bank forecast data ----

read_xlsx("../IEO_forecasts_material/raw_data/world bank/GEP_forecast_vintages_06232020.xlsx",
          sheet = "Table_long") %>% 
  select(1:4) %>% 
  setNames(c("date_publication","country","year_forecasted","wb")) %>% 
  mutate(wb = case_when(wb == "..." ~ NA,
                        T ~ wb))