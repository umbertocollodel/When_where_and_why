# Script to compare IMF WEO forecasts with World Bank GEP:


gep_data <- read_xlsx("../IEO_forecasts_material/raw_data/wb_gep.xlsx") %>% 
  rename_at(vars(matches("variable")), funs(str_replace(.,"variable","wb"))) 
  

# Comparison of median forecast error: ----

final_sr %>% 
  .$gdp %>% 
  merge(gep_data, by=c("country","year")) %>% 
  mutate_at(vars(contains("wb")),funs(targety_first - .)) %>% 
  mutate_at(vars(contains("variable")),funs(targety_first - .)) %>% 
  group_by(year) %>% 
  mutate(median_2wb = median(wb2, na.rm = T), median_4wb = median(wb4, na.rm = T)) %>% 
  mutate(median_2imf = median(variable2, na.rm = T), median_4imf = median(variable4, na.rm = T)) %>% 
  ggplot(aes(year)) +
  geom_line(aes(y = median_2wb, group = 1, col = "WB"), size = 1) +
  geom_line(aes(y = median_2imf, group = 1, col = "IMF" ), size = 1) +
  theme_minimal() +
  xlab("") +
  ylab("") +
  labs(col = "Institution") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom") +
  ylim(-4,4)

ggsave("../IEO_forecasts_material/output/figures/comparison/WB/current_year_comparison.pdf")


final_sr %>% 
  .$gdp %>% 
  merge(gep_data, by=c("country","year")) %>% 
  mutate_at(vars(contains("wb")),funs(targety_first - .)) %>% 
  mutate_at(vars(contains("variable")),funs(targety_first - .)) %>% 
  group_by(year) %>% 
  mutate(median_2wb = median(wb2, na.rm = T), median_4wb = median(wb4, na.rm = T)) %>% 
  mutate(median_2imf = median(variable2, na.rm = T), median_4imf = median(variable4, na.rm = T)) %>% 
  ggplot(aes(year)) +
  geom_line(aes(y = median_4wb, group = 1, col = "WB"), size = 1) +
  geom_line(aes(y = median_4imf, group = 1, col = "IMF" ), size = 1) +
  theme_minimal() +
  xlab("") +
  ylab("") +
  labs(col = "Institution") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom") +
  ylim(-4,4)

ggsave("../IEO_forecasts_material/output/figures/comparison/WB/year_ahead_comparison.pdf")


# Forecast errors during recessions and expansions: ----- 

final_sr %>% 
  .$gdp %>% 
  merge(gep_data, by=c("country","year")) %>%
  mutate_at(vars(contains("wb")),funs(targety_first - .)) %>% 
  mutate_at(vars(contains("variable")),funs(targety_first - .)) %>% 
  mutate(recession = case_when(targety_first < 0 ~ 1,
                               T ~ 0)) %>% 
  group_by(recession) %>% 
  summarise(wb2 = round(median(wb2, na.rm = T),2), imf2 = round(median(variable2, na.rm = T),2),
            wb4 = round(median(wb4, na.rm = T),2),imf4 = round(median(variable4, na.rm = T),2)) %>%
  setNames(c("Recession","Current-year (WB)","Current-year (IMF)","Year-ahead (WB)","Year-ahead (IMF)")) %>%
  mutate(Recession = case_when(Recession == 1 ~ "Recession",
                               T ~ "Non-recession")) %>% 
  stargazer(rownames = F,
            summary = F,
            out = "../IEO_forecasts_material/output/tables/comparison/WB/recession_forecast_error.tex")




# Detail of recession episodes: -----

final_sr %>% 
  .$gdp %>% 
  merge(gep_data, by=c("country","year")) %>% 
  mutate(recession = case_when(targety_first < 0 ~ 1,
                               T ~ 0)) %>% 
  filter(recession == 1) %>%
  select(country, year, targety_first, variable2, wb2) %>%
  unite("label", country:year, sep = " ", remove = F) %>% 
  gather("var","value",targety_first:wb2) %>% 
  mutate(var = case_when(var == "targety_first" ~ "Actual",
         var == "variable2" ~ "WEO Forecast",
         T ~ "GEP Forecast")) %>% 
  ggplot(aes(label, value, col = var, group = var)) +
    geom_line(size = 2) +
    geom_point(size = 3) +
    theme_minimal() +
    xlab("") +
    ylab("") +
    labs(col = "") +
    theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1)) +
    theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 21))

ggsave("../IEO_forecasts_material/output/figures/comparison/WB/comparison_recessions.pdf")
  
    





final_sr %>% 
  .$gdp %>% 
  merge(gep_data, by=c("country","year")) %>%
  mutate_at(vars(contains("wb")),funs(targety_first - .)) %>% 
  mutate_at(vars(contains("variable")),funs(targety_first - .)) %>% 
  mutate(recession = case_when(targety_first < 0 ~ 1,
                               T ~ 0)) %>% 
  group_by(recession) %>% 
  summarise(wb2 = round(median(wb2, na.rm = T),2), imf2 = round(median(variable2, na.rm = T),2),
            wb4 = round(median(wb4, na.rm = T),2),imf4 = round(median(variable4, na.rm = T),2)) %>%
  setNames(c("Recession","Current-year (WB)","Current-year (IMF)","Year-ahead (WB)","Year-ahead (IMF)")) %>%
  mutate(Recession = case_when(Recession == 1 ~ "Recession",
                               T ~ "Non-recession")) %>% 
  stargazer(rownames = F,
            summary = F,
            out = "../IEO_forecasts_material/output/tables/comparison/WB/recession_forecast_error.tex")
  
  
  
  





