# Script to compare IMF WEO forecasts with World Bank GEP:


gep_data <- read_xlsx("../IEO_forecasts_material/raw_data/wb_gep.xlsx") %>% 
  rename_at(vars(matches("variable")), funs(str_replace(.,"variable","wb"))) 
  

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
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylim(-4,4)

ggsave("../IEO_forecasts_material/output/figures/comparison/WB/current_year_comparison")


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
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylim(-4,4)

ggsave("../IEO_forecasts_material/output/figures/comparison/WB/year_ahead_comparison")



