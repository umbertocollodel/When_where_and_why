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


final_sr %>% 
  .$gdp %>% 
  merge(gep_data, by=c("country","year")) %>% 
  mutate_at(vars(contains("wb")),funs(targety_first - .)) %>% 
  mutate_at(vars(contains("variable")),funs(targety_first - .)) %>% 
  arrange(wb2) %>%
  mutate(rank = 1:nrow(.)) %>% 
  filter(rank < 10) %>% 
  unite("label",country:year, sep= " ") %>%
  mutate(label = factor(label, label)) %>% 
  ggplot(aes(label, wb2, fill = group)) +
  geom_col() +
  theme_minimal() +
  coord_flip() +
  xlab("") +
  ylab("") +
  theme(legend.position = "bottom") +
  labs(fill = "Country group") +
  ylim(0,-6)

final_sr %>% 
  .$gdp %>% 
  merge(gep_data, by=c("country","year")) %>% 
  mutate_at(vars(contains("wb")),funs(targety_first - .)) %>% 
  mutate_at(vars(contains("variable")),funs(targety_first - .)) %>% 
  arrange(variable2) %>%
  mutate(rank = 1:nrow(.)) %>% 
  filter(rank < 10) %>% 
  unite("label",country:year, sep= " ") %>%
  mutate(label = factor(label, label)) %>% 
  ggplot(aes(label, variable2, fill = group)) +
  geom_col() +
  theme_minimal() +
  coord_flip() +
  xlab("") +
  ylab("") +
  theme(legend.position = "bottom") +
  labs(fill = "Country group") +
  ylim(0,-6)

# Let's focus on the tails: 

final_sr %>% 
  .$gdp %>% 
  merge(gep_data, by=c("country","year")) %>% 
  arrange(targety_first) %>% 
  mutate(rank = 1:nrow(.)) %>% 
  filter(rank < 30) %>% 
  mutate_at(vars(contains("wb")),funs(targety_first - .)) %>% 
  mutate_at(vars(contains("variable")),funs(targety_first - .)) %>% 
  ggplot(aes(variable2, wb2)) +
  geom_point() +
  theme_minimal() +
  geom_abline(intercept = 0, col = "red", size = 1) +
  xlim(-4,0) +
  ylim(-4,0) +
  xlab("WEO Forecast error") +
  ylab("GEP Forecast error")

ggsave("../IEO_forecasts_material/output/figures/comparison/WB/comparison_negative.pdf")
 

# Relative table: 



