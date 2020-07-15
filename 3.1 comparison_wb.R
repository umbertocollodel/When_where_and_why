# Script to compare IMF WEO forecasts with World Bank GEP:

# World Bank data:

gep_data <- read_xlsx("../IEO_forecasts_material/raw_data/wb_gep.xlsx") %>% 
  rename_at(vars(matches("variable")), funs(str_replace(.,"variable","wb"))) %>% 
  select(country, year, wb2, wb4) %>% 
  rename(wb1 = wb2, wb2 = wb4)

# IMF WEO January update data:


load("../IEO_forecasts_material/intermediate_data/rgdp_jan_update.RData")

# Actual values: (fall issue of next year WEO)

target <- final_sr$gdp %>% 
  select(country_code, year, targety_first)

# Bind together:

comparison_wb <- x %>%   
  merge(gep_data, by=c("country","year")) %>% 
  merge(target, by=c("country_code","year")) %>% 
  select(country_code, country, year, targety_first, variable1, wb1, variable2, wb2)
  

# Comparison of median forecast error: ----

comparison_wb %>%  
  mutate_at(vars(contains("wb")),funs(targety_first - .)) %>% 
  mutate_at(vars(contains("variable")),funs(targety_first - .)) %>% 
  group_by(year) %>% 
  mutate(median_1wb = median(wb1, na.rm = T), median_2wb = median(wb2, na.rm = T)) %>% 
  mutate(median_1imf = median(variable1, na.rm = T), median_2imf = median(variable2, na.rm = T)) %>% 
  ggplot(aes(year)) +
  geom_line(aes(y = median_1wb, group = 1, col = "WB"), size = 1) +
  geom_line(aes(y = median_1imf, group = 1, col = "IMF" ), size = 1) +
  theme_minimal() +
  xlab("") +
  ylab("") +
  labs(col = "Institution") +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1),
        legend.position = "bottom") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 21),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16)
  ) +
  ylim(-4,4)

ggsave("../IEO_forecasts_material/output/figures/comparison/WB/current_year_comparison.pdf")


comparison_wb %>% 
  mutate_at(vars(contains("wb")),funs(targety_first - .)) %>% 
  mutate_at(vars(contains("variable")),funs(targety_first - .)) %>% 
  group_by(year) %>% 
  mutate(median_1wb = median(wb1, na.rm = T), median_2wb = median(wb2, na.rm = T)) %>% 
  mutate(median_1imf = median(variable1, na.rm = T), median_2imf = median(variable2, na.rm = T)) %>% 
  ggplot(aes(year)) +
  geom_line(aes(y = median_2wb, group = 1, col = "WB"), size = 1) +
  geom_line(aes(y = median_2imf, group = 1, col = "IMF" ), size = 1) +
  theme_minimal() +
  xlab("") +
  ylab("") +
  labs(col = "Institution") +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1),
        legend.position = "bottom") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 21),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16)
  ) +
  ylim(-4,4)


ggsave("../IEO_forecasts_material/output/figures/comparison/WB/year_ahead_comparison.pdf")


# Forecast errors during recessions and expansions: (table) ----- 

comparison_wb %>% 
  mutate_at(vars(contains("wb")),funs(targety_first - .)) %>% 
  mutate_at(vars(contains("variable")),funs(targety_first - .)) %>% 
  mutate(recession = case_when(targety_first < 0 ~ 1,
                               T ~ 0)) %>% 
  group_by(recession) %>% 
  summarise(wb1 = round(median(wb1, na.rm = T),2), imf1 = round(median(variable1, na.rm = T),2),
            wb2 = round(median(wb2, na.rm = T),2),imf2 = round(median(variable2, na.rm = T),2)) %>%
  setNames(c("Recession","Current-year (WB)","Current-year (IMF)","Year-ahead (WB)","Year-ahead (IMF)")) %>%
  mutate(Recession = case_when(Recession == 1 ~ "Recession",
                               T ~ "Non-recession")) %>% 
  stargazer(rownames = F,
            summary = F,
            out = "../IEO_forecasts_material/output/tables/comparison/WB/recession_forecast_error.tex")




# Detail of recession episodes: -----

comparison_wb %>% 
  mutate(recession = case_when(targety_first < 0 ~ 1,
                               T ~ 0)) %>% 
  filter(recession == 1) %>%
  select(country, year, targety_first, variable1, wb1) %>%
  unite("label", country:year, sep = " ", remove = F) %>% 
  gather("var","value",targety_first:wb1) %>% 
  mutate(var = case_when(var == "targety_first" ~ "Actual",
         var == "variable1" ~ "WEO Forecast",
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
        axis.title = element_text(size = 21),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16))+
    ylim(-4,4) +
    theme(legend.position = "bottom")

ggsave("../IEO_forecasts_material/output/figures/comparison/WB/comparison_recessions.pdf")

footnote = c("IEO calculations. WEO and GEP forecasts refer to current-year forecasts released in the January
             issue of each publication") %>% 
  cat(file = "../IEO_forecasts_material/output/figures/comparison/WB/comparison_recessions_footnote.tex")

  
    








