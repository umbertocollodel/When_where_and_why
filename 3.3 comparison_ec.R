# ################ Script to compare IMF WEO forecasts with European Commission AMECO:

# Prepare dataframe: ----

load("../IEO_forecasts_material/intermediate_data/rgdp_cleaned.RData")


ec <- readRDS("../IEO_forecasts_material/intermediate_data/european commission/ec_gdp_cleaned.rds") %>%
  rename_at(vars(matches("variable")), funs(str_replace(.,"variable","ec"))) %>% 
  select(-country)

comparison_ec <- x %>% 
  merge(ec, by=c("country_code","year")) %>%
  select(country_code, country, year, targety_first, variable1, variable2, variable3, variable4,
         ec1,ec2,ec3,ec4) %>% 
  as_tibble()




# Table with list countries comparison:----

comparison_ec %>% 
  group_by(country) %>% 
  slice(1) %>% 
  select(country) %>%
  rename(Country = country) %>% 
  stargazer(summary = F,
            rownames = F,
            out = "../IEO_forecasts_material/output/tables/comparison/EC/country_sample.tex"
  )


# Comparison of forecast errors:

comparison_ec %>%  
  mutate_at(vars(contains("ec")),funs(targety_first - .)) %>% 
  mutate_at(vars(contains("variable")),funs(targety_first - .)) %>% 
  group_by(year) %>% 
  mutate(median_1ec = median(ec1, na.rm = T), median_2ec = median(ec2, na.rm = T)) %>% 
  mutate(median_1imf = median(variable1, na.rm = T), median_2imf = median(variable2, na.rm = T)) %>% 
  ggplot(aes(year)) +
  geom_line(aes(y = median_1ec, group = 1, col = "EC"), size = 1) +
  geom_line(aes(y = median_1imf, group = 1, col = "IMF" ), size = 1) +
  geom_line(aes(y = median_2ec, group = 1, col = "EC"), alpha = 0.2, size = 1) +
  geom_line(aes(y = median_2imf, group = 1, col = "IMF"), alpha = 0.2, size = 1) +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  xlab("") +
  ylab("") +
  labs(col = "Institution") +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1),
        legend.position = "bottom") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 21),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16)) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  ylim(-4,4)


ggsave("../IEO_forecasts_material/output/figures/comparison/EC/current_year_comparison.pdf")


comparison_ec %>%  
  mutate_at(vars(contains("ec")),funs(targety_first - .)) %>% 
  mutate_at(vars(contains("variable")),funs(targety_first - .)) %>% 
  group_by(year) %>% 
  mutate(median_3ec = median(ec3, na.rm = T), median_4ec = median(ec4, na.rm = T)) %>% 
  mutate(median_3imf = median(variable3, na.rm = T), median_4imf = median(variable4, na.rm = T)) %>% 
  ggplot(aes(year)) +
  geom_line(aes(y = median_3ec, group = 1, col = "EC"), size = 1) +
  geom_line(aes(y = median_3imf, group = 1, col = "IMF" ), size = 1) +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  xlab("") +
  ylab("") +
  labs(col = "Institution") +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1),
        legend.position = "bottom") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 21),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16)) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  ylim(-4,4)


ggsave("../IEO_forecasts_material/output/figures/comparison/EC/year_ahead_comparison.pdf")



# Comparison of forecast accuracy: ----

raw <- comparison_ec %>% 
  group_by(country) %>% 
  summarise_at(vars(matches("variable|ec")), funs(hydroGOF::rmse(.,targety_first))) %>% 
  ungroup() %>% 
  mutate(ratio1 = (variable1/ec1)- 1,
         ratio2 = (variable2/ec2) - 1,
         ratio3 = (variable3/ec3) -1,
         ratio4 = (variable4/ec4)-1) %>%
  mutate(better_imf1 = case_when(ratio1 < 0 ~ 1,
                                 T ~ 0),
         better_imf2 = case_when(ratio2 < 0 ~ 1,
                                 T ~ 0),
         better_imf3 = case_when(ratio3 < 0 ~ 1,
                                 T ~ 0),
         better_imf4 = case_when(ratio4 < 0 ~ 1,
                                 T ~ 0)) %>% 
  ungroup() 



raw %>% 
  select(country, contains("better")) %>%
  print(n = Inf)
  summarise_at(vars(matches("better")), mean) %>% 
  gather("horizon","share",better_imf1:better_imf4) %>% 
  mutate(horizon = case_when(horizon == "better_imf1" ~ "H=0,F",
                             horizon == "better_imf2" ~ "H=0,S",
                             horizon == "better_imf3" ~ "H=1,F",
                             T ~ "H=1,S"
  )) %>% 
  ggplot(aes(horizon, share)) +
  geom_col(width = 0.3,col = "lightgrey",alpha = 0.6) +
  theme_minimal() +
  ylab("Share of countries (%)") +
  xlab("Horizon") +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1),
        legend.position = "bottom") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 21),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  ylim(0,1)

ggsave("../IEO_forecasts_material/output/figures/comparison/EC/comparison_rmse_group.pdf")


# Full list of RMSE by country: ---- 


comparison_ec %>% 
  group_by(country) %>% 
  summarise_at(vars(matches("variable|ec")), funs(hydroGOF::rmse(.,targety_first))) %>% 
  ungroup() %>% 
  mutate(ratio1 = (variable1/ec1)- 1,
         ratio2 = (variable2/ec2) - 1,
         ratio3 = (variable3/ec3) -1,
         ratio4 = (variable4/ec4)-1) %>% 
  mutate_at(vars(matches("ratio")), round, 2) %>% 
  select(country, matches("ratio")) %>% 
  setNames(c("Country","H=0,F","H=0,S","H=1,F","H=1,S")) %>% 
  stargazer(summary = F,
            rownames = F,
            out = "../IEO_forecasts_material/output/tables/comparison/EC/full_rmse.tex")





# Sovereign Debt crisis event study: ------

countries=c("Spain","Italy","Greece","Portugal")


comparison_ec %>% 
  filter(country %in% countries & year > 2011 & year < 2016) %>% 
  ggplot(aes(year, group = 1)) +
  geom_line(aes(y=targety_first),col = "blue", size= 1.5) +
  geom_line(aes(y=ec3)) +
  geom_line(aes(y=variable3), linetype = "dashed") +
  facet_wrap(~ country) +
  theme_minimal() +
  xlab("") +
  ylab("") +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1),
        legend.position = "bottom") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 21),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16)) +
  theme(strip.text = element_text(size = 16))

ggsave("../IEO_forecasts_material/output/figures/comparison/EC/event_study_sdc.pdf")
  
