# Comparison between WEO and Consensus foecasts 

# Prepare the data ----

load("../IEO_forecasts_material/intermediate_data/consensus/gdp_consensus_cleaned.RData")

consensus <- x %>% 
  rename_at(vars(matches("variable")), funs(str_replace(.,"variable","consensus"))) %>% 
  select(-country)

comparison_consensus <- final_sr$gdp %>%
  merge(consensus, by = c("country_code","year")) %>% 
  as_tibble()

# Dataframe with geographical groups:

group <- comparison_consensus %>%
  filter(forecaster == "Consensus (Mean)") %>%
  group_by(country_code) %>% 
  slice(1) %>% 
  select(country_code, group) %>% 
  mutate(group = case_when(group == "europe" ~ "Advanced Economies",
                           is.na(group) ~ "Advanced Economies",
                           group == "emerging_asia" ~ "Emerging Asia",
                           group == "latin_america" ~ "Latin America",
                           group == "emerging_europe" ~ "Emerging Europe",
                           group == "middle_east" ~ "Middle East",
                           T ~ "Africa"))


# Table with list countries ----

comparison_consensus %>% 
  group_by(country_code) %>% 
  slice(1) %>% 
  mutate(country_name = countrycode(country_code, "imf","country.name")) %>% 
  arrange(country_name) %>% 
  mutate(group = case_when(group == "emerging_asia" ~ "Emerging Asia",
                           group == "emerging_europe" ~ "Emerging Europe",
                           group == "latin_america" ~ "Latin America",
                           group == "middle_east" ~ "Middle East",
                           T ~ "Advanced Economies")) %>% 
  ungroup() %>% 
  select(country_name, group) %>%
  stargazer(summary = F,
            rownames = F,
            out = "../IEO_forecasts_material/output/tables/comparison/consensus/list_countries.tex")


# Compare RMSE by country ----


rmse_comparison <- comparison_consensus %>% 
  filter(forecaster == "Consensus (Mean)") %>% 
  group_by(country_code) %>%
  select(country_code, country, year, targety_first, variable1:variable4, consensus1:consensus4, group) %>% 
  summarise_at(vars(variable1:consensus4), funs(hydroGOF::rmse(.,targety_first))) %>% 
  mutate(country = countrycode(country_code,"imf","country.name")) %>% 
  mutate(ratio1 = variable1/consensus1 - 1,
         ratio2 = variable2/consensus2 - 1,
         ratio3 = variable3/consensus3 - 1,
         ratio4 = variable4/consensus4 - 1) 
  
 group %>% 
  merge(rmse_comparison, by=c("country_code")) %>% 
  mutate_at(vars(ratio1:ratio4), funs(case_when(. < 0 ~ 1,
                                                T ~ 0))) %>% 
  ungroup() %>% 
  group_by(group) %>% 
  summarise_at(vars(ratio1:ratio4), mean, na.rm = T) %>% 
  gather("horizon","share",ratio1:ratio4) %>% 
  mutate(horizon = case_when(horizon == "ratio1"~ "H=0,F",
                             horizon == "ratio2"~ "H=0,S",
                             horizon == "ratio3"~ "H=1,F",
                             T ~ "H=1,S")) %>% 
  ggplot(aes(horizon, share)) +
  geom_col(width = 0.3,col = "lightgrey") +
  facet_wrap(~ group) +
  theme_minimal() +
  ylab("Share of countries (%)") +
  xlab("Horizon") +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1),
         legend.position = "bottom") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 21),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16)) +
  theme(strip.text.x = element_text(size = 14, colour = "darkblue")) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())


ggsave("../IEO_forecasts_material/output/figures/comparison/consensus/comparison_rmse.pdf")


footnote=c("Share of countries with lower RMSE from WEO forecasts compared to Consensus Forecasts") %>% 
  cat(file = "../IEO_forecasts_material/output/figures/comparison/consensus/comparison_rmse_footnote.tex")
  
  
