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


# Full table:

rmse_comparison <- comparison_consensus %>% 
  filter(forecaster == "Consensus (Mean)") %>% 
  group_by(country_code) %>%
  select(country_code, country, year, targety_first, variable1:variable4, consensus1:consensus4, group) %>% 
  summarise_at(vars(variable1:consensus4), funs(hydroGOF::rmse(.,targety_first))) %>% 
  mutate(country = countrycode(country_code,"imf","country.name")) %>% 
  mutate(ratio1 = variable1/consensus1 - 1,
         ratio2 = variable2/consensus2 - 1,
         ratio3 = variable3/consensus3 - 1,
         ratio4 = variable4/consensus4 - 1) %>% 
  select(country_code,country,contains("ratio"))

# Export


rmse_comparison %>%
  select(country, contains("ratio")) %>% 
  mutate_at(vars(ratio1:ratio4), funs(round(.,digits = 2))) %>% 
  setNames(c("Country","H=0,F","H=0,S","H=1,F","H=1,S")) %>% 
  stargazer(summary= F,
            rownames = F,
            out = "../IEO_forecasts_material/output/tables/comparison/consensus/rmse_comparison_full.tex")
  
# Figure with different groups:
# Enhancement: add the number of countries by plot

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
  geom_col(width = 0.3,col = "lightgrey",alpha = 0.6) +
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

# Uncertainty of consensus forecasts: ----


comparison_consensus %>% 
  ggplot(aes(x = year)) +
  geom_boxplot(aes(y = consensus1), fill = "darkblue", outlier.size = 0) +
  theme_minimal() +
  xlab("")


# Recessions ----



comparison_consensus %>% 
  filter(forecaster == "Consensus (Mean)") %>% 
  mutate(Recession = case_when(targety_first < 0 ~ "Recession",
                               T ~ "Non-Recession")) %>% 
  group_by(Recession) %>%
  select(country_code, year, country, targety_first, contains("variable"), contains("consensus")) %>% 
  mutate_at(vars(matches("variable|consensus")), funs(targety_first - .)) %>% 
  summarise_at(vars(matches("variable|consensus")), median, na.rm = T) %>%
  mutate_at(vars(matches("variable|consensus")), round, 2) %>%
  stargazer(summary = F,
            rownames = F,
            out = "../IEO_forecasts_material/output/tables/comparison/consensus/comparison_recession.tex"
            )

# Recessions and best forecaster ----

best_forecaster <- comparison_consensus %>%
  filter(complete.cases(targety_first)) %>% 
  mutate_at(vars(matches("variable|consensus")), funs(targety_first - .)) %>%
  select(country, year, forecaster, targety_first, variable1, consensus1) %>% 
  group_by(country,year) %>%
  mutate(consensus1 = case_when(is.na(consensus1) ~ 10,
                                T ~ consensus1)) %>%
  mutate(consensus1 = abs(consensus1)) %>% 
  filter(consensus1 == min(abs(consensus1))) 


best_forecaster %>%
  filter(country == "Ukraine")
  mutate(recession = case_when(targety_first < 0 ~ 1,
                               T ~ 0)) %>% 
  filter(recession == 1) %>% 
  select(country, year, variable1, consensus1) %>% 
  print(n = Inf)



