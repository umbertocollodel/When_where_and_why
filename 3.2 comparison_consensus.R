############ Comparison between WEO and Consensus foecasts 

# Prepare the data ----

load("../When_where_and_why_material/intermediate_data/consensus/gdp_consensus_cleaned.RData")

consensus <- x %>% 
  rename_at(vars(matches("variable")), funs(str_replace(.,"variable","consensus"))) %>% 
  select(-country)

comparison_consensus <- final_sr$growth %>%
  merge(consensus, by = c("country_code","year")) %>%
  mutate(group = case_when(is.na(group) ~ "Other Adv. Economies",
                           T~ group)) %>% 
  as_tibble()

# Dataframe with geographical groups:

group <- comparison_consensus %>%
  filter(forecaster == "Consensus (Mean)") %>%
  group_by(country_code) %>% 
  slice(1) %>% 
  select(country_code, group) %>% 
  filter(!is.na(group))

# Table with list countries comparison ----

get_list_comparison_consensus(comparison_consensus, "consensus/country_sample.tex")



# Figure: scatterplot Consensus and IMF forecasts ------


get_scatterplot(comparison_consensus %>% filter(forecaster == "Consensus (Mean)"),
                "Consensus (Mean)",
                issues=c("currentOct","currentApr","aheadOct","aheadApr"),
                export_path = "consensus/accuracy/scatter_")

footnote=c("Red line is 45 degrees line.") %>% 
  cat(file ="../When_where_and_why_material/output/figures/comparison/consensus/accuracy/scatter_footnote.tex")



# Table appendix: comparison RMSE for all individual countries ----- 

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
            out = "../When_where_and_why_material/output/tables/comparison/consensus/rmse_comparison_full.tex")
  
# Footnote:

footnote=c("This table reports the ratio of the estimated RMSE for the WEO real GDP growth forecasts versus the RMSE for the Consensus Economics (CE) forecasts. 
We have subtracted one, so that values greater than zero suggest that the WEO forecasts are less accurate than the CE forecasts, while values below zero suggest that the WEO forecasts are more accurate.") %>% 
cat(file ="../When_where_and_why_material/output/tables/comparison/consensus/rmse_comparison_full_footnote.tex")



# Table: summary of accuracy (percentage RMSE and significance across geo. group) -----

get_accuracy_summary(comparison_consensus %>% filter(forecaster == "Consensus (Mean)"), 
                     c("H=0,Oct.", "H=0,Apr.","H=1,Oct.","H=1,Apr."), 
                     "consensus",
                     "consensus/accuracy/comparison.tex")

footnote=c("Percentage refers to the share of countries with a lower root mean squared error for WEO forecasts
           compared to Consesus forecasts.
           DM Test is the test statistic associated with a two-sided Diebold-Mariano test where the null
           is of equal accuracy between forecasts. ***: significant at 1% level, **: significant at 5% level,
           *: significant at 10% level.") %>% 
  cat(file = "../When_where_and_why_material/output/tables/comparison/consensus/accuracy/comparison_footnote.tex")



# Uncertainty of consensus forecasts: ----


comparison_consensus %>% 
  ggplot(aes(x = year)) +
  geom_boxplot(aes(y = consensus1), fill = "darkblue", outlier.size = 0) +
  theme_minimal() +
  xlab("")


# Table: recessions and best forecaster median forecast error ----
# Note: we keep Consensus (mean) in the df



filter_best_forecaster <- function(variable){
  
  quosurize_variable <- enquo(variable)
  
  comparison_consensus %>%
    filter(complete.cases(targety_first)) %>% 
    mutate_at(vars(matches("consensus")), funs(targety_first - .)) %>%
    select(country, year, forecaster, targety_first, !!quosurize_variable) %>% 
    group_by(country,year) %>%
    mutate_at(vars(contains("consensus")), funs(abs = abs(.))) %>% 
    filter(abs == min(abs, na.rm = T)) %>% 
    select(-abs) %>% 
    ungroup()
}



list(filter_best_forecaster(consensus1),filter_best_forecaster(consensus2),
     filter_best_forecaster(consensus3),filter_best_forecaster(consensus4)) %>% 
  map(~ .x %>% mutate(recession = case_when(targety_first < 0 ~ 1,
                               T ~ 0))) %>% 
  map(~ .x %>% group_by(recession)) %>% 
  map(~ .x %>% summarise_at(vars(matches("consensus")), median, na.rm = T)) %>% 
  bind_cols() %>% 
  ungroup() %>% 
  select(recession, matches("consensus")) %>%
  filter(recession ==1) %>% 
  mutate(recession = "Recession") %>% 
  mutate_at(vars(matches("consensus")),round, 2) %>%
  setNames(c("Recession","Current-year, Fall","Current-year, Spring","Year-ahead, Fall","Year-ahead, Spring")) %>% 
  stargazer(summary = F,
            rownames = F,
            out = "../When_where_and_why_material/output/tables/comparison/consensus/comparison_recession_best.tex")
  

# Footnote:

footnote=c("The table shows the median forecast error for the 'best' Consensus forecasters i.e. the one that produce forecasts
           closest to the actual value for recession periods.") %>% 
  cat(file = "../When_where_and_why_material/output/tables/comparison/consensus/comparison_recession_best_footnote.tex")



