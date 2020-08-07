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


# Figure 1: comparison of median forecast error: ----

evolution_median_fe <- comparison_ec %>%  
  mutate_at(vars(contains("ec")),funs(targety_first - .)) %>% 
  mutate_at(vars(contains("variable")),funs(targety_first - .)) %>% 
  group_by(year) %>% 
  mutate_at(vars(matches("variable|ec")), median, na.rm = T)


plot_evolution <- function(variable1, variable2){
  
  variable1_quosure <- enquo(variable1)
  variable2_quosure <- enquo(variable2)
  
  evolution_median_fe %>% 
    filter(complete.cases(variable2)) %>% 
    ggplot(aes(year)) +
    geom_line(aes(y = !!variable1_quosure, group = 1, col = "IMF"), size = 1) +
    geom_line(aes(y = !!variable2_quosure, group = 1, col = "EC" ), size = 1) +
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
    ylim(-3,2)
  
}

plot_evolution(variable1, ec1) %>% 
  save.plot("../IEO_forecasts_material/output/figures/comparison/EC/evolution_bias/current_year_fall_comparison.pdf")

plot_evolution(variable2, ec2) %>% 
  save.plot("../IEO_forecasts_material/output/figures/comparison/EC/evolution_bias/current_year_spring_comparison.pdf")

plot_evolution(variable3, ec3) %>% 
  save.plot("../IEO_forecasts_material/output/figures/comparison/EC/evolution_bias/year_ahead_fall_comparison.pdf")

plot_evolution(variable4, ec4) %>% 
  save.plot("../IEO_forecasts_material/output/figures/comparison/EC/evolution_bias/year_ahead_spring_comparison.pdf")


# Footnote:

footnote=c("The figure shows the median forecast error by institution and year.") %>% 
  cat(file ="../IEO_forecasts_material/output/figures/comparison/EC/evolution_bias/evolution_bias_footnote.tex")


# Figure 3: share of countries with lower RMSE for IMF by region (in this case only advanced economies) ----

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


# Footnote:

footnote=c("The figure shows the share of countries for which WEO forecasts produce a lower RMSE
           compared to AMECO forecasts.") %>% 
  cat(file = "../IEO_forecasts_material/output/figures/comparison/EC/comparison_rmse_group_footnote.tex")

# Figure 4: magnitude of RMSE difference by geographical group (in this case only advanced economies): -----

rmse_comparison %>% 
  summarise_at(vars(contains("ratio")), median, na.rm = T) %>% 
  gather("horizon","value",ratio1:ratio4) %>% 
  mutate(value = value*100) %>%
  mutate(horizon = case_when(horizon == "ratio1"~ "H=0,Fall",
                             horizon == "ratio2"~ "H=0,Spring",
                             horizon == "ratio3"~ "H=1,Fall",
                             T~ "H=1,Spring")) %>% 
  ggplot(aes(horizon, value)) +
  geom_col(width = 0.3, alpha = 0.6) +
  theme_minimal() +
  ylab("% of AMECO RMSE") +
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

ggsave("../IEO_forecasts_material/output/figures/comparison/EC/comparison_rmse_group_magnitude.pdf")


# Footnote:

footnote=c("The figure shows the median (country-by-country) difference between WEO root mean squared error and AMECO root mean squared error as a percentage
           of the latter.") %>% 
  cat(file ="../IEO_forecasts_material/output/figures/comparison/EC/comparison_rmse_group_magnitude_footnote.tex")



# Table appendix: comparison RMSE for all individual countries ----- 


rmse_comparison <- comparison_ec %>% 
  group_by(country_code) %>%
  summarise_at(vars(variable1:ec4), funs(hydroGOF::rmse(.,targety_first))) %>% 
  mutate(country = countrycode(country_code,"imf","country.name")) %>% 
  mutate(ratio1 = (variable1/ec1)- 1,
         ratio2 = (variable2/ec2) - 1,
         ratio3 = (variable3/ec3) - 1,
         ratio4 = (variable4/ec4) - 1) %>% 
  select(country_code,country,contains("ratio"))

# Export:

rmse_comparison %>%
  select(country, contains("ratio")) %>% 
  mutate_at(vars(ratio1:ratio4), funs(round(.,digits = 2))) %>% 
  setNames(c("Country","H=0,Jul.", "H=0,Jan.","H=1,Jul.","H=1,Jan.")) %>% 
  stargazer(summary= F,
            rownames = F,
            out = "../IEO_forecasts_material/output/tables/comparison/EC/full_rmse.tex")
# Footnote:

footnote=c("This table reports the ratio of the estimated RMSE for the WEO real GDP growth forecasts versus the RMSE for the AMECO forecasts. 
           We have subtracted one, so that values greater than zero suggest that the WEO forecasts are less accurate than the AMECO forecasts, while values below zero suggest that the WEO forecasts are more accurate.") %>% 
  cat(file ="../IEO_forecasts_material/output/tables/comparison/EC/full_rmse_footnote.tex")




# Table 1: forecast errors of both institutions during recessions and non-recessions ---- 

main_table <- comparison_ec %>% 
  mutate_at(vars(contains("ec")),funs(targety_first - .)) %>% 
  mutate_at(vars(contains("variable")),funs(targety_first - .)) %>% 
  mutate(recession = case_when(targety_first < 0 ~ 1,
                               T ~ 0)) %>% 
  group_by(recession) %>%
  summarise_at(vars(matches("variable|ec")),funs(round(median(.,na.rm = T),2))) %>% 
  select(recession, ec1, variable1, ec2, variable2, ec3, variable3, ec4, variable4) %>% 
  setNames(c("Recession",
             "Current-year, Fall (EC)","Current-year, Fall (IMF)",
             "Current-year, Spring (EC)","Current-year, Spring (IMF)",
             "Year-ahead, Fall (EC)","Year-ahead, Fall (IMF)",
             "Year-ahead, Spring (EC)","Year-ahead, Spring (IMF)")) %>%
  mutate(Recession = case_when(Recession == 1 ~ "Recession",
                               T ~ "Non-recession"))


issues=c("Fall","Spring")

issues %>% 
  map(~ main_table %>% select(Recession, contains(.x))) %>% 
  map2(issues, ~ .x %>% stargazer(rownames = F,
                                  summary = F,
                                  out = paste0("../IEO_forecasts_material/output/tables/comparison/EC/recession_forecast_error_",.y,".tex")))





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
  
