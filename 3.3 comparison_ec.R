# ################ Script to compare IMF WEO forecasts with European Commission AMECO:

# Prepare dataframe: ----

load("../IEO_forecasts_material/intermediate_data/rgdp_cleaned.RData")


ec <- readRDS("../IEO_forecasts_material/intermediate_data/european commission/ec_gdp_cleaned.rds") %>%
  rename_at(vars(matches("variable")), funs(str_replace(.,"variable","ec"))) %>% 
  select(-country)

# Geographical group:


geo_group <- readRDS("../IEO_forecasts_material/intermediate_data/country_group_geography_clean.RDS")


comparison_ec <- x %>% 
  merge(ec, by=c("country_code","year")) %>%
  merge(geo_group,by=c("country_code"), all.x = T) %>% 
  select(country_code, country,group, year, targety_first, variable1, variable2, variable3, variable4,
         ec1,ec2,ec3,ec4) %>% 
  as_tibble() 

# Dataframe with geographical group countries ----


group <- comparison_ec %>%
  group_by(country_code) %>% 
  slice(1) %>% 
  select(country_code, group) 

# Table with list countries comparison:----

get_list_comparison(comparison_ec, "EC/country_sample.tex")

# Figure: scatterplot EC and IMF forecasts ------

get_scatterplot(comparison_ec, "EC Forecasts",
                ylimits = c(-5,5),
                xlimits = c(-5,5),
                issues=c("currentOct","currentApr","aheadOct","aheadApr"),
                export_path = "EC/accuracy/scatter_")

footnote=c("Red line is 45 degrees line.") %>% 
  cat(file ="../IEO_forecasts_material/output/figures/comparison/EC/accuracy/scatter_footnote.tex")

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
  setNames(c("Country","H=0,Oct.", "H=0,Apr.","H=1,Oct.","H=1,Apr.")) %>% 
  stargazer(summary= F,
            rownames = F,
            out = "../IEO_forecasts_material/output/tables/comparison/EC/full_rmse.tex")
# Footnote:

footnote=c("This table reports the ratio of the estimated RMSE for the WEO real GDP growth forecasts versus the RMSE for the AMECO forecasts. 
           We have subtracted one, so that values greater than zero suggest that the WEO forecasts are less accurate than the AMECO forecasts, while values below zero suggest that the WEO forecasts are more accurate.") %>% 
  cat(file ="../IEO_forecasts_material/output/tables/comparison/EC/full_rmse_footnote.tex")


# Table: summary of accuracy (percentage RMSE and significance across geo. group) -----

get_accuracy_summary(comparison_ec, 
                     c("H=0,Oct.", "H=0,Apr.","H=1,Oct.","H=1,Apr."), 
                     "ec",
                     "EC/accuracy/comparison.tex")

footnote=c("Percentage refers to the share of countries with a lower root mean squared error for WEO forecasts
           compared to AMECO forecasts.
           DM Test is the test statistic associated with a two-sided Diebold-Mariano test where the null
           is of equal accuracy between forecasts. ***: significant at 1% level, **: significant at 5% level,
           *: significant at 10% level.") %>% 
  cat(file = "../IEO_forecasts_material/output/tables/comparison/EC/accuracy/comparison_footnote.tex")








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
  





