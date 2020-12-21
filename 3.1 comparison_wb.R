################ Script to compare IMF WEO forecasts with World Bank GEP:

# Prepare comparison dataframe -----
# Note: improve this reading part, switch intermediate files
# to RDS in order to avoid repetition


# World Bank data (complete) from Ayuhan:

gep_data <- readRDS("../When_where_and_why_material/intermediate_data/world bank/gdp_wb_cleaned.rds") %>% 
  filter(!country %in% country_to_exclude)
  

# IMF WEO updates data:

weo_updates <- readRDS("../When_where_and_why_material/intermediate_data/rgdp_update_cleaned.RDS")

# Actual values: (fall issue of next year WEO)

load("../When_where_and_why_material/intermediate_data/rgdp_cleaned.RData")

target <- x %>% 
  select(country_code, country, year, targety_first)

# Geographical group:

geo_group <- readRDS("../When_where_and_why_material/intermediate_data/country_group_geography_clean.RDS")


# Bind all together:

comparison_wb <- list(weo_updates, gep_data, target) %>% 
  reduce(merge, by=c("country_code","country","year")) %>% 
  merge(geo_group, by="country_code") %>% 
  select(country_code, country, year, group, targety_first, everything()) %>%
  arrange(country, year) %>% 
  as_tibble()

comparison_wb %>% 
  saveRDS("../When_where_and_why_material/intermediate_data/world bank/comparison_wb.RDS")

# Dataframe with geographical group countries ----


group <- comparison_wb %>%
  group_by(country_code) %>% 
  slice(1) %>% 
  select(country_code, group) 

# Table: list countries comparison:----

get_list_comparison(comparison_wb, "WB_updated/country_sample.tex")

# Figure: scatterplot WB and IMF forecasts ------

get_scatterplot(comparison_wb, "GEP Forecasts",
                issues=c("currentJun","currentJan","aheadJun","aheadJan"),
                export_path = "WB_updated/accuracy/scatter_")

footnote=c("Red line is 45 degrees line.") %>% 
  cat(file ="../When_where_and_why_material/output/figures/comparison/WB_updated/accuracy/scatter_footnote.tex")

# Table: summary of accuracy (percentage RMSE and significance across geo. group) -----

get_accuracy_summary(comparison_wb, 
                     c("H=0,Jun.", "H=0,Jan.","H=1,Jun.","H=1,Jan."), 
                     "wb",
                     "WB_updated/accuracy/comparison.tex")

footnote=c("Percentage refers to the share of countries with a lower root mean squared error for WEO forecasts
           compared to GEP forecasts.
           DM Test is the test statistic associated with a two-sided Diebold-Mariano test where the null
           is of equal accuracy between forecasts. ***: significant at 1% level, **: significant at 5% level,
           *: significant at 10% level.") %>% 
  cat(file = "../When_where_and_why_material/output/tables/comparison/WB_updated/accuracy/comparison_footnote.tex")

# Table appendix: comparison RMSE for all individual countries ----- 
    
    
    rmse_comparison <- comparison_wb %>% 
      group_by(country_code) %>%
      summarise_at(vars(variable1:wb4), funs(hydroGOF::rmse(.,targety_first))) %>% 
      mutate(country = countrycode(country_code,"imf","country.name")) %>% 
      mutate(ratio1 = (variable1/wb1)- 1,
             ratio2 = (variable2/wb2) - 1,
             ratio3 = (variable3/wb3) - 1,
             ratio4 = (variable4/wb4) - 1) %>% 
      select(country_code,country,contains("ratio"))
    
    # Export:
    
    rmse_comparison %>%
      select(country, contains("ratio")) %>% 
      mutate_at(vars(ratio1:ratio4), funs(round(.,digits = 2))) %>% 
      setNames(c("Country","H=0,Jul.", "H=0,Jan.","H=1,Jul.","H=1,Jan.")) %>% 
      stargazer(summary= F,
                rownames = F,
                out = "../When_where_and_why_material/output/tables/comparison/WB_updated/appendix/rmse_comparison_full.tex")
    
    # Footnote:
    
    footnote=c("This table reports the ratio of the estimated RMSE for the WEO real GDP growth forecasts versus the RMSE for the Global Economic Prospect (GEP) forecasts. 
               We have subtracted one, so that values greater than zero suggest that the WEO forecasts are less accurate than the GEP forecasts, while values below zero suggest that the WEO forecasts are more accurate.") %>% 
      cat(file ="../When_where_and_why_material/output/tables/comparison/WB_updated/appendix/rmse_comparison_full_footnote.tex")
    
# Forecast errors bias and aid ----

wb_aid <- readRDS("../When_where_and_why_material/intermediate_data/world bank/wb_aid_cleaned.RDS")

aid_comparison <- merge(comparison_wb, wb_aid) %>% 
  mutate_at(vars(contains("variable")), funs(targety_first - .)) %>% 
  mutate_at(vars(contains("wb")), funs(targety_first - .)) %>%
  arrange(country, year) %>% 
  as_tibble()


# Table 3: bias and WB engagement ----

aid_comparison %>% 
  mutate_at(vars(contains("aid")), funs(case_when(. > median(.,na.rm = T) ~ 1,
                                                  T ~ 0))) %>%
  gather("type_aid","upper",aid_ibrd:total_aid) %>% 
  split(.$type_aid) %>% 
  map(~ .x %>% group_by(upper)) %>% 
  map(~ .x %>% summarise(median1 = round(median(wb1, na.rm = T),2),
                         median2 = round(median(wb2, na.rm = T),2),
                         median3 = round(median(wb3, na.rm = T),2),
                         median4 = round(median(wb4, na.rm = T),2))) %>% 
  bind_rows(.id = "Source") %>% 
  mutate(upper = case_when(upper == 1 ~ "Extensive",
                           upper == 0 ~ "Normal")) %>% 
  mutate(Source = case_when(Source == "aid_ibrd" ~ "IBRD",
                            Source == "aid_ida" ~ "IDA",
                            T ~ "Total")) %>% 
  setNames(c("Source","Type of Engament","H=0, Jul.","H=0, Jan.", "H=1, Jul.","H=1, Jan.")) %>% 
  stargazer(summary = F,
            rownames = F,
            out = "../When_where_and_why_material/output/tables/comparison/WB_updated/aid_bias/aid_errors.tex")


footnote=c("Extensive engament is defined as total loans outstanding above the median of the distribution
           as of June 2014. H=0, J is the current-year January median forecast error for the group. H=1, J is the year-ahead January 
           median forecast error.") %>% 
  cat(file ="../IEO_forecasts_material/output/tables/comparison/WB_updated/aid_errors_footnote.tex")


# Tables 8 & 9: bias and WB engagement - robutstness with different cut-offs for extensive engagement ----

names=c("75th_percentile","95th_percentile")

c(0.75,0.95) %>% 
  map(~ aid_comparison %>% mutate_at(vars(contains("aid")), funs(case_when(. > quantile(.,.x,na.rm = T) ~ 1,
                                                  T ~ 0)))) %>%
  map(~ .x %>% gather("type_aid","upper",aid_ibrd:total_aid)) %>% 
  map(~ .x %>% split(.$type_aid)) %>% 
  modify_depth(2, ~ .x %>% group_by(upper)) %>% 
  modify_depth(2, ~ .x %>% summarise(median1 = round(median(wb1, na.rm = T),2),
                                     median2 = round(median(wb2, na.rm = T),2),
                                     median3 = round(median(wb3, na.rm = T),2),
                                     median4 = round(median(wb4, na.rm = T),2))) %>% 
  map(~ .x %>% bind_rows(.id = "Source")) %>% 
  map(~ .x %>% mutate(upper = case_when(upper == 1 ~ "Extensive",
                           upper == 0 ~ "Normal"))) %>% 
  map(~ .x %>% mutate(Source = case_when(Source == "aid_ibrd" ~ "IBRD",
                            Source == "aid_ida" ~ "IDA",
                            T ~ "Total"))) %>% 
  map(~ .x %>% setNames(c("Source","Type of Engament","H=0, Jul.","H=0, Jan.","H=1, Jul.","H=1, Jan."))) %>% 
  map2(names, ~ .x %>% stargazer(summary = F,
                         rownames = F,
                         out = paste0("../When_where_and_why_material/output/tables/comparison/WB_updated/aid_bias/aid_errors_",.y,".tex")))


# Figure 7 & 8 - scatterplot bias and WB aid: ----

# Calculate median forecast error throughout period:

list_scatter <- aid_comparison %>% 
  group_by(country) %>% 
  mutate_at(vars(matches("variable|wb")), median, na.rm = T) %>% 
  slice(1) %>%
  gather("type_aid","value",aid_ibrd:total_aid) %>% 
  split(.$type_aid) %>%
  map(~ .x %>% ungroup()) %>% 
  map(~ .x %>% mutate(type_engagement = case_when(value > median(value, na.rm = T) ~ "Extensive",
                                                  T ~ "Normal"))) 

# Custom function to plot different horizons:

plot_rel_bias_aid <- function(variable){
  
  variable_quosure <- enquo(variable)

list_scatter %>% 
  map(~ .x %>% 
  ggplot(aes(value, !!variable_quosure, col = type_engagement)) +
  geom_point(size=5, alpha = 0.6) +
  geom_smooth(method='lm', formula= y~x, se = F, col = "red") +
  theme_minimal() +
  ylab("Real Growth Forecast Error (%)") +
  xlab("Engagement") +
  ylim(-3,2) +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1)) +
  theme(  axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20),
          axis.title = element_text(size = 22)) +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18)) +
  labs(col = "Type: ")
  )
}

# Run the function and export:

plot_rel_bias_aid(wb1) %>%
  iwalk(~ ggsave(filename = paste0("../When_where_and_why_material/output/figures/comparison/WB_updated/aid_bias/",.y,"_currentJul.pdf"),.x))

  
plot_rel_bias_aid(wb2) %>% 
  iwalk(~ ggsave(filename = paste0("../When_where_and_why_material/output/figures/comparison/WB_updated/aid_bias/",.y,"_currentJan.pdf"),.x))


plot_rel_bias_aid(wb3) %>% 
  iwalk(~ ggsave(filename = paste0("../When_where_and_why_material/output/figures/comparison/WB_updated/aid_bias/",.y,"_aheadJul.pdf"),.x))


plot_rel_bias_aid(wb4) %>% 
  iwalk(~ ggsave(filename = paste0("../When_where_and_why_material/output/figures/comparison/WB_updated/aid_bias/",.y,"_aheadJan.pdf"),.x))


# Footnote:

footnote=c("Forecast error is the country-by-country median forecast error in the period 2010-2018. Engagement is total loans outstanding 
           in milions USD as of June 2014. Extensive engament is defined as total loans outstanding above the median of the distribution as of June 2014.") %>% 
  cat(file = "../When_where_and_why_material/output/figures/comparison/WB_updated/aid_error_footnote.tex")





  
  




