########### Script to analyse relationship between bias and "big" programs: -----
# Note: number of observations decreases when merging MONA with actual values
# because we exclude programs from 2019 onwards.


# Need actual value of real GDP

load("../IEO_forecasts_material/intermediate_data/rgdp_cleaned.RData")
rgdp_weo <- x

final_mona <- rgdp_weo %>%
  select(country_code,year, targety_first) %>% 
  merge(mona_rgdp, by = c("country_code","year")) %>% 
  as_tibble() 




# Scatterplots relationship amount approved (% of quota) and forecast error: ----


plot_rel_bias_big <- function(variable){
  

variable_quosure <- enquo(variable)  
  
final_mona %>% 
  filter(review == "R0") %>% 
  mutate_at(vars(contains("variable")),funs(targety_first - .)) %>%
  mutate(exceptional_access = case_when(exceptional_access == "Y" ~ "Exceptional",
                                        is.na(exceptional_access) ~ "No info",
                                        T ~ "Normal")) %>% 
  ggplot(aes(amount_percent_quota, !!variable_quosure,col = exceptional_access)) +
  geom_point(size = 5,alpha = 0.6) +
  xlab("") +
  ylab("") +
  geom_smooth(aes(group=1),method = "lm", formula = y ~ x, se = FALSE, col = "red") +
  theme_minimal() +
  ylim(-5,5) +
  ylab("Real Growth Forecast Error (%)") +
  xlab("Total Approved Amount (% of quota)") +
  labs(col = "Access: ") +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18)) +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1)) +
  theme(  axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20),
          axis.title = element_text(size = 22))
}

# Run the function and export: 

plot_rel_bias_big(variable1) %>% 
  ggsave(filename = "../IEO_forecasts_material/output/figures/programs/bias_big/current_year.pdf")

plot_rel_bias_big(variable2) %>% 
  ggsave(filename = "../IEO_forecasts_material/output/figures/programs/bias_big/year_ahead.pdf")

# Footnote:

footnote=c("Includes all programs in the period 2002-2018.") %>% 
  cat(file = "../IEO_forecasts_material/output/figures/programs/bias_big/bias_big_footnote.tex")



# Regressions amount approved (% of quota) and forecast error: -----
# Note: winsorized at 10% level.


regression_data <- final_mona %>% 
  filter(review == "R0") %>% 
  mutate_at(vars(contains("variable")),funs(targety_first - .)) %>%
  mutate_at(vars(contains("variable")),funs(Winsorize(., na.rm = T, probs = c(0.10,0.90)))) %>% 
  mutate(months_remaining = 12 - lubridate::month(date_action)) %>%
  mutate(after = case_when(year > 2009 ~ 1,
                           T ~ 0)) %>% 
  mutate(concessional = case_when(program_type == "SBA"| program_type == "EFF" |
                                    program_type == "PCL"| program_type == "PLL" ~ "Concessional",
                                  T ~ "Non-concessional"))

formulas=c("variable1 ~ 1",
           "variable1 ~ after",
           "variable1 ~ after + amount_percent_quota",
           "variable1 ~ amount_percent_quota + months_remaining",
           "variable1 ~ amount_percent_quota + concessional",
           "variable2 ~ 1",
           "variable2 ~ after",
           "variable2 ~ after + amount_percent_quota",
           "variable2 ~ amount_percent_quota + months_remaining",
           "variable2 ~ amount_percent_quota + concessional")

regressions <- formulas %>% 
  map(~ lm(.x,regression_data))

# Export:

regressions %>%
  stargazer(covariate.labels = c("Post-GFC",
                                 "Total amount (% quota)",
                                 "Remaining months",
                                 "Concessional"),
            dep.var.labels = c("GDP forecast error (current year)","GDP forecast error (year ahead)"),
            omit.stat = c("rsq","adj.rsq","res.dev","ser"),
            column.sep.width = "-10pt",
            df=F,
            out = "../IEO_forecasts_material/output/tables/programs/regressions/gdp.tex")

# Footnote: 

footnote=c("Dependent variable winsorized at the 10% level. Heteroskedasticity robust standard errors in
            parentheses.
           ***: significant at 1% level, **: significant at 5% level,
           *: significant at 10% level.") %>% 
  cat(file = "../IEO_forecasts_material/output/tables/programs/regressions/gdp_footnote.tex")

  
  





# Role of reviews: ----

reviews_data <- final_mona %>% 
  mutate_at(vars(contains("variable")),funs(targety_first - .)) %>%
  mutate_at(vars(contains("variable")),funs(Winsorize(., na.rm = T, probs = c(0.10,0.90)))) %>% 
  mutate(after = case_when(year > 2009 ~ 1,
                           T ~ 0)) %>% 
  mutate(review_dummy = case_when(review != "R0" ~ 1,
                                  T ~ 0)) %>% 
  mutate_at(vars(contains("date")),funs(as.POSIXct(.))) %>% 
  mutate(weeks_passed = round(difftime(date_action, date_approval, units = "weeks"),0)) %>% 
  mutate(concessional = case_when(program_type == "SBA"| program_type == "EFF" |
                                    program_type == "PCL"| program_type == "PLL" ~ "Concessional",
                                  T ~ "Non-concessional")) %>% 
  arrange(country, date_approval,review, year)



# Formulas:

formula_reviews=c("variable1 ~ 1 + review_dummy",
                  "variable1 ~ 1 + review_dummy + after + weeks_passed",
                  "variable1 ~ 1 + review_dummy + after + after*review_dummy + weeks_passed",
                  "variable2 ~ 1 + review_dummy",
                  "variable2 ~ 1 + review_dummy + after + weeks_passed",
                  "variable2 ~ 1 + review_dummy + after + after*review_dummy + weeks_passed")
                  



# Regress and export:

regressions_reviews <- formula_reviews %>% 
  map(~ lm(.x, reviews_data)) %>% 
  map(~ summary(.x))




regressions_reviews %>% 
  stargazer(covariate.labels = c("Review","Post-GFC","Review*Post-GFC"),
            model.numbers = F,
            dep.var.labels = c("GDP forecast error (current year)","GDP forecast error (year-ahead)"),
            omit.stat = c("rsq","adj.rsq","res.dev","ser"),
            df=F,
            out= "../IEO_forecasts_material/output/tables/programs/regressions/gdp_reviews.tex")


# Footnote:

footnote=c("Dependent variable winsorized at the 10% level. The samples R1 and R2 include respectively
           all first and second reviews made in the same year of program approval by the Board.
           Heteroskedasticity robust standard errors in parentheses.
           ***: significant at 1% level, **: significant at 5% level,
           *: significant at 10% level.") %>% 
  cat(file = "../IEO_forecasts_material/output/tables/programs/regressions/gdp_reviews_footnote.tex")



