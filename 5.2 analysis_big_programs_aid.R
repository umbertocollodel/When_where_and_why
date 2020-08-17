########### Script to analyse relationship between bias and "big" programs: -----


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


regression_data <- final_mona %>% 
  filter(review == "R0") %>% 
  mutate_at(vars(contains("variable")),funs(targety_first - .)) %>%
  mutate_at(vars(contains("variable")),funs(Winsorize(., na.rm = T))) %>% 
  mutate(months_remaining = 12 - lubridate::month(date_action)) %>% 
  mutate(concessional = case_when(program_type == "SBA"| program_type == "EFF" |
                                    program_type == "PCL"| program_type == "PLL" ~ "Concessional",
                                  T ~ "Non-concessional"))

formulas=c("variable1 ~ amount_percent_quota",
           "variable1 ~ amount_percent_quota + months_remaining",
           "variable1 ~ amount_percent_quota + concessional",
           "variable2 ~ amount_percent_quota",
           "variable2 ~ amount_percent_quota + months_remaining",
           "variable2 ~ amount_percent_quota + concessional")

regressions <- formulas %>% 
  map(~ lm(.x,regression_data))

# Export:

regressions %>%
  stargazer(covariate.labels = c("Total amount (% quota)",
                                 "Remaining months",
                                 "Concessional"),
            dep.var.labels = c("GDP forecast error (current year)","GDP forecast error (year ahaead)"),
            omit.stat = c("rsq","adj.rsq","res.dev","ser"),
            df=F,
            out = "../IEO_forecasts_material/output/tables/programs/regressions/gdp.tex")

# Footnote: 

footnote=c("Dependent variable winsorized at the 5% level.") %>% 
  cat(file = "../IEO_forecasts_material/output/tables/programs/regressions/gdp_footnote.tex")

  
  





# Role of reviews: ----

reviews_data <- final_mona %>% 
  mutate_at(vars(contains("variable")),funs(targety_first - .)) %>%
  mutate_at(vars(contains("variable")),funs(Winsorize(., na.rm = T))) %>% 
  mutate(months_remaining = 12 - lubridate::month(date_action)) %>% 
  mutate(concessional = case_when(program_type == "SBA"| program_type == "EFF" |
                                    program_type == "PCL"| program_type == "PLL" ~ "Concessional",
                                  T ~ "Non-concessional")) %>% 
  arrange(country, date_approval,review, year) %>% 
  mutate(same_year = case_when(year == lubridate::year(date_approval) ~ 1,
                               T ~ 0)) %>% 
  filter(same_year == 1) 

# Export:

reviews_data %>%
  split(.$review) %>% 
  map(~ lm(variable1 ~ amount_percent_quota,.x)) %>% 
  stargazer(covariate.labels = c("Total amount (% quota)"),
            column.labels = c("R0", "R1","R2"),
            model.numbers = F,
            dep.var.labels = rep(c("GDP forecast error (current year)",3)),
            omit.stat = c("rsq","adj.rsq","res.dev","ser"),
            df=F,
            out= "../IEO_forecasts_material/output/tables/programs/regressions/gdp_reviews.tex")




