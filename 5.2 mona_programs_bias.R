########### Script to analyse relationship between bias and "big" programs: -----
# Note: number of observations decreases when merging MONA with actual values
# because we exclude programs from 2019 onwards.


# Need actual value of real GDP

load("../When_where_and_why_material/intermediate_data/rgdp_cleaned.RData")
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
  ggsave(filename = "../When_where_and_why_material/output/figures/programs/bias_big/current_year.pdf")

plot_rel_bias_big(variable2) %>% 
  ggsave(filename = "../When_where_and_why_material/output/figures/programs/bias_big/year_ahead.pdf")

# Footnote:

footnote=c("Includes all programs in the period 2002-2018.") %>% 
  cat(file = "../When_where_and_why_material/output/figures/programs/bias_big/bias_big_footnote.tex")



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
           "variable1 ~ amount_percent_quota",
           "variable1 ~ amount_percent_quota + months_remaining",
           "variable1 ~ amount_percent_quota + concessional",
           "variable2 ~ 1",
           "variable2 ~ after",
           "variable2 ~ amount_percent_quota",
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
            out = "../When_where_and_why_material/output/tables/programs/regressions/gdp.tex")

# Footnote: 

footnote=c("Dependent variable winsorized at the 10% level. Heteroskedasticity robust standard errors in
            parentheses.
           ***: significant at 1% level, **: significant at 5% level,
           *: significant at 10% level.") %>% 
  cat(file = "../When_where_and_why_material/output/tables/programs/regressions/gdp_footnote.tex")

  
  
# Comparison with Consensus -----

# Cleaning consensus programs data: one df with current-year and one with year-ahead

consensus_programs <- c("1:12","13:24") %>% 
  map(~  read_xlsx("../When_where_and_why_material/raw_data/consensus/zidong_program_consensus.xlsx") %>% select(ccode, Country, targety,ggdpa, num_range("cf_ggdp",eval(parse(text=.x))))) %>% 
  imap(~ .x %>% pivot_longer(5:length(.x), "month", values_to = paste0("consensus",.y))) %>% 
  map(~ .x %>% mutate(month = as.numeric(str_extract(month,"\\d+")))) %>% 
  map(~ .x %>% mutate(month = rev(month))) %>%
  map(~ .x %>% mutate_at(vars(contains("consensus")),funs(ggdpa - .))) %>% 
  map(~ .x %>% select(-ggdpa,-Country)) %>% 
  map(~ .x %>% rename(country_code = ccode,
                      year = targety)) %>% 
  map(~ .x %>% mutate(year = as.character(year))) %>% 
  map(~ .x %>% mutate(month = case_when(month == 24 ~ 12,
                                        month == 23 ~ 11,
                                        month == 22 ~ 10,
                                        month == 21 ~ 9,
                                        month == 20 ~ 8,
                                        month == 19 ~ 7,
                                        month == 18 ~ 6,
                                        month == 17 ~ 5,
                                        month == 16 ~ 4,
                                        month == 15 ~ 3,
                                        month == 14 ~ 2,
                                        month == 13 ~ 1,
                                        T ~ month)))

comparison_data <- consensus_programs %>% 
  map(~ merge(.x, regression_data %>% mutate(month = lubridate::month(date_approval)))) %>% 
  map(~ as_tibble(.x)) %>% 
  map(~ .x %>% select(country, year, month, variable1, variable2, contains("consensus"), amount_percent_quota, months_remaining,after, concessional))  


# Creation formulas to run for each of them:

formulas1=c("variable1 ~ amount_percent_quota",
           "consensus1 ~ amount_percent_quota")

formulas2=c("variable2 ~ amount_percent_quota",
            "consensus2 ~ amount_percent_quota")


# Running:

current_year <- formulas1 %>% 
  map(~ lm(.x, comparison_data[[1]] %>% 
  filter(complete.cases(variable1)))) 

year_ahead <- formulas2 %>% 
  map(~ lm(.x, comparison_data[[2]] %>% 
             filter(complete.cases(variable2)))) 

# Plot the results:
  
comparison_plot <- list(current_year, year_ahead) %>% 
  modify_depth(2, ~ data.frame(confint(.x))) %>% 
  modify_depth(2, ~  .x %>% slice(2)) %>% 
  modify_depth(2, ~ .x %>% setNames(c("lower","upper"))) %>% 
  flatten() %>% 
  bind_rows() %>% 
  mutate(forecaster = rep(c("Mona","Consensus"),2)) %>% 
  mutate(horizon = c(rep("Current year",2), rep("Year-ahead",2))) %>% 
  split(.$horizon) %>% 
  map(~ .x %>% 
    ggplot(aes(forecaster, ymin = lower*100, ymax= upper*100, col = forecaster)) +
    geom_errorbar(width = 0.3) +
    coord_flip() +
    theme_minimal() +
    xlab("") +
    scale_y_continuous(limits = c(-0.5,0.25),breaks = c(-0.5,-0.25,0,0.25)) +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1)) +
    theme(panel.spacing.x=unit(1.5, "lines"),panel.spacing.y=unit(1.5, "lines")) +
    theme(panel.grid.major.y = element_blank()) +
    theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 21))) 

comparison_plot %>% 
  iwalk(~ ggsave(paste0("../When_where_and_why_material/output/figures/programs/bias_big/comparison_bias_big_",.y,".pdf"),.x))


# Footnote:
  
  footnote=c("The figure shows 95% confidence intervals obtained regressing the forecast
             errors for programs on the amount of the program (in % of country quota) and
             multiplying by 100.
             Data for forecasts are, respectively, from MONA and Consensus. 
             The sample of programs corresponds to data availability for Consensus") %>% 
    cat(file= "../When_where_and_why_material/output/figures/programs/bias_big/comparison_bias_big_footnote.tex")
  
# Role of reviews: ----



reviews_data <- final_mona %>% 
  mutate_at(vars(contains("variable")),funs(targety_first - .)) %>%
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
                  "variable1 ~ 1 + review_dummy + after",
                  "variable1 ~ 1 + review_dummy + after + weeks_passed",
                  "variable2 ~ 1 + review_dummy",
                  "variable2 ~ 1 + review_dummy + after",
                  "variable2 ~ 1 + review_dummy + after + weeks_passed")
                  



# Regress and export:

regressions_reviews <- formula_reviews %>% 
  map(~ lm(.x, reviews_data))



regressions_reviews %>% 
  stargazer(covariate.labels = c("Review","Post-GFC","Weeks passed"),
            model.numbers = F,
            dep.var.labels = c("GDP forecast error (current year)","GDP forecast error (year-ahead)"),
            omit.stat = c("rsq","adj.rsq","res.dev","ser"),
            df=F,
            out= "../When_where_and_why_material/output/tables/programs/regressions/gdp_reviews.tex")


# Footnote:

footnote=c("Dependent variable winsorized at the 5% level. The sample contains data at the inception of the
            program and for the two subsequent reviews.
           ***: significant at 1% level, **: significant at 5% level,
           *: significant at 10% level.") %>% 
  cat(file = "../When_where_and_why_material/output/tables/programs/regressions/gdp_reviews_footnote.tex")



# New path for Figures recession & non-recession years: ----


a <- c("variable1","variable2") %>% 
  map(~ reviews_data %>% filter(review == "R0" & complete.cases(get(.x)))) %>% 
  map(~ .x %>% mutate(month = lubridate::month(date_approval))) %>% 
  map(~ .x %>% remove_empty()) %>% 
  map(~ .x %>% rename_at(vars(contains("2")), funs(str_replace(.,"2","1"))))
  
  

a_cf <- c("1:12","13:24") %>% 
  map(~  read_xlsx("~/Desktop/consensus_data.xlsx") %>% select(ccode, Country, targety,ggdpa, num_range("cf_ggdp",eval(parse(text=.x))))) %>% 
  map(~ .x %>% gather("month","consensus1",5:length(.x))) %>% 
  map(~ .x %>% mutate(month = as.numeric(str_extract(month,"\\d+")))) %>% 
  map(~ .x %>% mutate(month = rev(month))) %>%
  map(~ .x %>% rename(country_code = ccode,
         year = targety)) %>% 
  map(~ .x %>% mutate(year = as.character(year))) %>% 
  map(~ .x %>% mutate(consensus1 = ggdpa - consensus1)) %>% 
  map(~ .x %>% mutate(month = case_when(month == 24 ~ 12,
                                        month == 23 ~ 11,
                                        month == 22 ~ 10,
                                        month == 21 ~ 9,
                                        month == 20 ~ 8,
                                        month == 19 ~ 7,
                                        month == 18 ~ 6,
                                        month == 17 ~ 5,
                                        month == 16 ~ 4,
                                        month == 15 ~ 3,
                                        month == 14 ~ 2,
                                        month == 13 ~ 1,
                                        T ~ month)))


comparison <- a %>% 
 map2(a_cf, ~ merge(.x,.y, by=c("country_code","year","month"))) %>% 
 map(~ .x %>% as_tibble()) 



comparison %>% 
 map(~ .x %>% select(country_code, Country, year, month, date_approval, variable1, consensus1, ggdpa)) %>% 
 map(~ .x %>% gather("forecaster","value",variable1:consensus1)) %>% 
 map(~ .x %>% mutate(forecaster = case_when(forecaster == "consensus1" ~ "Consensus (Mean)",
                               T ~ "IMF Mona"))) %>% 
 map(~ .x %>% mutate(recession = case_when(ggdpa > 0  ~ "Non-recession", T ~ "Recession"))) %>% 
 map(~ .x %>% ggplot(aes(x=value, fill = recession)) + 
  geom_density(col = "white",alpha = 0.4) + 
  facet_wrap(~ forecaster) +
  scale_fill_manual(values = c("#0000ff","#ff0000")) +
  xlab("") +
  ylab("") +
  labs(fill="") +
  xlim(-30,10) +
  theme_minimal() +
  theme(legend.position = "bottom") +
    theme(axis.text = element_text(size = 18),
          axis.title = element_text(size = 21),
          strip.text.x = element_text(size=14),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 16))) %>% 
  iwalk(~ ggsave(paste0("../When_where_and_why_material/output/figures/programs/comparison_",.y,".pdf"),.x))


# Footnote:

footnote=c("The figure shows the distribution of forecast errors for all programs in the period 2002-2018.
            Forecast for the current year in the Mona database are formulated at the date of program inception. 
            Consensus forecasts are matched by month.") %>% 
  cat(file = "../When_where_and_why_material/output/figures/programs/comparison_footnote.tex")


# TO DO: formal Kolgorov-Smirnov test and put consensus data in appropriate raw_data folder.

# Kolgorov-Smirnov: -----


names(comparison) <- c("Current Year","Year-ahead")

comparison %>% 
  map(~ .x %>% select(country, year, variable1,consensus1)) %>%
  map(~ .x %>% filter(complete.cases(consensus1))) %>%
  map(~ suppressWarnings(ks.test(.x$variable1, .x$consensus1))) %>% 
  map(~ data.frame(`KS Test Statistic` = .x$statistic,`KS Test P-value`= .x$p.value)) %>% 
  bind_rows(.id = "Horizon") %>% 
  stargazer(summary = F,
            nrow = F,
            out = "../When_where_and_why_material/output/tables/programs/kolgorov_smirnov.tex")
