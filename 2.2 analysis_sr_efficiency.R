############### Efficiency and explaining forecast errors


# Prepare dataframes and set parameters for custom function: -----

# Compute forecast errors:

fe <- final_sr$growth %>% 
  mutate_at(vars(starts_with("variable")),.funs = funs(targety_first - .))  

# List of dataframes with centre countries growth forecasts:

centre_countries_names = c("China","Germany","US")

centre_countries <- final_sr$growth %>%
  filter(country == "United States" | country == "China" | country == "Germany") %>%
  split(.$country) %>% 
  map(~ .x %>% rename_at(vars(starts_with("variable")),.funs = funs(str_replace(.,"variable","centre")))) %>% 
  map(~ .x %>% select(year, centre1, centre2, centre3, centre4))

# Set list regression, filtering condition and identifier for period:

list_regressions=c(variable1 ~ centre1, variable2 ~ centre2,
                   variable3 ~ centre3, variable4 ~ centre4)

conditions = c("year >= 2000 & year < 2010", "year >= 2010" )

period = c("2000-2010","2010-2018")

# Run function and wrangle resulting list into df -----


efficiency_results <- conditions %>% 
  map(~ analyze_sr_efficiency(fe, centre_countries, list_regressions, .x)) %>%
  map(~ .x %>% bind_rows(.id = "centre_country")) %>% 
  map2(period, ~ .x %>% mutate(period = .y)) %>% 
  bind_rows() %>% 
  as_tibble() %>% 
  split(.$centre_country)



# Plot and export: ----

plot_sr_efficiency <- efficiency_results %>%
  map(~ .x %>% ggplot(aes(x=horizon)) +
  geom_point(aes(y=Estimate, col = period), position = position_dodge(0.9), alpha = 0.6) +
  geom_errorbar(aes(ymin=lower, ymax=upper, col = period),position = position_dodge(0.9), width = 0.3, alpha = 0.6) +
  facet_wrap(~ Group) +
  scale_color_manual(values = c("#00008B","#FF0000")) +
  labs(col = "Period") +
  xlab("") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 270,vjust = 0.5)) +
  theme(strip.text.x = element_text(size = 16),
          axis.text.x = element_text(size = 18),
          axis.text.y = element_text(size = 18),
          axis.title = element_text(size = 21),
          legend.text = element_text(size = 16)))
  


plot_sr_efficiency %>%
  iwalk(~ ggsave(paste0("../IEO_forecasts_material/output/figures/short-run forecasts/efficiency/",.y,".pdf"),.x))

# Footnote:

footnote=c("The figure shows the estimated coefficients from the regression of year(t + h) errors in the WEO GDP growth forecasts made in year(t) on an intercept and the year(t) forecast of year(t + h) US GDP growth. 
           The point estimate are reported with 95% confidence intervals. Regression sample divided in two periods.") %>% 
  cat(file = "../IEO_forecasts_material/output/figures/short-run forecasts/efficiency/efficiency_footnote.tex")





