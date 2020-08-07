##### Script to analyse relationship between bias and "big" programs: -----



# Scatterplots relationship amount approved (% of quota) and forecast error: ----


first_reg <- final$growth %>%
  select(country_code, country, year, targety_first) %>% 
  merge(final_mona, by = c("country_code","country","year")) %>% 
  as_tibble() %>% 
  filter(precautionary == "N")



plot_rel_bias_big <- function(variable){
  

variable_quosure <- enquo(variable)  
  
first_reg %>% 
  mutate_at(vars(contains("variable")),funs(targety_first - .)) %>%
  filter(exceptional_access != "n.a.") %>% 
  mutate(exceptional_access = case_when(exceptional_access == "Y" ~ "Exceptional",
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

# Footnote: ----

footnote=c("Includes all programs in the period 2002-2018 with the exception of precautionary ones. Exceptional
           access lending from SPR programs sheet. When no information on whether or not exceptional
           access, program excluded.") %>% 
  cat(file = "../IEO_forecasts_material/output/figures/programs/bias_big/bias_big_footnote.tex")



# Regressions amount approved (% of quota) and forecast error: -----

a <- first_reg %>% 
  mutate_at(vars(contains("variable")),funs(targety_first - .)) %>%
  filter(exceptional_access != "n.a.")

lm(variable1 ~ amount_percent_quota, a) %>% 
  summary()




