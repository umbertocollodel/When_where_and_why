##### Script to analyse relationship between bias and "big" programs: -----



# First regressions: ----


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






