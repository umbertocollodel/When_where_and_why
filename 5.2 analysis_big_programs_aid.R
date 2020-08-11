########### Script to analyse relationship between bias and "big" programs: -----


# Need actual value of real GDP

load("../IEO_forecasts_material/intermediate_data/rgdp_cleaned.RData")
rgdp_weo <- x

final_mona <- rgdp_weo %>%
  select(country_code,year, targety_first) %>% 
  merge(mona_rgdp, by = c("country_code","year")) %>% 
  as_tibble() 



# Simple graph of median forecast error: ----

final_mona %>% 
  filter(!is.na(exceptional_access)) %>% 
  group_by(program_type) %>% 
  mutate_at(vars(contains("variable")),funs(targety_first - .)) %>% 
  summarise_at(vars(contains("variable")),median,na.rm = T) %>% 
  mutate(concessional = case_when(program_type == "SBA"| program_type == "EFF" |
                                  program_type == "PCL"| program_type == "PLL" ~ "Concessional",
                                  T ~ "Non-concessional")) %>%
  gather("horizon","value",variable1:variable2) %>% 
  split(.$horizon) %>% 
  map(~ .x %>% arrange(value)) %>% 
  map(~ .x %>% mutate(program_type = reorder(as.factor(program_type), value))) %>% 
  map(~ .x %>% 
        ggplot(aes(program_type, value, fill = concessional)) +
        geom_col(width = 0.2) +
        labs(fill = "") +
        xlab("") +
        ylab("Forecast error (%)") +
        theme_minimal() +
        theme(legend.position = "bottom") +
        theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1)) + 
        theme(panel.grid.major.x = element_blank()))
  

# Scatterplots relationship amount approved (% of quota) and forecast error: ----


plot_rel_bias_big <- function(variable){
  

variable_quosure <- enquo(variable)  
  
final_mona %>% 
  mutate_at(vars(contains("variable")),funs(targety_first - .)) %>%
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


regression_data <- final_mona %>% 
  mutate_at(vars(contains("variable")),funs(targety_first - .)) %>%
  filter(!is.na(exceptional_access)) %>% 
  mutate(months_remaining = 12 - lubridate::month(date))

formulas=c("variable1 ~ amount_percent_quota",
           "variable2 ~ amount_percent_quota",
           "variable1 ~ amount_percent_quota + months_remaining",
           "variable2 ~ amount_percent_quota + months_remaining")

formulas %>% 
  map(~ lm(.x,regression_data)) %>% 
  map(~ summary(.x))



