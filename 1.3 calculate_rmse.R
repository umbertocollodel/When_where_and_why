# Sample: -----

sample_first <- final %>% 
  map(~ .x %>% group_by(year) %>% summarise(n_first= length(unique(country_code))))

sample_last <- final %>% 
  map(~ .x %>% filter(complete.cases(targety_last)) %>% group_by(year) %>% summarise(n = length(unique(country_code))))

sample <- sample_first %>% 
  map2(sample_last, ~ merge(.x,.y, by=c("year")))

figures_sample <- sample %>% 
  map(~ .x %>% 
        rename(`Last (apr 2020)` = n, `First settled` = n_first) %>% 
        gather("type","value",2:ncol(.)) %>% 
        ggplot(aes(year,value,col = type, group = type))+
        geom_line() +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1)) +
        labs(color = "Actual") +
        xlab("") +
        ylab("") 
        ) 

names(figures_sample)

# Export:

figures_sample %>% 
  map2(names(figures_sample),~ ggsave(paste0("../IEO_forecasts_material/output/figures/sample/",.y,".pdf"),.x))



# Forecast errors: -----

# Calculate mean and median by year:

expected_fe2 <- final %>% 
  map(~ .x %>% group_by(year) %>% mutate(fe2 = targety_first - variable2)) %>% 
  map(~ .x %>% summarise(mean_fe2 = mean(fe2, na.rm = T), median_fe2 = median(fe2, na.rm = T)))


final %>% 
  map(~ .x %>% mutate(fe2 = targety_first - variable2) %>% select(country_code,country, year, fe2)) %>%
  map2(expected_fe2, ~ .x %>% merge(.y)) %>% 
  map(~ .x %>% ggplot(aes(year)) +
  geom_point(aes(y = fe2), alpha = 0.1) +
  geom_line(aes(y = mean_fe2, group = 1, color = "Mean"),size = 1) +
  geom_line(aes(y = median_fe2, group = 1, color = "Median"), size = 1) +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1),
        legend.position = "bottom") +
  labs(color = "") +
  xlab("") +
  ylab("") +
  ylim(-5,5)
  )






# Calculation RMSE 

a <- final %>% 
  map(~ .x %>% select(year, targety_first, variable1) %>%  group_by(year) %>% summarise(rmse_first = hydroGOF::rmse(targety_first, variable1, na.rm = T))) 

b <- final %>% 
  map(~ .x %>% select(year, targety_last, variable1) %>%  group_by(year) %>% summarise(rmse_last = hydroGOF::rmse(targety_last, variable1, na.rm = T))) 

a[[1]] %>%
  merge(b[[1]]) %>%
  gather("actual","rmse",rmse_first:ncol(.)) %>% 
  ggplot(aes(year,rmse,col = actual)) +
  geom_point() 
