# Figure 2 - Evolution of forecast errors: (replication of Figure 7 of the previous report) -----


figures_fe <- final_sr %>% 
  map(~ .x %>% mutate(fe2 = targety_first - variable2) %>% select(country_code,country, year, fe2)) %>%
  map(~ .x %>% group_by(year) %>% mutate(mean_fe2 = mean(fe2, na.rm = T), median_fe2 = median(fe2, na.rm = T))) %>% 
  imap(~ .x %>%  mutate(meta_information = .y)) %>% 
  map(~ if(unique(.x$meta_information) == "inflation"){
    .x %>% 
      ggplot(aes(year)) +
        geom_point(aes(y = fe2), alpha = 0.1) +
        geom_line(aes(y = median_fe2, group = 1, color = "Median"), size = 1) +
        geom_hline(yintercept = 0) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1),
              legend.position = "bottom") +
        labs(color = "") +
        xlab("") +
        ylab("") +
        ylim(-5,5)
  }
  else {
    .x %>% 
      ggplot(aes(year)) +
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
  }
  )


figures_fe %>% 
  walk2(names(figures_fe),~ ggsave(paste0("../IEO_forecasts_material/output/figures/forecast_errors/all/",.y,".pdf"),.x))


# Forecast errors with boxplots: (to see distributions) ----
# 
# final %>% 
#   map(~ .x %>% mutate(fe2 = targety_first - variable2) %>% select(country_code,country, year, fe2)) %>%
#   map(~ .x %>% ggplot(aes(year, fe2)) +
#         geom_boxplot(outlier.size = 0) +
#         geom_hline(yintercept = 0) +
#         theme_minimal() +
#         theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1),
#               legend.position = "bottom") +
#         labs(color = "") +
#         xlab("") +
#         ylab("") +
#         ylim(-5,5)
#   )


# Figure 3: Forecast errors by type of economy ----


figures_fe_adv <- final_sr %>%
  map(~ .x %>% mutate(fe2 = targety_first - variable2) %>%  filter(adv == 1) %>% select(country_code,country, year, fe2)) %>%
  map(~ .x %>% group_by(year) %>% mutate(mean_fe2 = mean(fe2, na.rm = T), median_fe2 = median(fe2, na.rm = T))) %>% 
  imap(~ .x %>%  mutate(meta_information = .y)) %>% 
  map(~ if(unique(.x$meta_information) == "inflation"){
    .x %>% 
      ggplot(aes(year)) +
      geom_point(aes(y = fe2), alpha = 0.1) +
      geom_line(aes(y = median_fe2, group = 1, color = "Median"), size = 1) +
      geom_hline(yintercept = 0) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1),
            legend.position = "bottom") +
      labs(color = "") +
      xlab("") +
      ylab("") +
      ylim(-10,10)
  }
  else {
    .x %>% 
      ggplot(aes(year)) +
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
      ylim(-10,10)
  }
  )

figures_fe_adv %>% 
  walk2(names(figures_fe_adv),~ ggsave(paste0("../IEO_forecasts_material/output/figures/forecast_errors/ae/",.y,".pdf"),.x))


figures_fe_eme <- final_sr %>%
  map(~ .x %>% mutate(fe2 = targety_first - variable2) %>%  filter(eme == 1) %>% select(country_code,country, year, fe2)) %>%
  map(~ .x %>% group_by(year) %>% mutate(mean_fe2 = mean(fe2, na.rm = T), median_fe2 = median(fe2, na.rm = T))) %>% 
  imap(~ .x %>%  mutate(meta_information = .y)) %>% 
  map(~ if(unique(.x$meta_information) == "inflation"){
    .x %>% 
      ggplot(aes(year)) +
      geom_point(aes(y = fe2), alpha = 0.1) +
      geom_line(aes(y = median_fe2, group = 1, color = "Median"), size = 1) +
      geom_hline(yintercept = 0) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1),
            legend.position = "bottom") +
      labs(color = "") +
      xlab("") +
      ylab("") +
      ylim(-10,10)
  }
  else {
    .x %>% 
      ggplot(aes(year)) +
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
      ylim(-10,10)
  }
  )

figures_fe_eme %>% 
  walk2(names(figures_fe_eme),~ ggsave(paste0("../IEO_forecasts_material/output/figures/forecast_errors/eme/",.y,".pdf"),.x))


figures_fe_lidc <- final_sr %>%
  map(~ .x %>% mutate(fe2 = targety_first - variable2) %>% filter(lidc == 1) %>% select(country_code,country, year, fe2)) %>%
  map(~ .x %>% group_by(year) %>% mutate(mean_fe2 = mean(fe2, na.rm = T), median_fe2 = median(fe2, na.rm = T))) %>% 
  imap(~ .x %>%  mutate(meta_information = .y)) %>% 
  map(~ if(unique(.x$meta_information) == "inflation"){
    .x %>% 
      ggplot(aes(year)) +
      geom_point(aes(y = fe2), alpha = 0.1) +
      geom_line(aes(y = median_fe2, group = 1, color = "Median"), size = 1) +
      geom_hline(yintercept = 0) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1),
            legend.position = "bottom") +
      labs(color = "") +
      xlab("") +
      ylab("") +
      ylim(-10,10)
  }
  else {
    .x %>% 
      ggplot(aes(year)) +
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
      ylim(-10,10)
  }
  )

figures_fe_lidc %>% 
  walk2(names(figures_fe_lidc),~ ggsave(paste0("../IEO_forecasts_material/output/figures/forecast_errors/lidc/",.y,".pdf"),.x))






# Formal testing: aggregate -----

bias_test_aggregate <- final_sr %>% 
  map(~ .x %>% group_by(year) %>% mutate(fe2 = targety_first - variable2)) %>%
  map(~ .x %>% summarise(median = median(fe2, na.rm = T))) %>% 
  map(~ lm(median ~ 1, .x)) 

bias_test_ae <- final_sr %>% 
  map(~ .x %>% filter(adv == 1)) %>%
  map(~ .x %>% group_by(year) %>% mutate(fe2 = targety_first - variable2)) %>%
  map(~ .x %>% summarise(median = median(fe2, na.rm = T))) %>% 
  map(~ lm(median ~ 1, .x)) 

bias_test_eme <- final_sr %>% 
  map(~ .x %>% filter(adv == 0)) %>% 
  map(~ .x %>% group_by(year) %>% mutate(fe2 = targety_first - variable2)) %>%
  map(~ .x %>% summarise(median = median(fe2, na.rm = T))) %>% 
  map(~ lm(median ~ 1, .x)) 

bias_test_lidc <- final_sr %>% 
  map(~ .x %>% filter(lidc == 1)) %>% 
  map(~ .x %>% group_by(year) %>% mutate(fe2 = targety_first - variable2)) %>%
  map(~ .x %>% summarise(median = median(fe2, na.rm = T))) %>% 
  map(~ lm(median ~ 1, .x)) 


stargazer(bias_test_aggregate$growth, bias_test_ae$growth, bias_test_eme$growth, bias_test_lidc$growth,
          dep.var.caption = "Median forecast error GDP growth",
          omit.stat = c("rsq","adj.rsq","ser"),
          object.names = TRUE,
          column.labels = c("Full sample","AE","EME","Low-income"),
          out = "../IEO_forecasts_material/output/tables/short-run forecasts/bias/formal_test_aggregate_growth.tex")

stargazer(bias_test_aggregate$inflation, bias_test_ae$inflation, bias_test_eme$inflation, bias_test_lidc$inflation,
          dep.var.caption = "Median forecast error Inflation",
          omit.stat = c("rsq","adj.rsq","ser"),
          object.names = TRUE,
          column.labels = c("Full sample","AE","EME","Low-income"),
          out = "../IEO_forecasts_material/output/tables/short-run forecasts/bias/formal_test_aggregate_inflation.tex")



# Formal testing: individual ----

# The export of tables with different names is not working - to check

final_sr %>% 
  map(~ .x %>% mutate(fe2 = targety_first - variable2)) %>%
  map(~ split(.x,.x$country)) %>% 
  modify_depth(2, ~ tryCatch(lm(fe2 ~ 1,.x), error = function(e){
    cat(crayon::red("Could not run regression. Check dataframe \n"))
  })) %>% 
  modify_depth(2, ~ tryCatch(summary(.x), error = function(e){
    cat(crayon::red("Could not run regression. Check dataframe \n"))
  })) %>% 
  modify_depth(2, ~ tryCatch(.x[["coefficients"]], error = function(e){
    cat(crayon::red("Could not run regression. Check dataframe \n"))
  })) %>% 
  map(~ discard(.x, ~ length(.x) != 4)) %>% 
  modify_depth(2, ~ as.data.frame(.x)) %>% 
  map(~ bind_rows(.x, .id = "country")) %>% 
  map(~ .x %>% mutate(Estimate = case_when(`t value` > 1.96 | `t value` < -1.96 ~ str_replace(as.character(Estimate), "$","**"),
                                           (`t value` > 1.68 & `t value` < 1.96) | (`t value` < -1.68 & `t value` > -1.96) ~ str_replace(as.character(Estimate), "$", "*"),
                                           TRUE ~ as.character(Estimate)))) %>% 
  map(~ .x %>% select(country, Estimate)) %>% 
  map(~ .x %>% rename(Constant = Estimate)) %>% 
  .$inflation
  #map2(name_variables, ~ stargazer(summary = F,
  #         rownames = F,
  #         out = paste0("../IEO_forecasts_material/output/tables/short-run forecasts/bias/by_country/",.y,".tex")))
  



# TO finish!



# Table 2: Median forecast errors by income group and horizon (focusing on growth) ----

median_ws <- final_sr[["growth"]] %>%
  filter(year < 2011) %>% 
  mutate_at(vars(starts_with("variable")),.funs = funs(targety_first - .)) %>%
  mutate(recession = case_when(targety_first <= 0 ~ 1,
                               TRUE ~ 0)) %>% 
  group_by(recession) %>% 
  summarise_at(vars(starts_with("variable")),median, na.rm =T) %>% 
  mutate_at(vars(starts_with("variable")),round, 2) %>% 
  mutate(group = "Full sample") %>% 
  mutate(recession = case_when(recession == 0 ~ "Non-recession",
                               recession == 1 ~ "Recession")) %>% 
  select(group, everything())
  
  


median_bg <- final_sr[["growth"]] %>% 
  filter(year < 2011) %>% 
  mutate_at(vars(starts_with("variable")),.funs = funs(targety_first - .)) %>%
  mutate(recession = case_when(targety_first <= 0 ~ 1,
                               TRUE ~ 0)) %>% 
  group_by(adv, recession) %>% 
  summarise_at(vars(starts_with("variable")),median, na.rm =T) %>% 
  mutate_at(vars(starts_with("variable")),round, 2) %>% 
  ungroup() %>% 
  mutate(recession = case_when(recession == 0 ~ "Non-recession",
                               recession == 1 ~ "Recession")) %>% 
  mutate(adv = case_when(adv == 0 ~ "Emerging market economies",
                               adv == 1 ~ "Advanced economies")) %>%
  rename(group = adv)

median_lidc <- final_sr[["growth"]] %>% 
  filter(year < 2011) %>% 
  mutate_at(vars(starts_with("variable")),.funs = funs(targety_first - .)) %>%
  mutate(recession = case_when(targety_first <= 0 ~ 1,
                               TRUE ~ 0)) %>% 
  group_by(lidc, recession) %>% 
  summarise_at(vars(starts_with("variable")),median, na.rm =T) %>% 
  mutate_at(vars(starts_with("variable")),round, 2) %>% 
  ungroup() %>% 
  mutate(recession = case_when(recession == 0 ~ "Non-recession",
                               recession == 1 ~ "Recession")) %>%
  filter(lidc ==1) %>% 
  mutate(lidc = "Low-income") %>%
  rename(group = lidc)
  

previous_evaluation <- rbind(median_ws, median_bg, median_lidc) %>% 
  stargazer(summary = F, 
            out = "../IEO_forecasts_material/output/tables/short-run forecasts/bias/previous_evaluation.tex",
            rownames = F)

# Table 2 highlighting performance over the last period ----

median_ws <- final_sr[["growth"]] %>% 
  filter(year > 2011) %>% 
  mutate_at(vars(starts_with("variable")),.funs = funs(targety_first - .)) %>%
  mutate(recession = case_when(targety_first <= 0 ~ 1,
                               TRUE ~ 0)) %>% 
  group_by(recession) %>% 
  summarise_at(vars(starts_with("variable")),median, na.rm =T) %>% 
  mutate_at(vars(starts_with("variable")),round, 2) %>% 
  mutate(group = "Full sample") %>% 
  mutate(recession = case_when(recession == 0 ~ "Non-recession",
                               recession == 1 ~ "Recession")) %>% 
  select(group, everything())




median_bg <- final_sr[["growth"]] %>%
  filter(year > 2011) %>% 
  mutate_at(vars(starts_with("variable")),.funs = funs(targety_first - .)) %>%
  mutate(recession = case_when(targety_first <= 0 ~ 1,
                               TRUE ~ 0)) %>% 
  group_by(adv, recession) %>% 
  summarise_at(vars(starts_with("variable")),median, na.rm =T) %>% 
  mutate_at(vars(starts_with("variable")),round, 2) %>% 
  ungroup() %>% 
  mutate(recession = case_when(recession == 0 ~ "Non-recession",
                               recession == 1 ~ "Recession")) %>% 
  mutate(adv = case_when(adv == 0 ~ "Emerging market economies",
                         adv == 1 ~ "Advanced economies")) %>%
  rename(group = adv)

median_lidc <- final_sr[["growth"]] %>% 
  filter(year > 2011) %>% 
  mutate_at(vars(starts_with("variable")),.funs = funs(targety_first - .)) %>%
  mutate(recession = case_when(targety_first <= 0 ~ 1,
                               TRUE ~ 0)) %>% 
  group_by(lidc, recession) %>% 
  summarise_at(vars(starts_with("variable")),median, na.rm =T) %>% 
  mutate_at(vars(starts_with("variable")),round, 2) %>% 
  ungroup() %>% 
  mutate(recession = case_when(recession == 0 ~ "Non-recession",
                               recession == 1 ~ "Recession")) %>%
  filter(lidc ==1) %>% 
  mutate(lidc = "Low-income") %>%
  rename(group = lidc)


current_evaluation <- rbind(median_ws,median_bg, median_lidc) %>% 
  stargazer(summary = F,
            out = "../IEO_forecasts_material/output/tables/short-run forecasts/bias/current_evaluation.tex",
            rownames = F)


# More on recessions ----


final_sr$growth %>% 
  mutate(recession = case_when(targety_first < 0 ~ 1,
                               TRUE ~ 0)) %>% 
  group_by(recession) %>% 
  filter(year > 2011) %>% 
  summarise_at(vars(matches("variable")), median, na.rm = T) %>% 
  slice(2) %>% 
  gather("forecast_horizon","forecast",variable1:variable4) %>% 
  mutate(forecast_horizon = seq(1:4)) %>% 
  ggplot(aes(forecast_horizon, forecast)) +
  geom_col() +
  theme_minimal()


# Taper tantrum


final_sr$growth %>% 
  filter(year == 2013) %>% 
  filter(adv == 0) %>% 
  group_by(country) %>% 
  summarise_at(vars(matches("variable")), median, na.rm = T) %>% 
  mutate(rev = ((variable1 - variable4)/variable4)*100) %>% 
  arrange(rev) %>% 
  filter(country %in% c("Mexico","China","Russia","Brazil","India","Argentina","Indonesia","Poland","South Africa","South Korea","Turkey")) %>% 
  gather("forecast_horizon","forecast",variable1:variable4) %>% 
  arrange(country, forecast_horizon) %>% 
  mutate(forecast_horizon = rep(seq(1:4),10)) %>% 
  ggplot(aes(forecast_horizon, forecast, col = country)) +
  geom_line() +
  theme_minimal()
  










