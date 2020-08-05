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
  walk2(names(figures_fe),~ ggsave(paste0("../IEO_forecasts_material/output/figures/short-run forecasts/bias/evolution/all/",.y,".pdf"),.x))


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
  walk2(names(figures_fe_adv),~ ggsave(paste0("../IEO_forecasts_material/output/figures/short-run forecasts/bias/evolution/ae/",.y,".pdf"),.x))


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
  walk2(names(figures_fe_eme),~ ggsave(paste0("../IEO_forecasts_material/output/figures/short-run forecasts/bias/evolution/eme/",.y,".pdf"),.x))


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
  walk2(names(figures_fe_lidc),~ ggsave(paste0("../IEO_forecasts_material/output/figures/short-run forecasts/bias/evolution/lidc/",.y,".pdf"),.x))






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


list_regressions=c(variable1 ~ 1, variable2 ~ 1,
                   variable3 ~ 1, variable4 ~ 1)


regressions <- final_sr$growth %>% 
  mutate_at(vars(contains("variable")),funs(targety_first - .)) %>%
  split(.$country_code) %>%
  map( ~ map(list_regressions, function(x){
      tryCatch(lm(x, .x), error = function(e){
        cat(crayon::red("Could not run the regression. Check data\n"))
      })
    }))  


df_bias <- regressions %>% 
  modify_depth(2, ~ tryCatch(summary(.x), error = function(e){
    cat(crayon::red("Could not run regression. Check dataframe \n"))
  })) %>% 
  modify_depth(2, ~ tryCatch(.x[["coefficients"]], error = function(e){
    cat(crayon::red("Could not run regression. Check dataframe \n"))
  })) %>% 
  map(~ discard(.x, ~ length(.x) != 4)) %>% 
  modify_depth(2, ~ as.data.frame(.x)) %>% 
  map(~ bind_rows(.x, .id = "horizon")) %>% 
  bind_rows(.id = "country_code") %>% 
  mutate(issue = case_when(horizon == 1 | horizon == 3  ~ "Fall",
                           T ~ "Spring"),
         horizon = case_when(horizon == 1 | horizon == 2 ~ "H=0",
                             horizon == 3 | horizon == 4 ~ "H=1")) %>% 
  mutate(Estimate = round(Estimate,2)) %>% 
  mutate(Estimate = case_when(`t value` > 1.96 | `t value` < -1.96 ~ str_replace(as.character(Estimate), "$","**"),
                              (`t value` > 1.68 & `t value` < 1.96) | (`t value` < -1.68 & `t value` > -1.96) ~ str_replace(as.character(Estimate), "$", "*"),
                              TRUE ~ as.character(Estimate))) %>% 
  mutate(country = countrycode(country_code,"imf","country.name")) %>% 
  select(country_code, country, horizon, issue, Estimate)



# Table appendix: individual countries biases for each forecast horizon ----


df_bias %>%
  unite("horizon",horizon, issue, sep = ",") %>%
  select(-country_code) %>% 
  spread(horizon, Estimate) %>%
  rename(Country = country) %>% 
  stargazer(summary = F, 
            rownames = F,
            out = paste0("../IEO_forecasts_material/output/tables/short-run forecasts/bias/by_country/growth.tex"))

# Share of countries with bias by horizon ----

share_aggregate <- df_bias %>% 
  split(.$horizon) %>% 
  map(~ .x %>% mutate(negative_significant = case_when(str_detect(Estimate, "\\*") & str_detect(Estimate, "-") ~ 1,
                                                       T ~ 0))) %>% 
  map(~ .x %>% mutate(positive_significant = case_when(str_detect(Estimate, "\\*") & str_detect(Estimate, "-", negate = T) ~ 1,
                                                       T ~ 0))) %>%
  map(~ .x %>% split(.$issue)) %>% 
  modify_depth(2, ~ .x %>% summarise_at(vars(contains("significant")), mean, na.rm = T)) %>% 
  map(~ .x %>% bind_rows(.id = "issue")) %>%
  bind_rows(.id = "horizon") %>% 
  gather("sign","share",negative_significant:positive_significant) %>% 
  mutate(sign = case_when(sign == "negative_significant" ~ "Optimistic",
                          T ~ "Pessimistic")) %>%
  split(.$issue) %>% 
  map(~ .x %>% 
        ggplot(aes(sign, share, fill = sign)) +
        geom_col(width = 0.4) +
        geom_text(aes(label = round(share,2)), size = 5, vjust = -0.5) +
        facet_wrap(~ horizon) +
        theme_minimal() +
        ylim(0,1)  +
        xlab("") +
        ylab("Share of countries (%)") +
        theme(legend.position = "bottom") +
        theme(strip.text.x = element_text(size = 16),
              axis.text.x = element_blank(),
              axis.text.y = element_text(size = 18),
              axis.title = element_text(size = 21),
              legend.text = element_text(size = 16)) +
        theme(panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank()) +
        labs(fill = ""))

# Export:

share_aggregate %>% 
  iwalk(~ ggsave(filename = paste0("../IEO_forecasts_material/output/figures/short-run forecasts/bias/aggregate/",.y,".pdf"),.x))

# Footnote:

footnote=c("The figure shows the share of countries for each forecast horizon and issue of the World Economic
           Outlook (Fall or Spring) with a statistically signicant negative and positive bias. Test of statistical
           significance is run individually with country-by-country regressions.") %>% 
  cat(file = "../IEO_forecasts_material/output/figures/short-run forecasts/bias/aggregate/aggregate_footnote.tex")


# Magnitude of bias ----

table_magnitude <- df_bias %>% 
  split(.$horizon) %>% 
  map(~ .x %>% filter(str_detect(Estimate,"\\*"))) %>%
  map(~ .x %>% mutate(negative = case_when(str_detect(Estimate,"-") ~ 1,
                                           T ~ 0))) %>% 
  map(~ .x %>% mutate(Estimate = as.numeric(str_remove(Estimate, "\\*+")))) %>% 
  map(~ .x %>% group_by(negative) %>% summarise(mean_bias = round(mean(Estimate, na.rm = T),2),
                                                median_bias = round(median(Estimate, na.rm = T),2),
                                                max_bias = round(max(Estimate),2),
                                                min_bias = round(min(Estimate),2))) %>% 
  bind_rows(.id = "horizon") %>% 
  mutate(negative = case_when(negative == 0 ~ "Optimistic",
                              T ~ "Pessimistic")) %>% 
  arrange(negative) %>% 
  setNames(c("Horizon","Type of bias","Mean","Median", "Min.", "Max."))


table_magnitude %>% 
  stargazer(summary = F,
            rownames = F,
            out = "../IEO_forecasts_material/output/tables/short-run forecasts/bias/magnitude_aggregate_bias.tex")





# Geographical subvidision biases: ----

share_aggregate_group <- df_bias %>% 
  merge(geo_group,by=c("country_code")) %>%
  split(.$horizon) %>% 
  map(~ .x %>% mutate(negative_significant = case_when(str_detect(Estimate, "\\*") & str_detect(Estimate, "-") ~ 1,
                                                       T ~ 0))) %>% 
  map(~ .x %>% mutate(positive_significant = case_when(str_detect(Estimate, "\\*") & str_detect(Estimate, "-", negate = T) ~ 1,
                                                       T ~ 0))) %>%
  map(~ .x %>% split(.$issue)) %>% 
  modify_depth(2, ~ .x %>% group_by(group)) %>% 
  modify_depth(2, ~ .x %>% summarise_at(vars(contains("significant")), mean, na.rm = T)) %>%
  map(~ .x %>% bind_rows(.id = "issue")) %>%
  bind_rows(.id = "horizon") %>% 
  gather("sign","share",negative_significant:positive_significant) %>% 
  mutate(sign = case_when(sign == "negative_significant" ~ "Optimistic",
                          T ~ "Pessimistic")) %>% 
  split(.$issue) %>% 
  map(~ .x %>% 
        ggplot(aes(group,share, fill = sign)) +
        geom_col(position = "dodge",width = 0.5) +
        coord_flip() +
        facet_wrap(~horizon) + 
        theme_minimal() +
        xlab("") +
        ylab("Share of countries (%)") +
        theme(legend.position = "bottom") +
        labs(fill="") +
        theme(strip.text.x = element_text(size = 16),
              axis.text.x = element_text(size = 16),
              axis.text.y = element_text(size = 18),
              axis.title = element_text(size = 21),
              legend.text = element_text(size = 16))
  )


share_aggregate_group %>% 
  iwalk(~ ggsave(filename = paste0("../IEO_forecasts_material/output/figures/short-run forecasts/bias/aggregate/",.y,"_group.pdf"),.x))

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
  










