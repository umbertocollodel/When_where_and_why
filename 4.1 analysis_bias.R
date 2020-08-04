list_regressions=c(variable7 ~ 1, variable8 ~ 1, variable9 ~ 1,
                   variable10 ~ 1, variable11 ~ 1, variable12 ~ 1)


regressions <- gdp_medium %>% 
  mutate_at(vars(matches("variable")), funs(targety_first - .)) %>% 
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
  mutate(issue = case_when(horizon == 1 | horizon == 3 | horizon == 5 ~ "Fall",
                           T ~ "Spring"),
         horizon = case_when(horizon == 1 | horizon == 2 ~ "H=3",
                             horizon == 3 | horizon == 4 ~ "H=4",
                             horizon == 5 | horizon == 6 ~ "H=5")) %>% 
  mutate(Estimate = round(Estimate,2)) %>% 
  mutate(Estimate = case_when(`t value` > 1.96 | `t value` < -1.96 ~ str_replace(as.character(Estimate), "$","**"),
                                           (`t value` > 1.68 & `t value` < 1.96) | (`t value` < -1.68 & `t value` > -1.96) ~ str_replace(as.character(Estimate), "$", "*"),
                                           TRUE ~ as.character(Estimate))) %>%
  mutate(country = countrycode(country_code,"imf","country.name")) %>% 
  select(country_code, country, horizon, issue, Estimate)


df_bias %>% 
  split(.$horizon) %>% 
  map(~ .x %>% spread(issue, Estimate)) %>%
  map(~ .x %>% select(-country_code,-horizon)) %>% 
  imap(~ .x %>% stargazer(summary = F, 
                          rownames = F,
                          out = paste0("../IEO_forecasts_material/output/tables/medium_term/bias/",.y,".tex")))
  

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
  iwalk(~ ggsave(filename = paste0("../IEO_forecasts_material/output/figures/medium_term/bias/aggregate/",.y,".pdf"),.x))

# Footnote:

footnote=c("The figure shows the share of countries for each forecast horizon and issue of the World Economic
           Outlook (Fall or Spring) with a statistically signicant negative and positive bias. Test of statistical
           significance is run individually with country-by-country regressions.") %>% 
  cat(file = "../IEO_forecasts_material/output/figures/medium_term/bias/aggregate/aggregate_footnote.tex")
  




# Magnitude of bias: ----

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
            out = "../IEO_forecasts_material/output/tables/medium_term/bias/magnitude_aggregate_bias.tex")

# Geographical subvidision biases: ----


share_aggregate_group <- df_bias %>% 
  merge(group) %>%
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
  iwalk(~ ggsave(filename = paste0("../IEO_forecasts_material/output/figures/medium_term/bias/aggregate/",.y,"_group.pdf"),.x))


# Where are advanced economies?

# Magnitude by group: ----


df_bias %>% 
  merge(group) %>%
  split(.$horizon) %>% 
  map(~ .x %>% filter(str_detect(Estimate,"\\*"))) %>% 
  map(~ .x %>% filter(str_detect(Estimate,"-"))) %>% 
  map(~ .x %>% group_by(group)) %>% 
  map(~ .x %>% mutate(Estimate = as.numeric(str_remove_all(Estimate,"\\*")))) %>% 
  map(~ .x %>% summarise(mean = round(mean(Estimate, na.rm = T),2),
                         median = round(median(Estimate, na.rm = T),2),
                         max = round(min(Estimate, na.rm = T),2))) %>% 
  bind_rows(.id = "horizon") %>%
  setNames(c("Horizon","Geo. group","Mean","Median","Max.")) %>% 
  stargazer(summary = F,
            rownames = F,
            out = paste0("../IEO_forecasts_material/output/tables/medium_term/bias/magnitude_aggregate_bias_group.tex"))
  








