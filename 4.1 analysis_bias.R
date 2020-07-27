list_regressions=c(variable7 ~ 1, variable8 ~ 1, variable9 ~ 1,
                   variable10 ~ 1, variable11 ~ 1, variable12 ~ 1)


gdp_medium %>% 
  mutate_at(vars(matches("variable")), funs(targety_first - .)) %>% 
  split(.$country) %>% 
  map( ~ map(list_regressions, function(x){
    tryCatch(lm(x, .x), error = function(e){
      cat(crayon::red("Could not run the regression. Check data\n"))
    })
  }))



df_bias <- a %>% 
modify_depth(2, ~ tryCatch(summary(.x), error = function(e){
  cat(crayon::red("Could not run regression. Check dataframe \n"))
})) %>% 
  modify_depth(2, ~ tryCatch(.x[["coefficients"]], error = function(e){
    cat(crayon::red("Could not run regression. Check dataframe \n"))
  })) %>% 
  map(~ discard(.x, ~ length(.x) != 4)) %>% 
  modify_depth(2, ~ as.data.frame(.x)) %>% 
  map(~ bind_rows(.x, .id = "horizon")) %>% 
  bind_rows(.id = "country") %>% 
  mutate(issue = case_when(horizon == 1 | horizon == 3 | horizon == 5 ~ "Fall",
                           T ~ "Spring"),
         horizon = case_when(horizon == 1 | horizon == 2 ~ "H=3",
                             horizon == 3 | horizon == 4 ~ "H=4",
                             horizon == 5 | horizon == 6 ~ "H=5")) %>% 
  mutate(Estimate = case_when(`t value` > 1.96 | `t value` < -1.96 ~ str_replace(as.character(Estimate), "$","**"),
                                           (`t value` > 1.68 & `t value` < 1.96) | (`t value` < -1.68 & `t value` > -1.96) ~ str_replace(as.character(Estimate), "$", "*"),
                                           TRUE ~ as.character(Estimate))) %>% 
  select(country, horizon, issue, Estimate)


df_bias %>% 
  split(.$horizon) %>% 
  map(~ .x %>% spread(issue, Estimate)) %>%
  map(~ .x %>% select(-horizon)) %>% 
  imap(~ .x %>% stargazer(summary = F, 
                          rownames = F,
                          out = paste0("../IEO_forecasts_material/output/tables/medium_term/bias/",.y,".tex")))
  
