# Efficiency and explaining forecast errors

# Compute forecast errors:

fe <- final_sr$growth %>% 
  mutate_at(vars(starts_with("variable")),.funs = funs(targety_first - .))  

# Efficiency growth forecasts: -----

centre_countries_names = c("China","Germany","US")

# Obtains forecast over different horizons for centre countries:

centre_countries <- final_sr$growth %>%
  filter(country == "United States" | country == "China" | country == "Germany") %>%
  split(.$country) %>% 
  map(~ .x %>% rename_at(vars(starts_with("variable")),.funs = funs(str_replace(.,"variable","centre")))) %>% 
  map(~ .x %>% select(year, centre1, centre2, centre3, centre4))

# Merge and regress by group:


list_regressions=c(variable1 ~ centre1, variable2 ~ centre2,
                   variable3 ~ centre3, variable4 ~ centre4)

regressions <- centre_countries %>% 
  map(~ merge(fe,.x, by=c("year"))) %>%
  map(~ as.tibble(.x)) %>% 
  map(~ split(.x, .x$group)) %>% 
  modify_depth(2, ~ map(list_regressions, function(x){
    tryCatch(lm(x, .x), error = function(e){
      cat(crayon::red("Could not run the regression. Check data\n"))
    })})) 

# Wrangle in table format: ----


table_sr_efficiency <- regressions %>% 
  modify_depth(3, ~ summary(.x)) %>% 
  modify_depth(3, ~ .x[["coefficients"]]) %>% 
  modify_depth(3, ~ .x %>% as_tibble() %>%  slice(2)) %>% 
  modify_depth(2, ~ .x %>% bind_rows(.id = "horizon")) %>% 
  map(~ .x %>% bind_rows(.id = "group")) %>% 
  map(~ .x %>% mutate(Estimate = round(Estimate, 2))) %>% 
  map(~ .x %>% mutate(Estimate = case_when(`t value` > 1.96 | `t value` < -1.96 ~ str_replace(as.character(Estimate), "$","**"),
                              (`t value` > 1.68 & `t value` < 1.96) | (`t value` < -1.68 & `t value` > -1.96) ~ str_replace(as.character(Estimate), "$", "*"),
                              TRUE ~ as.character(Estimate)))) %>% 
  map(~ .x %>% mutate(horizon = case_when(horizon == 1 ~ "H=0,F",
                                          horizon == 2 ~ "H=0,S",
                                          horizon == 3 ~ "H=1,F",
                                          T ~ "H=1,S"))) %>% 
  map(~ .x %>% mutate(group = case_when(group == "africa" ~ "Africa",
                                  group == "emerging_asia" ~ "Emerging Asia",
                                  group == "europe" ~ "Europe",
                                  group == "emerging_europe"~ "Emerging Europe",
                                  group == "latin_america" ~ "Latin America",
                                  T ~ "Middle East"))) %>% 
  map(~ .x %>% select(group, horizon, Estimate)) %>% 
  map(~ .x %>% spread(horizon, Estimate)) %>% 
  map(~ .x %>% rename(Group = group))


table_sr_efficiency %>% 
  iwalk(~ .x %>% stargazer(summary = F,
                           rownames = F,
                           out = paste0("../IEO_forecasts_material/output/tables/short-run forecasts/efficiency/",.y,".tex")))

# Footnote:

footnote=c("The table shows results from regressions of year(t + h) errors in the WEO GDP growth forecasts made in year(t) on an intercept and the year(t) forecast of year(t + h) China GDP growth (Table 4) , Germany GDP growth (Table 5) or US GDP growth (Table 6). 
           The four columns report the estimated beta coefficient from these regressions.") %>% 
  cat(file = "../IEO_forecasts_material/output/tables/short-run forecasts/efficiency/efficiency_footnote.tex")






