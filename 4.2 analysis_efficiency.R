# Efficiency of medium-term forecasts ----

# Compute forecast errors:

fe <- gdp_medium %>% 
  mutate_at(vars(starts_with("variable")),.funs = funs(targety_first - .)) %>% 
  merge(geo_group, by=c("country_code"))


centre_countries_names = c("China","Germany","US")

# Obtains forecast over different horizons for centre countries:

centre_countries <- gdp_medium %>%
  filter(country == "United States" | country == "China" | country == "Germany") %>%
  split(.$country) %>% 
  map(~ .x %>% rename_at(vars(starts_with("variable")),.funs = funs(str_replace(.,"variable","centre")))) %>% 
  map(~ .x %>% select(year, centre7, centre8, centre9, centre10,centre11,centre12))

# Merge and regress by group:


list_regressions=c(variable7 ~ centre7, variable8 ~ centre8, variable9 ~ centre9,
                   variable10 ~ centre10, variable11 ~ centre11, variable12 ~ centre12)

regressions <- centre_countries %>% 
  map(~ merge(fe,.x, by=c("year"))) %>%
  map(~ as.tibble(.x)) %>% 
  map(~ split(.x, .x$group)) %>% 
  modify_depth(2, ~ map(list_regressions, function(x){
    tryCatch(lm(x, .x), error = function(e){
      cat(crayon::red("Could not run the regression. Check data\n"))
  })})) 

# Wrangle in table format: ----


table_medium_efficiency <- regressions %>% 
  modify_depth(3, ~ summary(.x)) %>% 
  modify_depth(3, ~ .x[["coefficients"]]) %>% 
  modify_depth(3, ~ .x %>% as_tibble() %>%  slice(2)) %>% 
  modify_depth(2, ~ .x %>% bind_rows(.id = "horizon")) %>% 
  map(~ .x %>% bind_rows(.id = "group")) %>% 
  map(~ .x %>% mutate(Estimate = round(Estimate, 2))) %>% 
  map(~ .x %>% mutate(Estimate = case_when(`t value` > 1.96 | `t value` < -1.96 ~ str_replace(as.character(Estimate), "$","**"),
                                           (`t value` > 1.68 & `t value` < 1.96) | (`t value` < -1.68 & `t value` > -1.96) ~ str_replace(as.character(Estimate), "$", "*"),
                                           TRUE ~ as.character(Estimate)))) %>% 
  map(~ .x %>% mutate(horizon = case_when(horizon == 1 ~ "H=3,F",
                                          horizon == 2 ~ "H=3,S",
                                          horizon == 3 ~ "H=4,F",
                                          horizon == 4 ~ "H=4,S",
                                          horizon == 5 ~ "H=5,F",
                                          T ~ "H=5,S"))) %>% 
  map(~ .x %>% mutate(group = case_when(group == "africa" ~ "Africa",
                                        group == "emerging_asia" ~ "Emerging Asia",
                                        group == "europe" ~ "Europe",
                                        group == "emerging_europe"~ "Emerging Europe",
                                        group == "latin_america" ~ "Latin America",
                                        T ~ "Middle East"))) %>% 
  map(~ .x %>% select(group, horizon, Estimate)) %>% 
  map(~ .x %>% spread(horizon, Estimate)) %>% 
  map(~ .x %>% rename(Group = group))


table_medium_efficiency %>% 
  iwalk(~ .x %>% stargazer(summary = F,
                           rownames = F,
                           out = paste0("../IEO_forecasts_material/output/tables/medium_term/efficiency/",.y,".tex")))

# Footnote:

footnote=c("The table shows results from regressions of year(t + h) errors in the WEO GDP growth forecasts made in year(t) on an intercept and the year(t) forecast of year(t + h) US GDP growth (Panel I) , China GDP growth (Panel II) or Euro area GDP growth (Panel III). 
           The four columns to the left report the estimated beta coefficient from these regressions.") %>% 
  cat(file = "../IEO_forecasts_material/output/tables/medium_term/efficiency/efficiency_footnote.tex")



# Number of countries for which ineffiency: ----



list_regressions=c(variable7 ~ centre7, variable8 ~ centre8, variable9 ~ centre9,
                   variable10 ~ centre10, variable11 ~ centre11, variable12 ~ centre12)

regressions <- centre_countries %>% 
  map(~ merge(fe,.x, by=c("year"))) %>%
  map(~ as.tibble(.x)) %>% 
  map(~ split(.x, .x$country_code)) %>% 
  modify_depth(2, ~ map(list_regressions, function(x){
    tryCatch(lm(x, .x), error = function(e){
      cat(crayon::red("Could not run the regression. Check data\n"))
    })})) %>% 
  modify_depth(2,~ discard(.x,~ class(.x) != "lm"))



table_medium_efficiency <- regressions %>% 
  modify_depth(3, ~ summary(.x)) %>% 
  modify_depth(3, ~ .x[["coefficients"]]) %>% 
  modify_depth(3, ~ .x %>% as_tibble() %>%  slice(2)) %>% 
  modify_depth(2, ~ .x %>% bind_rows(.id = "horizon")) %>% 
  map(~ .x %>% bind_rows(.id = "country_code")) %>%
  map(~ .x %>% mutate(country = countrycode(country_code,"imf","country.name"))) %>% 
  map(~ .x %>% mutate(horizon = case_when(horizon == 1 ~ "H=3,Fall",
                                          horizon == 2 ~ "H=3,Spring",
                                          horizon == 3 ~ "H=4,Fall",
                                          horizon == 4 ~ "H=4,Spring",
                                          horizon == 5 ~ "H=5,Fall",
                                          T ~ "H=5,S"))) %>% 
  map(~ .x %>% separate(horizon, c("horizon","issue"), sep = ",")) %>%
  map(~ .x %>% filter(`t value` >= 1.96) %>% select(country_code, country, horizon, issue, Estimate)) %>% 
  map(~ .x %>% merge(geo_group)) 

# TO COMPLETE!
  

