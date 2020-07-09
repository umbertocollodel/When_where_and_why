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


# After 2011:

centre_countries %>% 
  map(~ .x %>% filter(year > 2011)) %>%
  map(~ merge(fe,.x, by=c("year"))) %>%
  map(~ as.tibble(.x)) %>% 
  map(~ split(.x, .x$group)) %>% 
  modify_depth(2, ~ tryCatch(lm(variable2 ~ centre2, .x), error = function(e){
    cat(crayon::red("Could not run regression: check dataframe."))
  })) %>% 
  walk2(centre_countries_names, ~ stargazer(.x,
                  omit.stat = c("rsq","adj.rsq","ser","f"),
                  omit = c("Constant"),
                  dep.var.labels = c("Forecast error"),
                  covariate.labels = paste0(.y," growth forecast"),
                  column.labels = c("Africa", "Emerging Asia","Emerging Europe", "Europe","Latam","Middle East"),
                  out = paste0("../IEO_forecasts_material/output/tables/short-run forecasts/efficiency/after2011_",.y,".tex"))) 

# Before 2011:

centre_countries %>% 
  map(~ .x %>% filter(year <= 2011)) %>%
  map(~ merge(fe,.x, by=c("year"))) %>%
  map(~ as.tibble(.x)) %>% 
  map(~ split(.x, .x$group)) %>% 
  modify_depth(2, ~ tryCatch(lm(variable2 ~ centre2, .x), error = function(e){
    cat(crayon::red("Could not run regression: check dataframe."))
  })) %>% 
  walk2(centre_countries_names, ~ stargazer(.x,
                                            omit.stat = c("rsq","adj.rsq","ser","f"),
                                            omit = c("Constant"),
                                            dep.var.labels = c("Forecast error"),
                                            covariate.labels = paste0(.y," growth forecast"),
                                            column.labels = c("Africa", "Emerging Asia","Emerging Europe", "Europe","Latam","Middle East"),
                                            out = paste0("../IEO_forecasts_material/output/tables/short-run forecasts/efficiency/before2011_",.y,".tex"))) 





