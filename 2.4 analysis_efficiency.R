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

# Create custom function for production table and export tables: ----
# Note: only run for Fall issue at the three different horizons (1= current-year Fall, 3= year-ahead Fall and so on...)


produce_table_efficiency <- function(horizon=1){
  
  horizon=horizon  
  
  regressions %>% 
    modify_depth(2, ~ .x[[horizon]]) %>% 
    imap(~ stargazer(.x, 
                     summary = F,
                     rownames = F,
                     omit.stat = c("rsq","adj.rsq","ser","f"),
                     omit = c("Constant"),
                     dep.var.labels = c("Forecast error"),
                     covariate.labels = paste0(.y," growth forecast"),
                     column.labels = c("Africa", "Emerging Asia","Emerging Europe", "Europe","Latam","Middle East"),
                     out = paste0("../IEO_forecasts_material/output/tables/short-run forecasts/efficiency/",.y,"_",horizon,".tex")))
}

c(1,3) %>% 
  map(~ produce_table_efficiency(.x)) 





