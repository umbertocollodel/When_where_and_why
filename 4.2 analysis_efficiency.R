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

# Create custom function for production table and export tables: ----
# Note: only run for Fall issue at the three different horizons (1=3 years ahead Fall, 3=4 years ahead Fall and so on...)


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
                  out = paste0("../IEO_forecasts_material/output/tables/medium_term/efficiency/",.y,"_",horizon,".tex")))
}

# Run:

c(1,3,5) %>% 
  map(~ produce_table_efficiency(.x)) 









