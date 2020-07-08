# Efficiency and explaining forecast errors:




# Explaining forecast errors: ----

fe <- final_sr$growth %>% 
  mutate_at(vars(starts_with("variable")),.funs = funs(targety_first - .))  

centre_countries <- final_sr$growth %>%
  mutate_at(vars(starts_with("variable")),.funs = funs(targety_first - .)) %>% 
  filter(country == "United States" | country == "China") %>%
  split(.$country) %>% 
  map(~ .x %>% rename_at(vars(starts_with("variable")),.funs = funs(str_replace(.,"variable","centre")))) %>% 
  map(~ .x %>% select(year, centre1, centre2, centre3, centre4))

df <- centre_countries %>% 
  map(~ merge(.x,fe,by=c("year"))) %>% 
  map(~ as.tibble(.x)) %>% 
  map(~ .x %>% filter(year > 1990))


list_models <- df[["United States"]] %>% 
  split(.$group) %>% 
  map(~ lm(variable1 ~ centre1, .x)) %>% 
  map(~ summary(.x))


a <- df[["United States"]] %>% 
  filter(adv ==1) 

lm(variable4 ~ centre4, a) %>% 
  summary()

models_dummy <- df[["United States"]] %>% 
  split(.$adv) %>%
  map(~ .x %>% mutate(dummy = case_when(year > 2011 ~ 1,
                                        TRUE ~ 0))) %>% 
  map(~ lm(variable1 ~ centre1 + centre1*dummy, .x)) %>% 
  map(~ summary(.x))

  

# Efficiency: -----

centre_countries <- final_sr$growth %>%
  filter(country == "United States" | country == "China") %>%
  split(.$country) %>% 
  map(~ .x %>% rename_at(vars(starts_with("variable")),.funs = funs(str_replace(.,"variable","centre")))) %>% 
  map(~ .x %>% select(year, centre1, centre2, centre3, centre4))


final_sr[["growth"]] %>% 
  split(.$group) %>% 
  map(~ merge(.x,centre_countries[["United States"]], by=c("year"))) %>% 
  map(~ as.tibble(.x)) %>% 
  map(~ tryCatch(lm(variable2 ~ centre2, .x), error = function(e){
    print("problem")}
    )) %>% 
  stargazer(omit.stat = c("rsq","adj.rsq","ser","f"))
