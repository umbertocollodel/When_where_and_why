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


breaks <- seq(2000,2018)


breaks %>%
  map(~ df[["United States"]] %>% filter(year <= .x) %>% filter(adv == 1)) %>%
  map(~ lm(variable1 ~ centre1, .x)) %>%
  map(~ cbind(.x[["coefficients"]][2],confint(.x))) %>%
  map(~ as.tibble(.x)) %>% 
  map(~ .x %>% setNames(c("coefs","lower","upper")) %>% slice(2)) %>% 
  bind_rows() %>% 
  cbind(breaks) %>% 
  ggplot(aes(breaks, coefs)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.4) +
  theme_minimal() +
  ylim(0,0.4)



breaks %>%
  map(~ df[["China"]] %>% filter(year <= .x) %>% filter(lidc == 1)) %>%
  map(~ lm(variable1 ~ centre1, .x)) %>% 
  map(~ summary(.x)) 


  
  
  
# Optimistic!