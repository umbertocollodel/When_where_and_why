

## IBRD: -----

# Read tables document:

tables <- pdf_text("../IEO_forecasts_material/raw_data/world bank/World_Bank_Financial_Statements_2014.pdf") %>% 
  str_subset("SUMMARY STATEMENT OF LOANS") %>% 
  str_split("\n") %>% 
  map(~ .x %>% as_tibble()) %>% 
  map(~ .x %>% slice(-1:-7))

# Work on them separetely:

first_part <- tables[[1]] %>% 
  slice(-1, -(60:61)) %>% 
  mutate(country = str_sub(value, 1, 35),
         aid_ibrd = str_sub(value, 45, 52)) %>% 
  select(-value) 

second_part <- tables[[2]] %>% 
  slice(-1, -(23:38)) %>% 
  mutate(country = str_sub(value, 1, 35),
         aid_ibrd = str_sub(value, 46, 58)) %>% 
  select(-value)


ibrd_stat  <- rbind(first_part, second_part) %>% 
  mutate(country = str_remove(country,"^\\s*")) 



## IDA: ----

tables <- pdf_text("../IEO_forecasts_material/raw_data/world bank/World_Bank_Financial_Statements_2014.pdf") %>% 
  str_subset("OF\\s*DEVELOPMENT CREDITS") %>% 
  str_split("\n") %>% 
  map(~ .x %>% as_tibble()) %>% 
  map(~ .x %>% slice(-1:-7))


# Work on them separetely:

first_part <- tables[[1]] %>%
  slice(-(57:58)) %>% 
  mutate(country = str_sub(value, 1, 35),
         aid_ida = str_sub(value, 42, 50)) %>% 
  select(-value) 

second_part <- tables[[2]] %>% 
  slice(-(46:48)) %>% 
  mutate(country = str_sub(value, 1, 35),
         aid_ida = str_sub(value, 45, 56)) %>% 
  select(-value)

ida_stat <- rbind(first_part, second_part)


# Finish cleaning: ----

wb_aid <- list(ibrd_stat, ida_stat) %>%
  map(~ .x %>% mutate_at(vars(contains("aid")),funs(str_replace(.,",","\\.")))) %>% 
  map(~ .x %>% mutate_at(vars(contains("aid")), funs(as.numeric(.)))) %>% 
  map(~ .x %>% mutate(country_code = countrycode(country, "country.name","imf"))) %>% 
  map(~ .x %>% select(country_code, contains("aid"))) %>%
  reduce(merge,by = c("country_code"), all = T) %>% 
  mutate(country = countrycode(country_code,"imf","country.name")) %>% 
  select(country_code, country, contains("aid")) %>% 
  mutate(total_aid = aid_ibrd + aid_ida)

# Export:

wb_aid %>% 
saveRDS(file = "../IEO_forecasts_material/intermediate_data/world bank/wb_aid_cleaned.RDS")


# Analysis: ----

wb_aid %>% 
  gather("aid_instit","amount",aid_ibrd:total_aid) %>%
  mutate(aid_instit = case_when(aid_instit == "aid_ibrd" ~ "IBRD",
                                aid_instit == "aid_ida" ~ "IDA",
                                T ~ "Total")) %>% 
  split(.$aid_instit) %>% 
  map(~ .x %>% arrange(-amount)) %>% 
  map(~ .x %>% slice(1:10)) %>% 
  bind_rows() %>% 
  mutate(country = as.factor(country)) %>% 
  mutate(country = reorder(factor(country),amount)) %>% 
  ggplot(aes(country, amount)) +
  geom_col(width = 0.6) +
  facet_wrap(~ aid_instit, nrow = 3, scales = "free_y") +
  coord_flip() +
  theme_minimal() +
  xlab("") +
  ylab("")

# Maybe a table is better... ----

wb_aid %>% 
  gather("aid_instit","amount",aid_ibrd:total_aid) %>%
  mutate(aid_instit = case_when(aid_instit == "aid_ibrd" ~ "IBRD",
                                aid_instit == "aid_ida" ~ "IDA",
                                T ~ "Total")) %>% 
  split(.$aid_instit) %>% 
  map(~ .x %>% arrange(-amount)) %>% 
  map(~ .x %>% slice(1:10))
