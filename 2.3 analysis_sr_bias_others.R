set.seed(253)


bootstrap <- comparison_wb %>% 
  mutate_at(vars(contains("wb")),funs(targety_first - .)) %>% 
  select(country_code, country, year, group, contains("wb")) %>% 
  split(.$country) %>% 
  map(~ .x %>% gather("horizon","value", wb1:wb4)) %>% 
  map(~ split(.x,.x$horizon)) %>%
  modify_depth(2, ~ modelr::bootstrap(.x,n=1000)) %>% 
  modify_depth(2,~ .x %>% pull(strap)) %>% 
  modify_depth(3, ~ .x %>% as.data.frame() %>% summarise(estimate = mean(value, na.rm = T))) %>% 
  modify_depth(2, ~ bind_rows(.x, .id = "replication_n")) %>% 
  modify_depth(2, ~ .x %>% summarise(sd = sd(estimate, na.rm = T))) %>% 
  map(~ bind_rows(.x, .id = "horizon")) %>% 
  bind_rows(.id = "country")
  
  


normal <-  comparison_wb %>% 
  mutate_at(vars(contains("wb")),funs(targety_first - .)) %>% 
  select(country_code, country, year, group, contains("wb")) %>% 
  split(.$country) %>% 
  map(~ .x %>% gather("horizon","value", wb1:wb4)) %>% 
  map(~ split(.x,.x$horizon)) %>% 
  modify_depth(2, ~ lm(value ~ 1, .x)) %>% 
  modify_depth(2, ~ summary(.x)) %>% 
  modify_depth(2, ~ .x[["coefficients"]][1]) %>% 
  modify_depth(2,~ data.frame(estimate = .x)) %>% 
  map(~ bind_rows(.x, .id = "horizon")) %>% 
  bind_rows(.id = "country")


  

merge(normal,bootstrap) %>% 
  as_tibble() %>% 
  mutate(tstat = estimate/sd) %>%
  mutate(horizon = as.numeric(str_extract(horizon,"\\d"))) %>% 
  mutate(issue = case_when(horizon %% 2 == 1 ~ "Summer",
                           T ~ "Winter"),
         horizon = case_when(horizon == 1 | horizon == 2 ~ "H=0",
                             horizon == 3 | horizon == 4 ~ "H=1")) %>% 
  split(.$horizon) %>% 
  map(~ .x %>% mutate(negative_significant = case_when(tstat <= -1.96 ~ 1,
                                                       T~ 0))) %>% 
  map(~ .x %>% mutate(positive_significant = case_when(tstat >= 1.96 ~ 1,
                                                       T~ 0))) %>%
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
        scale_fill_grey() +
        theme(legend.position = "bottom") +
        theme(strip.text.x = element_text(size = 16),
              axis.text.x = element_blank(),
              axis.text.y = element_text(size = 18),
              axis.title = element_text(size = 21),
              legend.text = element_text(size = 16)) +
        theme(panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank()) +
        labs(fill = ""))



footnote=c("The figure shows the share of countries for each forecast horizon and issue of the Global Economic
           Prospects (Summer or Winter) with a 5\% statistically signicant negative or positive bias. Test of statistical
           significance is run individually with country-by-country regressions where the standard errors are obtained
           from a 1,000 replications bootstrap.")

