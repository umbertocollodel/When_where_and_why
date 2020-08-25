# Inability to predict recessions


a <- comparison_wb %>% 
  mutate(recession = case_when(targety_first <= 0 ~ 1,
                               T ~ 0)) %>% 
  mutate_at(vars(contains("wb")),funs(targety_first - .)) %>% 
  select(country_code,year,wb3, recession) 

b <- comparison_ec %>% 
  mutate_at(vars(contains("ec")),funs(targety_first - .)) %>% 
  mutate(recession = case_when(targety_first <= 0 ~ 1,
                               T ~ 0)) %>% 
  select(country_code,year,ec3, recession) 


c <- comparison_consensus %>% 
  mutate(recession = case_when(targety_first <= 0 ~ 1,
                               T ~ 0)) %>% 
  mutate_at(vars(contains("consensus")),funs(targety_first - .)) %>% 
  select(country_code,year,consensus3, recession) %>% 
  rename_at(vars(contains("consensus")),funs(str_replace(.,"consensus","cn")))

e <- d[[3]] %>%
  rename_at(vars(contains("consensus")),funs(str_replace(.,"consensus","cb"))) %>% 
  mutate(country_code = countrycode(country,"country.name","imf")) %>% 
  select(country_code,year,cb3, recession) 
  
  

list(a,b,c,e) %>% 
  reduce(merge, all=T) %>%
  as_tibble() %>% 
  gather("institution","fe",wb3:cb3) %>%
  mutate(recession = case_when(recession == 0 ~ "Non-recession",
                               T ~ "Recession")) %>% 
  mutate(institution = case_when(institution == "wb3" ~ "World Bank",
                                 institution == "ec3" ~ "European Commission",
                                 institution == "cn3" ~ "Consensus (Mean)",
                                 T ~ "Consensus (Best)")) %>% 
  ggplot(aes(x=fe,y=institution, fill = recession)) +
  geom_density_ridges(col="white",alpha = 0.4, scale=0.95) +
  theme_minimal() +
  scale_fill_manual(values = c("#0000ff","#ff0000")) +
  ylab("") +
  xlab("Real Growth Forecast Error (%)") +
  theme(legend.position = "bottom") +
  labs(fill = "",col="") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 21),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16))


ggsave("../IEO_forecasts_material/output/figures/comparison/inability_recessions.pdf")




  

