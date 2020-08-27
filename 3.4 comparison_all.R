############# Script to summarise previous tables on inability to predict recessions
############# by major institutions:

# Inability to predict recessions:

inability_df <- list(final_sr$growth,
                     comparison_wb,
                     comparison_ec,
                     comparison_consensus %>% filter(forecaster == "Consensus (Mean)")) %>% 
  map(~ .x %>% mutate_at(vars(matches("variable\\d|wb\\d|ec\\d|consensus\\d")),funs(targety_first - .))) %>% 
  map(~ .x %>% mutate(recession = case_when(targety_first <= 0 ~ "Recession",
                                            T ~ "Non-recession"))) 


inability_df[[1]] <- inability_df[[1]] %>% 
  rename_at(vars(matches("variable")),funs(str_replace(.,"variable","if"))) 



inability_df %>% 
  map(~ .x %>% select(country_code,year,matches("if\\d|wb\\d|ec\\d|consensus\\d"),recession)) %>% 
  map(~ .x %>% rename_at(vars(matches("consensus")),funs(str_replace(.,"consensus","cn")))) %>% 
  reduce(merge, all=T) %>% 
  gather("institution","fe",if1:ncol(.)) %>%
  separate(institution, c("institution","horizon"), sep = 2) %>% 
  mutate(institution = case_when(institution == "wb" ~ "World Bank",
                                 institution == "ec" ~ "European Commission",
                                 institution == "if" ~ "IMF",
                                 institution == "cn" ~ "Consensus (Mean)")) %>% 
  mutate(horizon = case_when(horizon == 1 ~ "H=0,F",
                                 horizon == 2 ~ "H=0,S",
                                 horizon == 3 ~ "H=1,F",
                                 T ~ "H=1,S")) %>% 
  ggplot(aes(x=fe,y=institution, fill = recession)) +
  geom_density_ridges(col="white",alpha = 0.4, scale=0.95) +
  theme_minimal() +
  facet_wrap(~ horizon) +
  scale_fill_manual(values = c("#0000ff","#ff0000")) +
  ylab("") +
  xlab("Real Growth Forecast Error (%)") +
  theme(legend.position = "bottom") +
  xlim(-20,10) +
  labs(fill = "",col="") +
  theme(legend.position = "bottom") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 21),
        strip.text.x = element_text(size=14),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16))


  


ggsave("../IEO_forecasts_material/output/figures/comparison/inability_recessions.pdf")




  

