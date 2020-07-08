# Which actual value to use for benchmark?

# Biasedness ----


bias_test_aggregate <- final_sr %>% 
  map(~ .x %>% group_by(year) %>% mutate(fe2 = targety_last - variable2)) %>%
  map(~ .x %>% summarise(median = median(fe2, na.rm = T))) %>% 
  map(~ lm(median ~ 1, .x)) 

bias_test_ae <- final_sr %>% 
  map(~ .x %>% filter(adv == 1)) %>%
  map(~ .x %>% group_by(year) %>% mutate(fe2 = targety_last - variable2)) %>%
  map(~ .x %>% summarise(median = median(fe2, na.rm = T))) %>% 
  map(~ lm(median ~ 1, .x)) 

bias_test_eme <- final_sr %>% 
  map(~ .x %>% filter(adv == 0)) %>% 
  map(~ .x %>% group_by(year) %>% mutate(fe2 = targety_last - variable2)) %>%
  map(~ .x %>% summarise(median = median(fe2, na.rm = T))) %>% 
  map(~ lm(median ~ 1, .x)) 

bias_test_lidc <- final_sr %>% 
  map(~ .x %>% filter(lidc == 1)) %>% 
  map(~ .x %>% group_by(year) %>% mutate(fe2 = targety_last - variable2)) %>%
  map(~ .x %>% summarise(median = median(fe2, na.rm = T))) %>% 
  map(~ lm(median ~ 1, .x)) 


stargazer(bias_test_aggregate$growth, bias_test_ae$growth, bias_test_eme$growth, bias_test_lidc$growth,
          dep.var.caption = "Median forecast error GDP growth",
          omit.stat = c("rsq","adj.rsq","ser"),
          object.names = TRUE,
          column.labels = c("Full sample","AE","EME","Low-income"),
          out = "../IEO_forecasts_material/output/tables/short-run forecasts/which_actual/formal_test_aggregate.tex")


# Graphical representation ----


which_actual_ae <- final_sr$growth %>%
  filter(adv == 1) %>%
  mutate(fe2_first = targety_first - variable2,
         fe2_last = targety_last - variable2) %>%
  group_by(year) %>% 
  summarise(median_first = median(fe2_first,na.rm = T),
            median_last = median(fe2_last, na.rm = T)) %>% 
  ggplot(aes(year)) +
  geom_line(aes(y = median_first, group = 1, color = "First settled"),size = 1) +
  geom_line(aes(y = median_last, group = 1, color = "April 2020 WEO"), size = 1) +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1),
        legend.position = "bottom") +
  labs(color = "") +
  xlab("") +
  ylab("") +
  ylim(-5,5)

ggsave("../IEO_forecasts_material/output/figures/short-run forecasts/which_actual/which_actual_ae.pdf",which_actual_ae)





