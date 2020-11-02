############# Script to summarise previous tables on inability to predict recessions
############# by major institutions and problems to forecast dynamics during financial crises:

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

footnote=c("Distribution of real GDP growth forecast errors for main institutional and private forecasters.
           Recessions are periods of negative growth.") %>% 
  cat(file ="../IEO_forecasts_material/output/figures/comparison/inability_recessions_footnote.tex")



# New part on financial crises: ----


load("~/Dropbox/Early warning model of currency crisis/Betin_Collodel/2. Text mining IMF_data/datasets/comparison/other_data.RData")


# Clean Laeven & Valencia database:

laeven_clean <- x %>% 
  select_at(vars(matches("LV|ISO3|year"))) %>% 
  select(ISO3_Code,year,BC.LV,CC.LV,SD.LV) %>%
  mutate(ISO3_Code = as.character(ISO3_Code)) %>% 
  group_by(ISO3_Code,year) %>% 
  summarise_at(vars(contains("LV")),mean,na.rm = T) %>% 
  mutate_at(vars(contains("LV")),funs(case_when(. >0 ~ 1,
                                                T~ 0))) %>%
  mutate(country_code = countrycode(ISO3_Code,"iso3c","imf")) %>% 
  ungroup() %>% 
  mutate(triple = case_when(BC.LV == 1 & CC.LV == 1 & SD.LV == 1 ~ 1,
                            T~0),
         double_cc_sd = case_when(BC.LV == 0 & CC.LV ==1 & SD.LV ==1 ~ 1,
                                  T ~ 0),
         double_cc_bc = case_when(BC.LV == 1 & CC.LV ==1 & SD.LV ==0 ~ 1,
                                  T ~ 0),
         double_bc_sd = case_when(BC.LV == 1 & CC.LV ==0 & SD.LV ==1 ~ 1,
                                  T ~ 0)) 


# Calculate forecast errors when recessions are accompanied by multiplicity of financial crises: 

financial_crises_fe <- final_sr$growth %>% 
  merge(laeven_clean,all.x = T) %>% 
  mutate_at(vars(matches("variable")),funs(targety_first - .)) %>% 
  mutate(recession = case_when(targety_first <= 0 ~ "Recession",
                                            T ~ "Non-recession")) %>%
  mutate(triple = case_when(BC.LV == 1 & CC.LV == 1 & SD.LV == 1 ~ 1,
                          T~0),
         double_cc_sd = case_when(recession == "Recession" & BC.LV == 0 & CC.LV ==1 & SD.LV ==1 ~ 1,
                                  T ~ 0),
         double_cc_bc = case_when(recession == "Recession" & BC.LV == 1 & CC.LV ==1 & SD.LV ==0 ~ 1,
                                  T ~ 0),
         double_bc_sd = case_when(recession == "Recession" & BC.LV == 1 & CC.LV ==0 & SD.LV ==1 ~ 1,
                                  T ~ 0),
         single_bc = case_when(recession == "Recession" & BC.LV == 1 & CC.LV ==0 & SD.LV ==0 ~ 1,
                               T~0),
         single_cc = case_when(recession == "Recession" & BC.LV == 0 & CC.LV ==1 & SD.LV ==0 ~ 1,
                               T~0),
         single_sd = case_when(recession == "Recession" & BC.LV == 0 & CC.LV ==0 & SD.LV==1 ~ 1,
                               T~0),
         normal_recession = case_when(recession == "Recession" & BC.LV == 0 & CC.LV ==0 & SD.LV ==0 ~ 1,
                                  T ~ 0)) %>% 
  mutate(type = case_when(triple == 1 ~ "Triple+Recession",
    double_cc_sd == 1 | double_cc_bc == 1 | double_bc_sd ==1 ~ "Twin+Recession",
    single_bc == 1 | single_cc == 1 | single_sd ==1 ~ "Single+Recession",
    normal_recession == 1 ~ "Normal Recession",
    T ~ "No-Recession")) %>% 
  gather("Horizon","value",variable1:variable4) %>% 
  split(.$Horizon) %>% 
  map(~ .x %>% group_by(type) %>% summarise(median = median(value, na.rm = T))) %>% 
  map(~ .x %>% filter(type != "Triple+Recession")) %>% 
  map(~ .x %>% arrange(median)) %>% 
  bind_rows(.id = "Horizon") %>%
  spread(type,median) %>% 
  select(Horizon,`Normal Recession`,`Single+Recession`,`Twin+Recession`) %>% 
  mutate_at(vars(contains("Recession")),funs(round(.,2))) %>% 
  mutate(Horizon = case_when(Horizon == "variable1" ~ "H=0,F",
                             Horizon == "variable2" ~ "H=0,S",
                             Horizon == "variable3" ~ "H=1,F",
                             T~ "H=1,S")) 
  
# Export:

financial_crises_fe %>% 
stargazer(summary = F,
          rownames = F,
          out = "../IEO_forecasts_material/output/tables/short-run forecasts/bias/bias_financial_crisis_growth.tex")
  
  
# Footnote:

footnote=c("Median forecast error during normal recessions and recessions accompanied by
           single or twin financial crises. Dates for banking, currency and sovereign debt crises from Laeven & Valencia 
           database.") %>% 
  cat(file="../IEO_forecasts_material/output/tables/short-run forecasts/bias/bias_financial_crisis_growth_footnote.tex")
  
  
  
  

