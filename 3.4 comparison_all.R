############# Script to summarise previous tables on inability to predict recessions
############# by major institutions and problems to forecast dynamics during financial crises:

# Inability to predict recessions: -----

# Set parameters

names = c("recessions","trough_recessions","recovery_expansions")

# Provide clean dataframe


inability_df <- list(final_sr$growth,
                     comparison_wb,
                     comparison_ec,
                     comparison_consensus %>% filter(forecaster == "Consensus (Mean)")) %>% 
  map(~ .x %>% mutate_at(vars(matches("variable\\d|wb\\d|ec\\d|consensus\\d")),funs(targety_first - .))) %>% 
  map(~ .x %>% group_by(country) %>% mutate(recession = case_when(targety_first <= 0 ~ "Recession",
                                            T ~ "Non-recession"))) %>%
  map(~ list(normal = .x,
             trough_recession = .x %>% mutate(recession = case_when(dplyr::lag(recession,1) == "Non-recession" & recession == "Recession" ~ "Through",
                                                        T ~ recession)) %>% filter(recession == "Recession" | recession == "Through"),
             recovery_expansion = .x %>% mutate(recession = case_when(dplyr::lag(recession,1) == "Recession" & recession == "Non-recession" ~ "Recovery",
                                                          T ~ recession)) %>% filter(recession == "Recovery" | recession == "Non-recession"))) %>% 
  modify_depth(2, ~ .x %>% ungroup())
  


for (i in 1:length(inability_df[[1]])) {
  inability_df[[1]][[i]] <- inability_df[[1]][[i]] %>% 
    rename_at(vars(matches("variable")),funs(str_replace(.,"variable","if"))) 
}


clean_inability_df  <- inability_df %>% 
  modify_depth(2, ~ .x %>% select(country_code,year,matches("if\\d|wb\\d|ec\\d|consensus\\d"),recession)) %>% 
  modify_depth(2, ~ .x %>% rename_at(vars(matches("consensus")),funs(str_replace(.,"consensus","cn"))))  
  
  
# Run custom function and export

 c(1,2,3) %>%   
    map(~ new %>% plot_distribution_comparison(.x)) %>% 
    walk2(names, ~ ggsave(paste0("../IEO_forecasts_material/output/figures/comparison/inability_",.y,".pdf"), .x))
          
# Footnote:

footnote=c("Distribution of real GDP growth forecast errors for main institutional and private forecasters.
            The sample for each forecaster reflects data availability (refer to section 1). 
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
  as_tibble() %>% 
  mutate_at(vars(matches("variable")),funs(targety_first - .)) %>% 
  mutate(recession = case_when(targety_first <= 0 ~ "Recession",
                                            T ~ "Non-recession")) %>%
  mutate(triple = case_when(recession == "Recession" & BC.LV == 1 & CC.LV == 1 & SD.LV == 1 ~ 1,
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
  filter(!type == "Triple+Recession") %>% 
  gather("horizon","value",variable1:variable4) %>% 
  mutate(horizon = case_when(horizon == "variable1" ~ "H=0,F",
                             horizon == "variable2" ~ "H=0,S",
                             horizon == "variable3" ~ "H=1,F",
                             T ~ "H=1,S")) %>% 
  ggplot(aes(x=value, type)) +
  geom_density_ridges(scale=0.8, alpha = 0.6) +
  facet_wrap(~ horizon) +
  theme_minimal() +
  ylab("") +
  xlim(-25,10) +
  xlab("Real Growth Forecast Error (%)") +
  theme(legend.position = "bottom") +
  labs(fill="") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 21),
        strip.text.x = element_text(size=14),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16))

# Export:

ggsave("../IEO_forecasts_material/output/figures/comparison/inability_recessions_financial.pdf")
  
  
# Footnote:

footnote=c("Distribution of WEO forecast errors during episodes of recessions and recessions accompanied by financial crises") %>% 
  cat(file="../IEO_forecasts_material/output/figures/comparison/inability_recessions_financial_footnote.tex")
  
  

