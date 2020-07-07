# Figure 1: Root mean squared error by income group divided by realized volatility -----

# Whole period:

accuracy_wp_figure <- final_sr %>% 
  map(~ .x %>% group_by(adv) %>% summarise(rmse = hydroGOF::rmse(targety_first,variable2, na.rm = T),
                                           sd = sd(targety_first, na.rm = T))) %>% 
  map(~ .x %>% mutate(rmse_scaled = rmse/sd)) %>%  
  map(~ .x %>% ungroup()) %>% 
  map(~ .x %>% mutate(adv = case_when(adv == 1 ~ "Advanced", 
                                      TRUE ~ "Emerging"))) %>% 
  map(~ .x %>% 
                ggplot(aes(adv, rmse_scaled, fill = adv)) +
                geom_col() +
                geom_text(aes(x=adv,y=rmse_scaled,label=round(rmse_scaled,2)),color = "black",alpha=0.5,vjust=-1)+
                theme_minimal() +
                labs(fill = "Group")+
                xlab("") +
                ylab("RMSE / Realized volatility") +
                theme(legend.position = "bottom"))

# Export:

accuracy_wp_figure %>% 
  walk2(names(accuracy_wp_figure), ~ ggsave(paste0("../IEO_forecasts_material/output/figures/short-run forecasts/accuracy/all/",.y,".pdf"),.x))

 
# Different time chunks: 

accuracy_bp_figure <- final_sr %>% 
  map(~ .x %>% group_by(adv,period) %>% summarise(rmse = hydroGOF::rmse(targety_first,variable2, na.rm = T),
                                                  sd = sd(targety_first, na.rm = T))) %>% 
  map(~ .x %>% mutate(rmse_scaled = rmse/sd)) %>%
  map(~ .x %>% ungroup()) %>% 
  map(~ .x %>% mutate(adv = case_when(adv == 1 ~ "Advanced", 
                                      TRUE ~ "Emerging"))) %>% 
  map(~ split(.x, .x$period)) %>% 
  modify_depth(2, ~ .x %>% 
                 ggplot(aes(adv, rmse_scaled, fill = adv)) +
                 geom_col() +
                 geom_text(aes(x=adv,y=rmse_scaled,label=round(rmse_scaled,2)),color = "black",alpha=0.5,vjust=-1)+
                 theme_minimal() +
                 labs(fill = "Group")+
                 xlab("") +
                 ylab("RMSE / Realized volatility") +
                 theme(legend.position = "bottom"))

# Export:

accuracy_bp_figure[["growth"]] %>% 
  walk2(names(accuracy_bp_figure[["growth"]]), ~ ggsave(paste0("../IEO_forecasts_material/output/figures/short-run forecasts/accuracy/growth_",.y,".pdf"),.x))

accuracy_bp_figure[["inflation"]] %>% 
  walk2(names(accuracy_bp_figure[["inflation"]]), ~ ggsave(paste0("../IEO_forecasts_material/output/figures/short-run forecasts/accuracy/inflation_",.y,".pdf"),.x))

accuracy_bp_figure[["cagdp"]] %>% 
  walk2(names(accuracy_bp_figure[["cagdp"]]), ~ ggsave(paste0("../IEO_forecasts_material/output/figures/short-run forecasts/accuracy/cagdp_",.y,".pdf"),.x))



#######




# Calculation RMSE 

a <- final %>% 
  map(~ .x %>% select(year, targety_first, variable1) %>%  group_by(year) %>% summarise(rmse_first = hydroGOF::rmse(targety_first, variable1, na.rm = T))) 

b <- final %>% 
  map(~ .x %>% select(year, targety_last, variable1) %>%  group_by(year) %>% summarise(rmse_last = hydroGOF::rmse(targety_last, variable1, na.rm = T))) 

a[[1]] %>%
  merge(b[[1]]) %>%
  gather("actual","rmse",rmse_first:ncol(.)) %>% 
  ggplot(aes(year,rmse,col = actual)) +
  geom_point() 


final %>% 
  map(~ .x %>% group_by(year) %>% summarise(rmse_first = hydroGOF::rmse(targety_first, variable1, na.rm = T))) %>% 
  map(~ .x %>% ggplot(aes(year, rmse_first, group = 1)) +
        geom_line() +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1)) +
        xlab("") +
        ylab(""))

rmse_adv <- final[["growth"]] %>% 
  merge(country_group, by = c("country_code")) %>%
  filter(adv ==1) %>% 
  group_by(year) %>% 
  summarise(rmse_2_adv = hydroGOF::rmse(targety_first, variable2, na.rm = T))


rmse_eme <- final[["growth"]] %>% 
  merge(country_group, by = c("country_code")) %>%
  filter(eme ==1) %>% 
  group_by(year) %>% 
  summarise(rmse_2_eme = hydroGOF::rmse(targety_first, variable2, na.rm = T))


rmse_lidc <- final[["growth"]] %>% 
  merge(country_group, by = c("country_code")) %>%
  filter(lidc ==1) %>% 
  group_by(year) %>% 
  summarise(rmse_2_lidc = hydroGOF::rmse(targety_first, variable2, na.rm = T))


rmse_figure <- list(rmse_adv, rmse_eme, rmse_lidc) %>% 
  reduce(merge, by=c("year")) %>% 
  ggplot(aes(year)) +
  geom_line(aes(y=rmse_2_adv, color = "Advanced", group = 1)) +
  geom_line(aes(y=rmse_2_eme, color = "Emerging", group = 1)) +
  geom_line(aes(y=rmse_2_lidc, color = "Low-income", group = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1)) +
  xlab("") +
  ylab("RMSE") +
  labs(colour = "Group")


ggsave("../IEO_forecasts_material/output/figures/rmse/comparison_rmse.pdf", rmse_figure)


# RMSE for different horizons: (Table 1 Timmerman) -----

rmse_horizon_group <- final %>%
  map(~ .x %>% merge(country_group,by =c("country_code"))) %>%
  map(~ .x %>% group_by(adv)) %>% 
  map(~ .x %>% summarise_at(vars(starts_with("variable")), ~ round(rmse(.,targety_first, na.rm = T),2))) %>%
  map(~ .x %>% mutate(adv = case_when(adv == 1 ~ "AE",
                                      TRUE ~ "EME"))) %>% 
  map(~ .x %>% rename(`Income group` = adv)) %>% 
  .$growth  

rmse_horizon_lidc <- final %>%
  map(~ .x %>% merge(country_group,by =c("country_code")) %>% filter(lidc == 1)) %>%
  map(~ .x %>% summarise_at(vars(starts_with("variable")), ~ round(rmse(.,targety_first, na.rm = T),2))) %>%
  map(~ .x %>% mutate(`Income group` = "Low-income") %>% select(`Income group`, everything())) %>%  
  .$growth  


rmse_horizon <- final %>%
  map(~ .x %>% merge(country_group,by =c("country_code"))) %>%
  map(~ .x %>% summarise_at(vars(starts_with("variable")), ~ round(rmse(.,targety_first, na.rm = T),2))) %>%
  map(~ .x %>% mutate(`Income group` = "Full") %>% select(`Income group`, everything())) %>% 
  .$growth  

list(rmse_horizon, rmse_horizon_group, rmse_horizon_lidc) %>% 
  reduce(rbind) %>%   
  stargazer(summary = F, rownames = F)