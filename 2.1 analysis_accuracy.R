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


# Footnote:

footnote = c("Root mean squared error of current year Spring WEO issue forecasts divided by the realized volatility of
  the actual value over the period. Actual value is the first settled i.e. reported in the Fall WEO of the
  following year.")

cat(footnote, file = "../IEO_forecasts_material/output/figures/short-run forecasts/accuracy/footnote.tex")


# Figure 2: evolution of RMSE over different horizons:  -----

rmse_horizon_group <- final_sr %>%
  map(~ .x %>% group_by(adv)) %>% 
  map(~ .x %>% summarise_at(vars(starts_with("variable")), ~ round(rmse(.,targety_first, na.rm = T),2))) %>%
  map(~ .x %>% mutate(adv = case_when(adv == 1 ~ "AE",
                                      TRUE ~ "EME"))) %>% 
  map(~ .x %>% rename(`Income group` = adv)) %>% 
  .$growth  

rmse_horizon_lidc <- final_sr %>%
  map(~ .x %>% filter(lidc == 1)) %>%
  map(~ .x %>% summarise_at(vars(starts_with("variable")), ~ round(rmse(.,targety_first, na.rm = T),2))) %>%
  map(~ .x %>% mutate(`Income group` = "Low-income") %>% select(`Income group`, everything())) %>%  
  .$growth  


rmse_horizon <- final_sr %>%
  map(~ .x %>% summarise_at(vars(starts_with("variable")), ~ round(rmse(.,targety_first, na.rm = T),2))) %>%
  map(~ .x %>% mutate(`Income group` = "Full") %>% select(`Income group`, everything())) %>% 
  .$growth  

list(rmse_horizon, rmse_horizon_group, rmse_horizon_lidc) %>% 
  reduce(rbind) %>%   
  stargazer(summary = F, rownames = F) %>% 
  cat(file = "../IEO_forecasts_material/output/tables/short-run forecasts/accuracy/rmse_forecast_horizon.tex")
