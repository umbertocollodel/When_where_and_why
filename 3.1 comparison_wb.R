################ Script to compare IMF WEO forecasts with World Bank GEP:

# Prepare comparison dataframe -----
# Note: improve this reading part, switch intermediate files
# to RDS in order to avoid repetition


# World Bank data (complete) from Ayuhan:

gep_data <- readRDS("../IEO_forecasts_material/intermediate_data/world bank/gdp_wb_cleaned.rds") %>% 
  filter(!country %in% country_to_exclude)
  

# IMF WEO updates data:

weo_updates <- readRDS("../IEO_forecasts_material/intermediate_data/rgdp_update_cleaned.RDS")

# Actual values: (fall issue of next year WEO)

load("../IEO_forecasts_material/intermediate_data/rgdp_cleaned.RData")

target <- x %>% 
  select(country_code, country, year, targety_first)

# Geographical group:

load("../IEO_forecasts_material/intermediate_data/country_group_geography_clean.RData")

geo_group <- x


# Bind all together:

comparison_wb <- list(weo_updates, gep_data, target) %>% 
  reduce(merge, by=c("country_code","country","year")) %>% 
  merge(geo_group, by="country_code") %>% 
  select(country_code, country, year, group, targety_first, everything()) %>%
  arrange(country, year) %>% 
  as_tibble()

comparison_wb %>% 
  saveRDS("../IEO_forecasts_material/intermediate_data/world bank/comparison_wb.RDS")

# Dataframe with geographical group countries ----


group <- comparison_wb %>%
  group_by(country_code) %>% 
  slice(1) %>% 
  select(country_code, group) %>% 
  mutate(group = case_when(group == "europe" ~ "Advanced Economies",
                           is.na(group) ~ "Advanced Economies",
                           group == "emerging_asia" ~ "Emerging Asia",
                           group == "latin_america" ~ "Latin America",
                           group == "emerging_europe" ~ "Emerging Europe",
                           group == "middle_east" ~ "Middle East",
                           T ~ "Africa"))

# Table with list countries comparison:----

comparison_wb %>% 
  group_by(country) %>% 
  slice(1) %>% 
  select(country, group) %>% 
  arrange(group) %>%
  mutate(group = case_when(group == "emerging_asia" ~ "Emerging Asia",
                           group == "emerging_europe" ~ "Emerging Europe",
                           group == "latin_america" ~ "Latin America",
                           group == "middle_east" ~ "Middle East",
                           group == "africa"~ "Africa")) %>%
  rename(Country = country, `Geo. group` = group) %>% 
  stargazer(summary = F,
            rownames = F,
            out = "../IEO_forecasts_material/output/tables/comparison/WB_updated/country_sample.tex"
            )


# Figure 1: comparison of median forecast error: ----

evolution_median_fe <- comparison_wb %>%  
  mutate_at(vars(contains("wb")),funs(targety_first - .)) %>% 
  mutate_at(vars(contains("variable")),funs(targety_first - .)) %>% 
  group_by(year) %>% 
  mutate_at(vars(matches("variable|wb")), median, na.rm = T)


plot_evolution <- function(variable1, variable2){
  
  variable1_quosure <- enquo(variable1)
  variable2_quosure <- enquo(variable2)
  
  evolution_median_fe %>% 
  filter(complete.cases(variable2)) %>% 
  ggplot(aes(year)) +
  geom_line(aes(y = !!variable1_quosure, group = 1, col = "WB"), size = 1) +
  geom_line(aes(y = !!variable2_quosure, group = 1, col = "IMF" ), size = 1) +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  xlab("") +
  ylab("") +
  labs(col = "Institution") +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1),
        legend.position = "bottom") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 21),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16)) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  ylim(-2,2)
  
}

plot_evolution(variable1, wb1) %>% 
save.plot("../IEO_forecasts_material/output/figures/comparison/WB_updated/evolution_bias/current_year_jul_comparison.pdf")

plot_evolution(variable2, wb2) %>% 
  save.plot("../IEO_forecasts_material/output/figures/comparison/WB_updated/evolution_bias/current_year_jan_comparison.pdf")

plot_evolution(variable3, wb3) %>% 
  save.plot("../IEO_forecasts_material/output/figures/comparison/WB_updated/evolution_bias/year_ahead_jul_comparison.pdf")

plot_evolution(variable4, wb4) %>% 
  save.plot("../IEO_forecasts_material/output/figures/comparison/WB_updated/evolution_bias/year_ahead_jan_comparison.pdf")



# By country group (not in the paper): ----

# comparison_wb %>% 
#   mutate_at(vars(contains("wb")),funs(targety_first - .)) %>% 
#   mutate_at(vars(contains("variable")),funs(targety_first - .)) %>% 
#   split(.$group) %>% 
#   map(~ .x %>% group_by(year)) %>% 
#   map(~ .x %>% mutate(median_1wb = median(wb1, na.rm = T), median_2wb = median(wb2, na.rm = T))) %>% 
#   map(~ .x %>% mutate(median_1imf = median(variable1, na.rm = T), median_2imf = median(variable2, na.rm = T))) %>% 
#   map(~ .x %>% ggplot(aes(year)) +
#         geom_line(aes(y = median_2wb, group = 1, col = "WB"), size = 1) +
#         geom_line(aes(y = median_2imf, group = 1, col = "IMF" ), size = 1) +
#         theme_minimal() +
#         xlab("") +
#         ylab("") +
#         labs(col = "Institution") +
#         theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1),
#               legend.position = "bottom") +
#         theme(axis.text = element_text(size = 18),
#               axis.title = element_text(size = 21),
#               legend.title = element_text(size = 18),
#               legend.text = element_text(size = 16)
#         ) +
#         ylim(-4,4))


# Figure 2: number of countries with lower RMSE for each institution ----
  
raw <- comparison_wb %>% 
  group_by(country) %>%
  summarise_at(vars(matches("variable|wb")),funs(hydroGOF::rmse(.,targety_first))) %>% 
  ungroup() %>% 
  mutate(ratio1 = (variable1/wb1)- 1,
         ratio2 = (variable2/wb2) - 1,
         ratio3 = (variable3/wb3) - 1,
         ratio4 = (variable4/wb4) - 1) %>%
  mutate_at(vars(contains("ratio")), funs(better_imf = case_when(. < 0 ~ 1,
                                                    T ~ 0))) 


  raw %>% 
    gather("better_imf","value",ratio1_better_imf:ratio4_better_imf) %>% 
    group_by(better_imf, value) %>% 
    count() %>% 
    ungroup() %>% 
    mutate(value = case_when(value == 0 ~ "WB",
                                  T ~ "IMF")) %>%
    mutate(better_imf = case_when(better_imf == "ratio1_better_imf" ~ "H=0, Jul.",
                                  better_imf == "ratio2_better_imf" ~ "H=0, Jan.",
                                  better_imf == "ratio3_better_imf" ~ "H=1, Jul.",
                                  T ~ "H=1, Jan.")) %>% 
    ggplot(aes(better_imf, n)) +
    geom_col(aes(fill=value),position = "dodge", alpha = 0.8, width = 0.3, col = "white") +
    geom_text(aes(y = n,label=n, fill = value), position=position_dodge(width=0.3), size = 7, vjust = -0.25) +
    theme_minimal() +
    ylab("Number of countries") +
    xlab("") +
    labs(fill = "Lower RMSE for:") +
    theme(legend.position = "bottom") +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank()) +
    theme(axis.text = element_text(size = 18),
          axis.title = element_text(size = 21),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 16)) 
    
ggsave("../IEO_forecasts_material/output/figures/comparison/WB_updated/accuracy/comparison_individual_countries.pdf")
    
# Table appendix: comparison RMSE for all individual countries ----- 
    
    
    rmse_comparison <- comparison_wb %>% 
      group_by(country_code) %>%
      summarise_at(vars(variable1:wb4), funs(hydroGOF::rmse(.,targety_first))) %>% 
      mutate(country = countrycode(country_code,"imf","country.name")) %>% 
      mutate(ratio1 = (variable1/wb1)- 1,
             ratio2 = (variable2/wb2) - 1,
             ratio3 = (variable3/wb3) - 1,
             ratio4 = (variable4/wb4) - 1) %>% 
      select(country_code,country,contains("ratio"))
    
    # Export:
    
    rmse_comparison %>%
      select(country, contains("ratio")) %>% 
      mutate_at(vars(ratio1:ratio4), funs(round(.,digits = 2))) %>% 
      setNames(c("Country","H=0,Jul.", "H=0,Jan.","H=1,Jul.","H=1,Jan.")) %>% 
      stargazer(summary= F,
                rownames = F,
                out = "../IEO_forecasts_material/output/tables/comparison/WB_updated/appendix/rmse_comparison_full.tex")
    
# Figure 3: share of countries with lower RMSE for IMF by region ----
    
    group %>% 
      merge(rmse_comparison, by=c("country_code")) %>% 
      mutate_at(vars(ratio1:ratio4), funs(case_when(. < 0 ~ 1,
                                                    T ~ 0))) %>% 
      ungroup() %>% 
      group_by(group) %>% 
      summarise_at(vars(ratio1:ratio4), mean, na.rm = T) %>% 
      gather("horizon","share",ratio1:ratio4) %>% 
      mutate(horizon = case_when(horizon == "ratio1"~ "H=0,Jul.",
                                 horizon == "ratio2"~ "H=0,Jan.",
                                 horizon == "ratio3"~ "H=1,Jul.",
                                 T~ "H=1,Jan.")) %>% 
      ggplot(aes(horizon, share)) +
      geom_col(width = 0.3,col = "lightgrey",alpha = 0.6) +
      facet_wrap(~ group) +
      theme_minimal() +
      ylab("Share of countries (%)") +
      xlab("Horizon") +
      theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1),
            legend.position = "bottom") +
      theme(axis.text = element_text(size = 18),
            axis.title = element_text(size = 21),
            legend.title = element_text(size = 18),
            legend.text = element_text(size = 16)) +
      theme(strip.text.x = element_text(size = 14, colour = "darkblue")) +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.y = element_blank())
    
    
ggsave("../IEO_forecasts_material/output/figures/comparison/WB_updated/accuracy/comparison_rmse_group.pdf")

# Footnote:

footnote=c("The figure shows the share of countries for which WEO forecasts produce a lower RMSE
           compared to GEP forecasts.") %>% 
  cat(file = "../IEO_forecasts_material/output/figures/comparison/WB_updated/accuracy/comparison_rmse_group_footnote.tex")
    

# Figure 4: magnitude of RMSE difference by geographical group: -----

group %>% 
  merge(rmse_comparison, by=c("country_code")) %>% 
  group_by(group) %>% 
  summarise_at(vars(contains("ratio")), median, na.rm = T) %>% 
  gather("horizon","value",ratio1:ratio4) %>% 
  mutate(value = value*100) %>%
  mutate(horizon = case_when(horizon == "ratio1"~ "H=0,Jul.",
                             horizon == "ratio2"~ "H=0,Jan.",
                             horizon == "ratio3"~ "H=1,Jul.",
                             T~ "H=1,Jan.")) %>% 
  ggplot(aes(horizon, value)) +
  geom_col(width = 0.3, alpha = 0.6) +
  facet_wrap(~ group) +
  theme_minimal() +
  ylab("% of GEP RMSE") +
  xlab("Horizon") +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1),
        legend.position = "bottom") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 21),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16)) +
  theme(strip.text.x = element_text(size = 14, colour = "darkblue")) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

ggsave("../IEO_forecasts_material/output/figures/comparison/WB_updated/accuracy/comparison_rmse_group_magnitude.pdf")

# Footnote:

footnote=c("The figure shows the median (country-by-country) difference between WEO root mean squared error and GEP root mean squared error as a percentage
           of the latter.") %>% 
  cat(file ="../IEO_forecasts_material/output/figures/comparison/WB_updated/accuracy/comparison_rmse_group_magnitude_footnote.tex")


# Table 1: forecast errors of both institutions during recessions and non-recessions ---- 

main_table <- comparison_wb %>% 
  mutate_at(vars(contains("wb")),funs(targety_first - .)) %>% 
  mutate_at(vars(contains("variable")),funs(targety_first - .)) %>% 
  mutate(recession = case_when(targety_first < 0 ~ 1,
                               T ~ 0)) %>% 
  group_by(recession) %>%
  summarise_at(vars(matches("variable|wb")),funs(round(median(.,na.rm = T),2))) %>% 
  select(recession, wb1, variable1, wb2, variable2, wb3, variable3, wb4, variable4) %>% 
  setNames(c("Recession",
             "Current-year, Jul. (WB)","Current-year, Jul. (IMF)",
             "Current-year, Jan. (WB)","Current-year, Jan. (IMF)",
             "Year-ahead, Jul. (WB)","Year-ahead, Jul. (IMF)",
             "Year-ahead, Jan. (WB)","Year-ahead, Jan. (IMF)")) %>%
  mutate(Recession = case_when(Recession == 1 ~ "Recession",
                               T ~ "Non-recession"))


issues=c("Jul","Jan")

issues %>% 
  map(~ main_table %>% select(Recession, contains(.x))) %>% 
  map2(issues, ~ .x %>% stargazer(rownames = F,
                         summary = F,
                         out = paste0("../IEO_forecasts_material/output/tables/comparison/WB_updated/recession/recession_forecast_error_",.y,".tex")))



# Table 2: forecast errors of both institutions during recessions and non-recessions (by geo. group) -----

main_table <- comparison_wb %>% 
  mutate_at(vars(contains("wb")),funs(targety_first - .)) %>% 
  mutate_at(vars(contains("variable")),funs(targety_first - .)) %>%
  as_tibble() %>% 
  split(.$group) %>% 
  map(~ .x %>% mutate(recession = case_when(targety_first < 0 ~ 1,
                               T ~ 0))) %>% 
  map(~ .x %>% group_by(recession)) %>% 
  map(~ .x %>% summarise_at(vars(matches("variable|wb")),funs(round(median(.,na.rm = T),2)))) %>% 
  map(~ .x %>% select(recession, wb1, variable1, wb2, variable2, wb3, variable3, wb4, variable4)) %>% 
  map(~ .x %>% setNames(c("Recession",
                            "Current-year, Jul. (WB)","Current-year, Jul. (IMF)",
                            "Current-year, Jan. (WB)","Current-year, Jan. (IMF)",
                            "Year-ahead, Jul. (WB)","Year-ahead, Jul. (IMF)",
                            "Year-ahead, Jan. (WB)","Year-ahead, Jan. (IMF)"))) %>%
  map(~ .x %>% filter(Recession == 1)) %>% 
  bind_rows(.id = "Geo. group") %>% 
  select(-Recession) %>% 
  mutate(`Geo. group` = case_when(`Geo. group` == "emerging_asia" ~ "Emerging Asia",
                           `Geo. group` == "latin_america" ~ "Latin America",
                           `Geo. group` == "emerging_europe" ~ "Emerging Europe",
                           `Geo. group` == "middle_east" ~ "Middle East",
                           T ~ "Africa")) 

issues %>% 
  map(~ main_table %>% select(`Geo. group`,contains(.x))) %>% 
  map2(issues,~ .x %>% stargazer(rownames = F,
                       summary = F,
                       out = paste0("../IEO_forecasts_material/output/tables/comparison/WB_updated/recession/recession_forecast_error_group_",.y,".tex")))


# Figure 5: distribution forecast errors during recessions by group ----

distribution_group <- comparison_wb %>% 
  mutate_at(vars(contains("wb")),funs(targety_first - .)) %>% 
  mutate_at(vars(contains("variable")),funs(targety_first - .)) %>%
  as_tibble() %>% 
  mutate(recession = case_when(targety_first < 0 ~ 1,
                                            T ~ 0)) %>% 
  filter(recession == 1) %>% 
  filter(country != "Papua New Guinea") 


distribution_group_plot <- c(1,2,3,4) %>% 
  map(~ distribution_group %>% select(country_code, country, year, group, contains(as.character(.x)))) %>% 
  map(~ .x %>% gather("institution","value",5:6)) %>% 
  map(~ .x %>% mutate(institution = case_when(str_detect(institution, "variable") ~ "IMF",
                                 T ~ "WB"))) %>% 
  map(~ .x %>% mutate(group = case_when(group == "emerging_asia" ~ "Em. Asia",
                                  group == "latin_america" ~ "Lat. America",
                                  group == "emerging_europe" ~ "Em. Europe",
                                  group == "middle_east" ~ "Middle East",
                                  T ~ "Africa"))) %>% 
  map(~ .x %>% ggplot(aes(group, value, col = group)) +
  geom_boxplot(width = 0.4, outlier.size = 0) +
  facet_wrap(~ institution) +
  coord_flip() +
  theme_minimal() +
  ylab("") +
  xlab("") +
  ylim(-15,5) +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1)) +
  theme(strip.text = element_text(size = 16),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 21),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16)) +
  theme(legend.position = "none"))

horizon=c("current","year_ahead") %>% 
  map(~ rep(.x, 2)) %>% 
  unlist()

list(distribution_group_plot,horizon, rep(issues,2)) %>% 
  pwalk(function(x,y,z){
    ggsave(x,filename = paste0("../IEO_forecasts_material/output/figures/comparison/WB_updated/distribution_fe_recession/distribution_fe_recession_",y,"_",z,".pdf"))
    })


footnote=c("IEO calculations. Emerging Asia is excluded from the graph because it reports only one recession in the period 2010-2018.") %>% 
  cat(file = "../IEO_forecasts_material/output/figures/comparison/WB_updated/distribution_fe_recession_footnote.tex")
  
  
# Evolution forecast errors during periods of consecutive recessions: -----
# Note: evolution of current-year forecasts

recession <- comparison_wb %>% 
  mutate(recession = case_when(targety_first < 0 ~ 1,
                               T ~ 0)) %>% 
  filter(recession == 1) %>% 
  select(country, year, targety_first, variable1, wb1) %>%
  gather("var","value",targety_first:wb1) %>% 
  mutate(var = case_when(var == "targety_first" ~ "Actual",
         var == "variable1" ~ "WEO Forecast",
         T ~ "GEP Forecast")) %>% 
  mutate(year = as.numeric(year)) %>% 
  group_by(country, var, grp = cumsum(c(1, diff(year) != 1))) %>%  # have to understand this line!
  filter(n() > 1) %>% 
  ungroup() %>% 
  mutate(year = as.character(year))
    
  
 
    
  recession %>% 
  ggplot(aes(year, value, col = var, group = var)) +
    geom_line(aes(group = paste(country,var)),size = 2) +
    geom_point(size = 3) +
    theme_minimal() +
    xlab("") +
    ylab("") +
    labs(col = "") +
    facet_wrap(~country, strip.position = "bottom", scales = "free_x", nrow = 1) +
    theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1, size = 14)) +
    theme(axis.text.y = element_text(size = 18),
        axis.title = element_text(size = 21),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        strip.text.x = element_text(size = 18))+
    ylim(-17,17) +
    theme(legend.position = "bottom") +
    theme(panel.spacing = unit(0, "lines"), 
            strip.background = element_blank(),
            strip.placement = "outside")

ggsave("../IEO_forecasts_material/output/figures/comparison/WB_updated/comparison_recessions.pdf")

footnote = c("IEO calculations.") %>% 
  cat(file = "../IEO_forecasts_material/output/figures/comparison/WB/comparison_recessions_footnote.tex")

  
# Figures requested by Prakash (single countries) ----

countries=c("Argentina","Bangladesh","China","Colombia","Nigeria","Ethiopia","Peru","Vietnam")


# Current-year January:

individual_current_jan <- countries %>% 
  map(~ comparison_wb %>% filter(country == .x)) %>% 
  map(~ .x %>% select(country, year, targety_first, variable1, wb1)) %>% 
  map(~ .x %>% mutate_at(vars(variable1:wb1), funs(targety_first -.))) %>%
  map(~ .x %>% rename(IMF = variable1, `World Bank`= wb1)) %>% 
  map(~ .x %>% gather("var","value", IMF:`World Bank`)) %>% 
  map(~ .x %>% 
        ggplot(aes(year, value, group = var, col = var)) +
        geom_line(size = 1.5) +
        geom_hline(yintercept = 0, size = 1) +
        geom_text(aes(y=9.5,x=year,label=round(targety_first,2)),color="black",size=7,angle = 270,alpha=0.9)+
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1, size = 22)) +
        theme(axis.text.y = element_text(size = 22),
              axis.title = element_text(size = 22),
              legend.text = element_text(size = 18)) +
        theme(legend.position = "none") +
        xlab("")  +
        ylab("") +
        labs(col = "") +
        ylim(-10,10)
        )

individual_current_jan %>% 
  walk2(countries, ~ ggsave(paste0("../IEO_forecasts_material/output/figures/comparison/WB_updated/individual/current_jan_",.y,".pdf"),.x))


# Table with summary:

comparison_wb %>% 
  filter(country %in% countries) %>% 
  group_by(country) %>% 
  summarise_at(vars(variable1:wb2), funs(hydroGOF::rmse(.,targety_first))) %>%
  mutate_at(vars(variable1:wb2), funs(round(.,2))) %>%
  setNames(c("Country","Current-year (IMF)","Current-year (WB)", "Year-ahead (IMF)","Year-ahead (WB)")) %>% 
  stargazer(summary = F,
            rownames = F,
            out = "../IEO_forecasts_material/output/figures/comparison/WB_updated/individual/summary_table_accuracy.tex")



comparison_wb %>% 
  filter(country %in% countries) %>% 
  group_by(country) %>% 
  summarise_at(vars(variable1:wb2), funs(median(targety_first - ., na.rm = T))) %>%
  mutate_at(vars(variable1:wb2), funs(round(.,2))) %>%
  setNames(c("Country","Current-year (IMF)","Current-year (WB)", "Year-ahead (IMF)","Year-ahead (WB)")) %>% 
  stargazer(summary = F,
            rownames = F,
            out = "../IEO_forecasts_material/output/figures/comparison/WB_updated/individual/summary_table_bias.tex")





# Year-ahead January:

individual_year_jan <- countries %>% 
  map(~ comparison_wb %>% filter(country == .x)) %>%
  map(~ .x %>% filter(year >= 2011)) %>% 
  map(~ .x %>% select(country, year, targety_first, variable2, wb2)) %>% 
  map(~ .x %>% mutate_at(vars(variable2:wb2), funs(targety_first -.))) %>%
  map(~ .x %>% rename(IMF = variable2, `World Bank`= wb2)) %>% 
  map(~ .x %>% gather("var","value", IMF:`World Bank`)) %>% 
  map(~ .x %>% 
        ggplot(aes(year, value, group = var, col = var)) +
        geom_line(size = 1.5) +
        geom_hline(yintercept = 0, size = 1) +
        geom_text(aes(y=9.5,x=year,label=round(targety_first,2)),color="black",size=7,angle = 270,alpha=0.9)+
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1, size = 22)) +
        theme(axis.text.y = element_text(size = 22),
              axis.title = element_text(size = 22),
              legend.text = element_text(size = 18)) +
        theme(legend.position = "none") +
        xlab("")  +
        ylab("") +
        labs(col = "") +
        ylim(-10,10)
  )

individual_year_jan %>% 
  walk2(countries, ~ ggsave(paste0("../IEO_forecasts_material/output/figures/comparison/WB_updated/individual/year_ahead_jan_",.y,".pdf"),.x))


# Footnote:

footnote=c("IEO calculations. Orange and blue line indicate respectively the forecast error of the World
Economic Outlook (IMF) and Global Economic Prospects (World Bank) January current-year forecasts. 
At the top of each year, the actual GDP growth is reported in bold")


# Forecast errors bias and aid ----

wb_aid <- readRDS("../IEO_forecasts_material/intermediate_data/world bank/wb_aid_cleaned.RDS")

aid_comparison <- merge(comparison_wb, wb_aid) %>% 
  mutate_at(vars(contains("variable")), funs(targety_first - .)) %>% 
  mutate_at(vars(contains("wb")), funs(targety_first - .)) %>%
  arrange(country, year) %>% 
  as_tibble()


# Table 3: bias and WB engagement ----

aid_comparison %>% 
  mutate_at(vars(contains("aid")), funs(case_when(. > median(.,na.rm = T) ~ 1,
                                                  T ~ 0))) %>%
  gather("type_aid","upper",aid_ibrd:total_aid) %>% 
  split(.$type_aid) %>% 
  map(~ .x %>% group_by(upper)) %>% 
  map(~ .x %>% summarise(median1 = round(median(wb1, na.rm = T),2),
                         median2 = round(median(wb2, na.rm = T),2),
                         median3 = round(median(wb3, na.rm = T),2),
                         median4 = round(median(wb4, na.rm = T),2))) %>% 
  bind_rows(.id = "Source") %>% 
  mutate(upper = case_when(upper == 1 ~ "Extensive",
                           upper == 0 ~ "Normal")) %>% 
  mutate(Source = case_when(Source == "aid_ibrd" ~ "IBRD",
                            Source == "aid_ida" ~ "IDA",
                            T ~ "Total")) %>% 
  setNames(c("Source","Type of Engament","H=0, Jul.","H=0, Jan.", "H=1, Jul.","H=1, Jan.")) %>% 
  stargazer(summary = F,
            rownames = F,
            out = "../IEO_forecasts_material/output/tables/comparison/WB_updated/aid_bias/aid_errors.tex")


footnote=c("Extensive engament is defined as total loans outstanding above the median of the distribution
           as of June 2014. H=0, J is the current-year January median forecast error for the group. H=1, J is the year-ahead January 
           median forecast error.") %>% 
  cat(file ="../IEO_forecasts_material/output/tables/comparison/WB_updated/aid_errors_footnote.tex")


# Tables 8 & 9: bias and WB engagement - robutstness with different cut-offs for extensive engagement ----

names=c("75th_percentile","95th_percentile")

c(0.75,0.95) %>% 
  map(~ aid_comparison %>% mutate_at(vars(contains("aid")), funs(case_when(. > quantile(.,.x,na.rm = T) ~ 1,
                                                  T ~ 0)))) %>%
  map(~ .x %>% gather("type_aid","upper",aid_ibrd:total_aid)) %>% 
  map(~ .x %>% split(.$type_aid)) %>% 
  modify_depth(2, ~ .x %>% group_by(upper)) %>% 
  modify_depth(2, ~ .x %>% summarise(median1 = round(median(wb1, na.rm = T),2),
                                     median2 = round(median(wb2, na.rm = T),2),
                                     median3 = round(median(wb3, na.rm = T),2),
                                     median4 = round(median(wb4, na.rm = T),2))) %>% 
  map(~ .x %>% bind_rows(.id = "Source")) %>% 
  map(~ .x %>% mutate(upper = case_when(upper == 1 ~ "Extensive",
                           upper == 0 ~ "Normal"))) %>% 
  map(~ .x %>% mutate(Source = case_when(Source == "aid_ibrd" ~ "IBRD",
                            Source == "aid_ida" ~ "IDA",
                            T ~ "Total"))) %>% 
  map(~ .x %>% setNames(c("Source","Type of Engament","H=0, Jul.","H=0, Jan.","H=1, Jul.","H=1, Jan."))) %>% 
  map2(names, ~ .x %>% stargazer(summary = F,
                         rownames = F,
                         out = paste0("../IEO_forecasts_material/output/tables/comparison/WB_updated/aid_bias/aid_errors_",.y,".tex")))


# Figure 7 & 8 - scatterplot bias and WB aid: ----

# Calculate median forecast error throughout period:

list_scatter <- aid_comparison %>% 
  group_by(country) %>% 
  mutate_at(vars(matches("variable|wb")), median, na.rm = T) %>% 
  slice(1) %>%
  gather("type_aid","value",aid_ibrd:total_aid) %>% 
  split(.$type_aid) %>%
  map(~ .x %>% ungroup()) %>% 
  map(~ .x %>% mutate(type_engagement = case_when(value > median(value, na.rm = T) ~ "Extensive",
                                                  T ~ "Normal"))) 

# Custom function to plot different horizons:

plot_rel_bias_aid <- function(variable){
  
  variable_quosure <- enquo(variable)

list_scatter %>% 
  map(~ .x %>% 
  ggplot(aes(value, !!variable_quosure, col = type_engagement)) +
  geom_point(size=5, alpha = 0.6) +
  geom_smooth(method='lm', formula= y~x, se = F, col = "red") +
  theme_minimal() +
  ylab("Real Growth Forecast Error (%)") +
  xlab("Engagement") +
  ylim(-3,2) +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1)) +
  theme(  axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20),
          axis.title = element_text(size = 22)) +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18)) +
  labs(col = "Type: ")
  )
}

# Run the function and export:

plot_rel_bias_aid(wb1) %>%
  iwalk(~ ggsave(filename = paste0("../IEO_forecasts_material/output/figures/comparison/WB_updated/aid_bias/",.y,"_currentJul.pdf"),.x))

  
plot_rel_bias_aid(wb2) %>% 
  iwalk(~ ggsave(filename = paste0("../IEO_forecasts_material/output/figures/comparison/WB_updated/aid_bias/",.y,"_currentJan.pdf"),.x))


plot_rel_bias_aid(wb3) %>% 
  iwalk(~ ggsave(filename = paste0("../IEO_forecasts_material/output/figures/comparison/WB_updated/aid_bias/",.y,"_aheadJul.pdf"),.x))


plot_rel_bias_aid(wb4) %>% 
  iwalk(~ ggsave(filename = paste0("../IEO_forecasts_material/output/figures/comparison/WB_updated/aid_bias/",.y,"_aheadJan.pdf"),.x))


# Footnote:

footnote=c("Forecast error is the country-by-country median forecast error in the period 2010-2018. Engagement is total loans outstanding 
           in milions USD as of June 2014. Extensive engament is defined as total loans outstanding above the median of the distribution as of June 2014.") %>% 
  cat(file = "../IEO_forecasts_material/output/figures/comparison/WB_updated/aid_error_footnote.tex")

