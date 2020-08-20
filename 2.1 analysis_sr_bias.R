################### Script to analyze bias in short-term WEO forecasts
# Note: every figure and table vectorized through the function
# analyse_sr_bias with the exception of table 3.

# Set parameters: ----


regressions=c("variable1 ~ 1", "variable2 ~ 1",
              "variable3 ~ 1", "variable4 ~ 1")

name_vars=names(final_sr)


# Appendix B table: H=0 & H=1 ----

export_paths=name_vars %>% 
  map_chr(~ paste0("../IEO_forecasts_material/output/tables/short-run forecasts/bias/by_country/",.x,".tex"))

final_sr %>% 
  map2(export_paths, ~ analyse_sr_bias(.x,regressions, "appendix_table",.y))


# Figure 1: share of countries with short-term biases ----


export_paths=name_vars %>% 
  map_chr(~ paste0("../IEO_forecasts_material/output/figures/short-run forecasts/bias/aggregate/",.x,"_"))

final_sr %>% 
  map2(export_paths, ~ analyse_sr_bias(.x,regressions, "share_plot",.y))


footnote=c("The figure shows the share of countries for each forecast horizon and issue of the World Economic
           Outlook (Fall or Spring) with a 5% statistically significant negative or positive bias. Test of statistical
           significance is run individually with country-by-country regressions.") %>% 
  cat(file = "../IEO_forecasts_material/output/figures/short-run forecasts/bias/aggregate/aggregate_footnote.tex")


# Table 1: magnitude of short-term biases -----

export_paths=name_vars %>% 
  map_chr(~ paste0("../IEO_forecasts_material/output/tables/short-run forecasts/bias/magnitude_aggregate_bias_",.x,".tex"))


final_sr %>% 
  map2(export_paths, ~ analyse_sr_bias(.x,regressions,"magnitude_table",.y))

# Footnote:

footnote=c("Summary statistics of country-by-country intercepts significant at 5% level.
           Fall and Spring issues of WEO pooled together by horizon.") %>% 
  cat(file = "../IEO_forecasts_material/output/tables/short-run forecasts/bias/magnitude_aggregate_bias_footnote.tex")


# Figure 2: share of countries with short-term biases - region -----

export_paths=name_vars %>% 
  map_chr(~ paste0("../IEO_forecasts_material/output/figures/short-run forecasts/bias/aggregate/",.x,"_"))

final_sr %>% 
  map2(export_paths, ~ analyse_sr_bias(.x,regressions,"share_plot_geo",.y))


# Table 2: magnitude of short-term biases region ----

export_paths=name_vars %>% 
  map_chr(~ paste0("../IEO_forecasts_material/output/tables/short-run forecasts/bias/magnitude_aggregate_bias_",.x,"_group.tex"))

final_sr %>% 
  map2(export_paths, ~ analyse_sr_bias(.x,regressions,"magnitude_table_geo",.y))



# Table 3: forecast errors during recessions and non-recessions ----

full_sample_recession <- final_sr %>%
  map(~ .x %>% mutate_at(vars(starts_with("variable")),.funs = funs(targety_first - .))) %>%
  map(~ .x %>% merge(years_recession)) %>% 
  map(~ .x %>% group_by(recession)) %>% 
  map(~ .x %>% summarise_at(vars(starts_with("variable")),median, na.rm =T)) %>% 
  map(~ .x %>% mutate_at(vars(starts_with("variable")),round, 2)) %>% 
  map(~ .x %>% mutate(group = "Full sample")) %>% 
  map(~ .x %>% mutate(recession = case_when(recession == 0 ~ "Non-recession",
                               recession == 1 ~ "Recession"))) %>% 
  map(~ .x %>% select(group, everything()))


by_group_recession <- final_sr %>%
  map(~ .x %>% merge(geo_group)) %>%
  map(~ .x %>% merge(years_recession)) %>% 
  map(~ .x %>% mutate_at(vars(starts_with("variable")),.funs = funs(targety_first - .))) %>%
  map(~ .x %>% mutate(recession = case_when(targety_first <= 0 ~ 1,
                               TRUE ~ 0))) %>% 
  map(~ .x %>% group_by(group, recession)) %>% 
  map(~ .x %>% summarise_at(vars(starts_with("variable")),median, na.rm =T)) %>% 
  map(~ .x %>% mutate_at(vars(starts_with("variable")),round, 2)) %>% 
  map(~ .x %>% ungroup()) %>% 
  map(~ .x %>% mutate(group = case_when(group == "africa" ~ "Africa",
                           group == "emerging_asia" ~ "Emerging Asia",
                           group == "europe" ~ "Europe",
                           group == "emerging_europe"~ "Emerging Europe",
                           group == "latin_america" ~ "Latin America",
                           T ~ "Middle East"))) %>% 
  map(~ .x %>% mutate(recession = case_when(recession == 0 ~ "Non-recession",
                               recession == 1 ~ "Recession")))

full_sample_recession %>% 
  map2(by_group_recession, ~ rbind(.x,.y)) %>% 
  map(~ .x %>% setNames(c("Geo. group","Recession","H=0,F","H=0,S","H=1,F","H=1,S"))) %>% 
  imap(~ .x %>% stargazer(summary = F,
            rownames = F,
            out = paste0("../IEO_forecasts_material/output/tables/short-run forecasts/bias/bias_recession_",.y,".tex")))
  
  

# Footnote:

footnote=c("Median forecast error by horizon, issue and geographical group. Recessions are
           defined as periods of negative growth.") %>% 
  cat(file = "../IEO_forecasts_material/output/tables/short-run forecasts/bias/bias_recession_footnote.tex")





# EXTRA!!!: -----
# Figure 2 - Evolution of forecast errors: (replication of Figure 7 of the previous report)


figures_fe <- final_sr %>% 
  map(~ .x %>% mutate(fe2 = targety_first - variable2) %>% select(country_code,country, year, fe2)) %>%
  map(~ .x %>% group_by(year) %>% mutate(mean_fe2 = mean(fe2, na.rm = T), median_fe2 = median(fe2, na.rm = T))) %>% 
  imap(~ .x %>%  mutate(meta_information = .y)) %>% 
  map(~ if(unique(.x$meta_information) == "inflation"){
    .x %>% 
      ggplot(aes(year)) +
      geom_point(aes(y = fe2), alpha = 0.1) +
      geom_line(aes(y = median_fe2, group = 1, color = "Median"), size = 1) +
      geom_hline(yintercept = 0) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1),
            legend.position = "bottom") +
      labs(color = "") +
      xlab("") +
      ylab("") +
      ylim(-5,5)
  }
  else {
    .x %>% 
      ggplot(aes(year)) +
      geom_point(aes(y = fe2), alpha = 0.1) +
      geom_line(aes(y = mean_fe2, group = 1, color = "Mean"),size = 1) +
      geom_line(aes(y = median_fe2, group = 1, color = "Median"), size = 1) +
      geom_hline(yintercept = 0) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1),
            legend.position = "bottom") +
      labs(color = "") +
      xlab("") +
      ylab("") +
      ylim(-5,5)
  }
  )


figures_fe %>% 
  walk2(names(figures_fe),~ ggsave(paste0("../IEO_forecasts_material/output/figures/short-run forecasts/bias/evolution/all/",.y,".pdf"),.x))


