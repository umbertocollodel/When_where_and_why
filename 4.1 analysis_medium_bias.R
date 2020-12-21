################### Script to analyze bias in medium-term WEO forecasts
# Note: every figure and table vectorized through the function
# analyse_medium_bias.

# Set parameters: ----


regressions=c("variable7 ~ 1", "variable8 ~ 1",
              "variable9 ~ 1", "variable10 ~ 1",
              "variable11 ~ 1","variable12 ~ 1")

name_vars=names(final_medium)

# Appendix E table: H=3 & H=4 & H=5 ----

export_paths=name_vars %>% 
  map_chr(~ paste0("../When_where_and_why_material/output/tables/medium_term/bias/",.x,".tex"))

final_medium %>% 
  map2(export_paths, ~ analyse_medium_bias(.x,regressions, "appendix_table",.y))


# Figure 11: share of countries with medium-term biases ----


export_paths=name_vars %>% 
  map_chr(~ paste0("../When_where_and_why_material/output/figures/medium_term/bias/aggregate/",.x,"_"))

final_medium %>% 
  map2(export_paths, ~ analyse_medium_bias(.x,regressions, "share_plot",.y))


footnote=c("The figure shows the share of countries for each forecast horizon and issue of the World Economic
           Outlook (Fall or Spring) with a 5% statistically significant negative or positive bias. Test of statistical
           significance is run individually with country-by-country regressions.") %>% 
  cat(file = "../When_where_and_why_material/output/figures/medium_term/bias/aggregate/aggregate_footnote.tex")


# Table 14: magnitude of medium-term biases -----

export_paths=name_vars %>% 
  map_chr(~ paste0("../When_where_and_why_material/output/tables/medium_term/bias/magnitude_aggregate_bias_",.x,".tex"))


final_medium %>% 
  map2(export_paths, ~ analyse_medium_bias(.x,regressions,"magnitude_table",.y))

# Footnote:

footnote=c("Summary statistics of country-by-country intercepts significant at 5% level.
           Fall and Spring issues of WEO pooled together by horizon.") %>% 
  cat(file = "../When_where_and_why_material/output/tables/medium_term/bias/magnitude_aggregate_bias_footnote.tex")


# Figure 12: share of countries with medium-term biases - region -----

export_paths=name_vars %>% 
  map_chr(~ paste0("../When_where_and_why_material/output/figures/medium_term/bias/aggregate/",.x,"_"))

final_medium %>% 
  map2(export_paths, ~ analyse_medium_bias(.x,regressions,"share_plot_geo",.y))


# Table 15: magnitude of medium-term biases region ----

export_paths=name_vars %>% 
  map_chr(~ paste0("../When_where_and_why_material/output/tables/medium_term/bias/magnitude_aggregate_bias_",.x,"_group.tex"))

final_medium %>% 
  map2(export_paths, ~ analyse_medium_bias(.x,regressions,"magnitude_table_geo",.y))










