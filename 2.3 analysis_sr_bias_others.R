################### Script to analyze bias in publications other than WEO
# Note: might take a while to run because of heavy use of bootstrap. To lower time, decrease the number
# of replications


# Set common parameters: ----

replication=1000
seed=253



# Run for World Bank -----


name_issues=c("Summer","Winter")


analyse_sr_bias_others(comparison_wb, seed, "wb", replications, c("Summer","Winter"))


footnote=c("The figure shows the share of countries for each forecast horizon and issue of the Global Economic
           Prospects (Summer or Winter) with a 5% statistically signicant negative or positive bias. Test of statistical
           significance is run individually with country-by-country regressions where the standard errors are obtained
           from a 1,000 replications bootstrap.") %>% 
  cat(file = "../IEO_forecasts_material/output/figures/short-run forecasts/bias_others/wb_foonote.tex")


# Run for European Commission ----



name_issues=c("Fall","Spring")


analyse_sr_bias_others(comparison_ec, seed, "ec", replications, c("Fall","Spring"))


footnote=c("The figure shows the share of countries for each forecast horizon and issue of the Global Economic
           Prospects (Summer or Winter) with a 5\% statistically signicant negative or positive bias. Test of statistical
           significance is run individually with country-by-country regressions where the standard errors are obtained
           from a 1,000 replications bootstrap.")

