######### Script to simulate Debt-to-GDP dynamics with and without biased forecasts
# Note: Biased forecasts calibrated with average bias in WEO


# Set parameters: ----


r=0.025
g=c(0.02,0.03)
initial_debt=0.70
names_debt_path=c("actual","biased")


# Custom function for debt dynamics: ---- 


get_debt_dynamics <- function(r,g,initial_debt){
  ((1+r)/(1+g))*initial_debt
}  



# Simulate over different values of g: ---- 

debt_path <- g %>% 
  map(function(x){
    
    for (i in 1:20) {
      debt_gdp = get_debt_dynamics(r,x,initial_debt[i])
      initial_debt=c(initial_debt,debt_gdp)
    }
    return(initial_debt)
  })



# Plot: -----


names(debt_path) <- names_debt_path


debt_path %>%
  map(~ .x*100) %>% 
  bind_cols() %>% 
  mutate(period = 1:nrow(.)) %>% 
  gather("type","debt_gdp", actual:biased) %>% 
  mutate(type = str_to_title(type)) %>% 
  ggplot(aes(period, debt_gdp, col = type)) +
  geom_line(size = 2) +
  xlab("") +
  ylab("Debt-to-GDP Ratio (%)") +
  ylim(50,100) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 14)) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20))

# Export with footnote:

ggsave("../When_where_and_why_material/output/figures/short-run forecasts/bias/aggregate/debt_path.pdf")


footnote=c("The simulation exercise follows closely De Resende (2014). Actual debth path is simulated with 
            growth rate equal to 2%, interest rate 2.5% and initial debt-to-gdp 70%. 
            Biased forecast for growth rate is 3%.") %>% 
  cat(file = "../When_where_and_why_material/output/figures/short-run forecasts/bias/aggregate/debt_path_footnote.tex")
