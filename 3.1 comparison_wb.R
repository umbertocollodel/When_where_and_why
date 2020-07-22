################ Script to compare IMF WEO forecasts with World Bank GEP:

# Note: we are using the updated version of gep transcribed manually, needs
# to be changed with the total dataset received from WB.

# World Bank data:


# Data scraped manually:
gep_data <- read_xlsx("../IEO_forecasts_material/raw_data/world bank/wb_gep_updated.xlsx") %>% 
  rename_at(vars(matches("variable")), funs(str_replace(.,"variable","wb"))) %>% 
  select(country, year, wb2, wb4) %>% 
  rename(wb1 = wb2, wb2 = wb4)

# Data from Ayuhan:
gep_data <- readRDS("../IEO_forecasts_material/intermediate_data/world bank/gdp_wb_cleaned.rds") %>% 
  select(country, year, wb2, wb4) %>%
  rename(wb1 = wb2, wb2 = wb4)
  

# IMF WEO January update data:


load("../IEO_forecasts_material/intermediate_data/rgdp_jan_update.RData")


# Actual values: (fall issue of next year WEO)

target <- final_sr$gdp %>% 
  select(country_code, year, group, targety_first)

# Bind together:
# Note: removed Latvia, Lithuania and Vanatu because have important gap years after entering the sample 
# in 2010. In the future, check this automatically and remove countries.

comparison_wb <- x %>%   
  merge(gep_data, by=c("country","year")) %>% 
  merge(target, by=c("country_code","year")) %>% 
  select(country_code, country, year, group, targety_first, variable1, wb1, variable2, wb2) %>% 
  filter(country != "Latvia" & country != "Lithuania" & country != "Vanuatu") %>% 
  filter(country != "Japan" & country != "United States")
  

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

# Comparison of median forecast error: ----

comparison_wb %>%  
  mutate_at(vars(contains("wb")),funs(targety_first - .)) %>% 
  mutate_at(vars(contains("variable")),funs(targety_first - .)) %>% 
  group_by(year) %>% 
  mutate(median_1wb = median(wb1, na.rm = T), median_2wb = median(wb2, na.rm = T)) %>% 
  mutate(median_1imf = median(variable1, na.rm = T), median_2imf = median(variable2, na.rm = T)) %>% 
  ggplot(aes(year)) +
  geom_line(aes(y = median_1wb, group = 1, col = "WB"), size = 1) +
  geom_line(aes(y = median_1imf, group = 1, col = "IMF" ), size = 1) +
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
  ylim(-4,4)

ggsave("../IEO_forecasts_material/output/figures/comparison/WB_updated/current_year_comparison.pdf")


comparison_wb %>% 
  mutate_at(vars(contains("wb")),funs(targety_first - .)) %>% 
  mutate_at(vars(contains("variable")),funs(targety_first - .)) %>% 
  group_by(year) %>% 
  mutate(median_1wb = median(wb1, na.rm = T), median_2wb = median(wb2, na.rm = T)) %>% 
  mutate(median_1imf = median(variable1, na.rm = T), median_2imf = median(variable2, na.rm = T)) %>% 
  ggplot(aes(year)) +
  geom_line(aes(y = median_2wb, group = 1, col = "WB"), size = 1) +
  geom_line(aes(y = median_2imf, group = 1, col = "IMF" ), size = 1) +
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
        legend.text = element_text(size = 16)
  ) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  ylim(-4,4)


ggsave("../IEO_forecasts_material/output/figures/comparison/WB_updated/year_ahead_comparison.pdf")


# By country group: ----

comparison_wb %>% 
  mutate_at(vars(contains("wb")),funs(targety_first - .)) %>% 
  mutate_at(vars(contains("variable")),funs(targety_first - .)) %>% 
  split(.$group) %>% 
  map(~ .x %>% group_by(year)) %>% 
  map(~ .x %>% mutate(median_1wb = median(wb1, na.rm = T), median_2wb = median(wb2, na.rm = T))) %>% 
  map(~ .x %>% mutate(median_1imf = median(variable1, na.rm = T), median_2imf = median(variable2, na.rm = T))) %>% 
  map(~ .x %>% ggplot(aes(year)) +
        geom_line(aes(y = median_2wb, group = 1, col = "WB"), size = 1) +
        geom_line(aes(y = median_2imf, group = 1, col = "IMF" ), size = 1) +
        theme_minimal() +
        xlab("") +
        ylab("") +
        labs(col = "Institution") +
        theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1),
              legend.position = "bottom") +
        theme(axis.text = element_text(size = 18),
              axis.title = element_text(size = 21),
              legend.title = element_text(size = 18),
              legend.text = element_text(size = 16)
        ) +
        ylim(-4,4))


# Comparison of forecast accuracy: ----


# Diebold-Mariano (version in Timmerman paper)
# Note: very few observations, we ascribe the absence of significant differences to the low statistical 
# power of the test

comparison_wb %>% 
    mutate_at(vars(contains("wb")),funs(targety_first - .)) %>% 
    mutate_at(vars(contains("variable")),funs(targety_first - .)) %>%
    mutate(diff = wb1^2 - variable1^2) %>% 
    split(.$country) %>% 
    map(~ lm(diff ~ 1, .x)) %>%
    map(~ summary(.x)) %>% 
    map(~ coef(.x))

# Raw comparison (with no significance) of RMSE for countries and institution:
  
raw <- comparison_wb %>% 
  group_by(country) %>% 
  summarise(rmse_imf1 = hydroGOF::rmse(variable1, targety_first),
            rmse_wb1 = hydroGOF::rmse(wb1, targety_first),
            rmse_imf2 = hydroGOF::rmse(variable2, targety_first),
            rmse_wb2 = hydroGOF::rmse(wb2, targety_first)) %>% 
  ungroup() %>% 
  mutate(ratio1 = (rmse_imf1/rmse_wb1)- 1,
         ratio2 = (rmse_imf2/rmse_wb2) - 1) %>%
  mutate(better_imf1 = case_when(ratio1 < 0 ~ 1,
                                T ~ 0),
         better_imf2 = case_when(ratio2 < 0 ~ 1,
                                 T ~ 0)) %>% 
  ungroup() 

# list(raw %>% group_by(better_imf) %>% count(), raw %>% filter(better_imf == 0) %>% .$country %>% unique())
  
  raw %>% 
    gather("better_imf","value",better_imf1:better_imf2) %>% 
    group_by(better_imf, value) %>% 
    count() %>% 
    ungroup() %>% 
    mutate(value = case_when(value == 0 ~ "WB",
                                  T ~ "IMF")) %>%
    mutate(better_imf = case_when(better_imf == "better_imf1" ~ "Current Year",
                             T ~ "Year Ahead")) %>% 
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
    
    ggsave("../IEO_forecasts_material/output/figures/comparison/WB_updated/comparison_individual_countries.pdf")
    
  


# Forecast errors during recessions and expansions: (table) ----- 

comparison_wb %>% 
  mutate_at(vars(contains("wb")),funs(targety_first - .)) %>% 
  mutate_at(vars(contains("variable")),funs(targety_first - .)) %>% 
  mutate(recession = case_when(targety_first < 0 ~ 1,
                               T ~ 0)) %>% 
  group_by(recession) %>% 
  summarise(wb1 = round(median(wb1, na.rm = T),2), imf1 = round(median(variable1, na.rm = T),2),
            wb2 = round(median(wb2, na.rm = T),2),imf2 = round(median(variable2, na.rm = T),2)) %>%
  setNames(c("Recession","Current-year (WB)","Current-year (IMF)","Year-ahead (WB)","Year-ahead (IMF)")) %>%
  mutate(Recession = case_when(Recession == 1 ~ "Recession",
                               T ~ "Non-recession")) %>% 
  stargazer(rownames = F,
            summary = F,
            out = "../IEO_forecasts_material/output/tables/comparison/WB_updated/recession_forecast_error.tex")




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

  








