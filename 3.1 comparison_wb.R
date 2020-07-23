################ Script to compare IMF WEO forecasts with World Bank GEP:

# Prepare comparison dataframe -----


# # World Bank data scraped manually:
# gep_data <- read_xlsx("../IEO_forecasts_material/raw_data/world bank/wb_gep_updated.xlsx") %>% 
#   rename_at(vars(matches("variable")), funs(str_replace(.,"variable","wb"))) %>% 
#   select(country, year, wb2, wb4) %>% 
#   rename(wb1 = wb2, wb2 = wb4)

# World Bank data (complete) from Ayuhan:
gep_data <- readRDS("../IEO_forecasts_material/intermediate_data/world bank/gdp_wb_cleaned.rds") %>% 
  select(country, year, wb2, wb4) %>%
  rename(wb1 = wb2, wb2 = wb4) %>% 
  filter(!country %in% country_to_exclude)
  

# IMF WEO January update data:


load("../IEO_forecasts_material/intermediate_data/rgdp_jan_update.RData")


# Actual values: (fall issue of next year WEO)

target <- final_sr$gdp %>% 
  select(country_code, year, group, targety_first)

# Bind together:

comparison_wb <- x %>%   
  merge(gep_data, by=c("country","year")) %>% 
  merge(target, by=c("country_code","year")) %>% 
  select(country_code, country, year, group, targety_first, variable1, wb1, variable2, wb2)  

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
    
    rmse_comparison <- comparison_wb %>% 
      group_by(country_code) %>%
      select(country_code, country, year, targety_first, variable1:variable2, wb1:wb2, group) %>% 
      summarise_at(vars(variable1:wb2), funs(hydroGOF::rmse(.,targety_first))) %>% 
      mutate(country = countrycode(country_code,"imf","country.name")) %>% 
      mutate(ratio1 = variable1/wb1 - 1,
             ratio2 = variable2/wb2 - 1) %>% 
      select(country_code,country,contains("ratio"))
    
    # Export
    
    
    rmse_comparison %>%
      select(country, contains("ratio")) %>% 
      mutate_at(vars(ratio1:ratio2), funs(round(.,digits = 2))) %>% 
      setNames(c("Country","H=0,J","H=1,J")) %>% 
      stargazer(summary= F,
                rownames = F,
                out = "../IEO_forecasts_material/output/tables/comparison/WB_updated/rmse_comparison_full.tex")
    
# Same figure with different groups: ----
# Enhancement: add the number of countries by plot
    
    group %>% 
      merge(rmse_comparison, by=c("country_code")) %>% 
      mutate_at(vars(ratio1:ratio2), funs(case_when(. < 0 ~ 1,
                                                    T ~ 0))) %>% 
      ungroup() %>% 
      group_by(group) %>% 
      summarise_at(vars(ratio1:ratio2), mean, na.rm = T) %>% 
      gather("horizon","share",ratio1:ratio2) %>% 
      mutate(horizon = case_when(horizon == "ratio1"~ "H=0,J",
                                 horizon == "ratio2"~ "H=1,J")) %>% 
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
    
    
ggsave("../IEO_forecasts_material/output/figures/comparison/WB_updated/comparison_rmse_group.pdf")
    

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
  summarise_at(vars(variable1:wb1), funs(hydroGOF::rmse(.,targety_first))) %>%
  mutate_at(vars(variable1:wb1), funs(round(.,2))) %>%
  setNames(c("Country","Current-year (IMF)","Current-year (WB)")) %>% 
  stargazer(summary = F,
            rownames = F,
            out = "../IEO_forecasts_material/output/figures/comparison/WB_updated/individual/summary_table.tex")



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

