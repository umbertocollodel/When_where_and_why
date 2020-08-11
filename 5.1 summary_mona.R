################ Script to produce summary stat MONA database: evolution
################ programs over time, top rankers and list of programs



# Summary stat ----

names=c("evolution","top_rankers")

evolution <- mona_rgdp %>% 
  group_by(year) %>%
  count() %>%
  ungroup() %>% 
  mutate(var_ma = zoo::rollmean(n, 3, align = "center", fill = NA))  %>%
  mutate(max_var=ifelse(var_ma>=quantile(var_ma,na.rm=T,p=0.99),var_ma,NA)) %>%
  filter(year <= 2018) %>% 
  ggplot(aes(year)) +
  geom_line(aes(y=var_ma),size = 1, col = "darkblue") +
  geom_col(aes(y=n), width = 0.3,fill="darkgrey",alpha = 0.4) +
  geom_point(aes(y=max_var),col="red",size=4)+
  theme_minimal() +
  xlab("") +
  ylab("") +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1)) +
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18))

top_rankers <- mona_rgdp %>% 
  group_by(country_code) %>%
  count() %>% 
  merge(geo_group) %>%
  as_tibble() %>% 
  mutate(country = countrycode(country_code,"imf","country.name")) %>%
  mutate(group = case_when(group == "africa" ~ "Africa",
                           group == "emerging_europe" ~ "Emerging Europe",
                           group == "latin_america" ~ "Latin America",
                           group == "middle_east" ~ "Middle East")) %>% 
  ungroup() %>%
  arrange(-n) %>%
  slice(1:20) %>% 
  mutate(country = reorder(as.factor(country),n)) %>% 
  ggplot(aes(country, n, fill = group)) +
  geom_col(width = 0.3) +
  coord_flip() +
  theme_minimal() +
  ylab("") +
  xlab("") +
  labs(fill = "") +
  theme(legend.position = "bottom") +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  theme(axis.text.x = element_text(size=18),
        axis.text.y = element_text(size = 18),
        legend.text = element_text(size = 16)) 


# Export:

list(evolution,top_rankers) %>% 
  walk2(names,~ ggsave(filename = paste0("../IEO_forecasts_material/output/figures/programs/summary/",.y,".pdf"),.x))


# Footnote:

footnote=c("Grey bars denote the number of countries entering an IMF program. 
           The blue line denotes the 3 years moving average and the red dot the highest value of the moving average over the period.") %>% 
  cat(file="../IEO_forecasts_material/output/figures/programs/summary/evolution_footnote.tex")



# Table appendix: list of programs -----


mona_rgdp %>% 
  select(-country_code, -contains("variable"),-original_duration) %>% 
  select(program_id, everything()) %>%
  filter(year < 2019) %>% 
  arrange(country) %>% 
  mutate(exceptional_access = str_replace(exceptional_access, "n.a.","/")) %>%
  mutate_at(vars(contains("amount")),round,2) %>% 
  mutate(date = as.character(date)) %>% 
  setNames(c("Program ID","Country","Date","Year","Type of program","Precautionary at approval","Exceptional access","Total amount drawn (mln SDR)","Total amount (% quota)")) %>% 
  stargazer(summary = F,
            rownames = F,
            out = "../IEO_forecasts_material/output/tables/programs/list_programs.tex")




