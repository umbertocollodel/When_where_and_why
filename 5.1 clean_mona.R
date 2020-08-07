####### Script to clean the MONA database


# Clean MONA with macro-variables 2002-2020 ----
# Note: for the moment we focus only on board approval forecasts
# in the future work also on correction of forecasts with reviews.

mona_macro <- read_excel("../IEO_forecasts_material/raw_data/mona/mona_2002-2020_macro.xlsx") %>%
  filter(`Review Type` == "R0") %>% 
  filter(Description == "Gross domestic product, constant prices") %>% 
  select(`Arrangement Number`,`Country Name`,`Arrangement Type`,`Approval Date`,
         `Approval Year`,`T-1`,`T`,`T+1`) %>% 
  mutate(`T+1` = ((`T+1` - `T`)/`T`)*100,
         `T` = ((`T` - `T-1`)/`T-1`)*100) %>% 
  select(-`T-1`) %>% 
  setNames(c("program_id","country","program_type","date","year","variable1","variable2")) %>%
  mutate(country = str_to_sentence(tolower(country))) %>% 
  mutate(country_code = countrycode(country,"country.name","imf")) %>% 
  mutate(date = as.Date(date)) %>% 
  select(country_code, country, program_id,date, year, program_type, variable1, variable2)


# Simple analysis: evolution of programs over time and top rankers ----

names=c("evolution","top_rankers")

evolution <- mona_macro %>% 
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

top_rankers <- mona_macro %>% 
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



# Clean sheet with dummy for exceptional access: ----
# Note: origin for date format changes according to machine used.

mona_amount <- read_excel("../IEO_forecasts_material/raw_data/mona/mona_amounts.xlsx") %>% 
  select(-c(1, 7, 18, 20, 22, 24, 26, 28, 30, 35, 37, 40)) %>% 
  slice(-1) %>% 
  row_to_names(1) %>%
  select(`IFS Code`,Year,`Date of Arrangement`,`Precautionary at program approval`,`Exceptional Access`,`Total Approved Amount (percent of current quota)`) %>%
  setNames(c("country_code","year","date","precautionary","exceptional_access","amount_percent_quota")) %>% 
  mutate(country = countrycode(country_code,"imf","country.name")) %>%
  mutate(amount_percent_quota = as.numeric(amount_percent_quota)) %>%
  mutate(date = as.Date(as.numeric(date), origin = "1899-12-30")) %>% 
  filter(year >= 2002) %>% 
  select(country_code, country, everything()) 

# Combine the two:
# why the number of programs decreases?


final_mona <- merge(mona_macro, mona_amount) %>% 
  as_tibble()


# Table appendix: list of programs


final_mona %>% 
  select(-country_code, -contains("variable")) %>% 
  select(program_id, everything()) %>%
  filter(year < 2019) %>% 
  arrange(country) %>% 
  mutate(exceptional_access = str_replace(exceptional_access, "n.a.","/")) %>%
  mutate(amount_percent_quota = round(amount_percent_quota,2)) %>% 
  mutate(date = as.character(date)) %>% 
  setNames(c("Program ID","Country","Date","Year","Type of program","Precautionary at approval","Exceptional access","Total amount (% quota)")) %>% 
  stargazer(summary = F,
            rownames = F,
            out = "../IEO_forecasts_material/output/tables/programs/list_programs.tex")
  






