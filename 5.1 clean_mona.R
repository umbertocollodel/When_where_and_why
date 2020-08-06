
series=c("RGDPC","PCPIC","BCAX","GPB_Y")

list_df <- series %>% 
  map(~ read.csv("../IEO_forecasts_material/raw_data/mona/ArchMecon.csv") %>% filter(str_detect(Mneumonic,.x))) %>%
  map(~ .x %>% as_tibble())


names(list_df) <- series


# Keep only the approval:


list_df$RGDPC %>% 
  filter(`Review.Type` == "OldBoardApproval") %>% 
  select(`Country.Code`,`Country.Name`, `Arrangement.Type`,`Approval.Date`,`T`,`T.1.1`) %>%
  mutate_at(vars(matches("\\.")),funs(str_remove_all(.,"\\s")))

# Need to convert the factors without messing up!



# Need to merge with the target year!


# Need to understand how to define (in the data) - find a variable - for big programs!