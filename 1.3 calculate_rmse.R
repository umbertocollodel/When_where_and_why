# Calculation RMSE 

final %>% 
  map(~ .x %>% mutate_at(vars(contains('variable')), .funs = list(rmse = ~rmse(targety_first, .))))