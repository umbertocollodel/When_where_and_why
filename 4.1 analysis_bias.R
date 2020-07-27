list_regressions=c(variable7 ~ 1, variable8 ~ 1, variable9 ~ 1,
                   variable10 ~ 1, variable11 ~ 1, variable12 ~ 1)


gdp_medium %>% 
  mutate_at(vars(matches("variable")), funs(targety_first - .)) %>% 
  split(.$country) %>% 
  map( ~ map(list_regressions, function(x){
    tryCatch(lm(x, .x), error = function(e){
      cat(crayon::red("Could not run the regression. Check data\n"))
    })
  }))

  