#' Analyse short-run forecasts bias
#' 
#' @param data df with forecasts at different horizons and actual values (named respectively variable1/2/3/4 
#' and targety_first)
#' @param regressions character vector. Different regressions to perform.
#' @param output_type character string. Either "appendix_table" or "share_plot". The first
#' produces a tex table with value of the intercept for each country and forecast horizon.
#' The second produces two plots with the share of countries with significant intercept
#' at each horizon (dividing between Spring and Fall.)
#' @param output_path character string. Path to export table or graph.
#' 

analyse_sr_bias <- function(data, regressions, output_type, output_path){

  # Run regressions for each country and formula, produces nested list:

  regressions <- data %>% 
    mutate_at(vars(contains("variable")),funs(targety_first - .)) %>%
    split(.$country_code) %>%
    map( ~ map(regressions, function(x){
      tryCatch(lm(x, .x), error = function(e){
        cat(crayon::red("Could not run the regression. Check data\n"))
      })
    })) 
  
  # Wrangle in dataframe format with estimate, country and horizon:
  
  df_bias <- regressions %>% 
    modify_depth(2, ~ tryCatch(summary(.x), error = function(e){
      cat(crayon::red("Could not run regression. Check dataframe \n"))
    })) %>% 
    modify_depth(2, ~ tryCatch(.x[["coefficients"]], error = function(e){
      cat(crayon::red("Could not run regression. Check dataframe \n"))
    })) %>% 
    map(~ discard(.x, ~ length(.x) != 4)) %>% 
    modify_depth(2, ~ as.data.frame(.x)) %>% 
    map(~ bind_rows(.x, .id = "horizon")) %>% 
    bind_rows(.id = "country_code") %>% 
    mutate(issue = case_when(as.numeric(horizon) %% 2 == 1 ~ "Fall",
                             T ~ "Spring"),
           horizon = case_when(horizon == 1 | horizon == 2 ~ "H=0",
                               horizon == 3 | horizon == 4 ~ "H=1")) %>% 
    mutate(Estimate = round(Estimate,2))  %>% 
    mutate(Estimate = case_when(`t value` > 1.96 | `t value` < -1.96 ~ str_replace(as.character(Estimate), "$","**"),
                                (`t value` > 1.68 & `t value` < 1.96) | (`t value` < -1.68 & `t value` > -1.96) ~ str_replace(as.character(Estimate), "$", "*"),
                                TRUE ~ as.character(Estimate))) %>% 
    mutate(country = countrycode(country_code,"imf","country.name")) %>% 
    select(country_code, country, horizon, issue, Estimate)
  
  # Produces output:
  
  if(output_type == "appendix_table"){
    
    df_bias %>%
      unite("horizon",horizon, issue, sep = ",") %>%
      select(-country_code) %>% 
      spread(horizon, Estimate) %>%
      rename(Country = country) %>% 
      stargazer(summary = F, 
                rownames = F,
                out = output_path)
    
  }
  
  else if(output_type == "share_plot"){
    
    df_bias %>% 
    split(.$horizon) %>% 
    map(~ .x %>% mutate(negative_significant = case_when(str_detect(Estimate, "\\*") & str_detect(Estimate, "-") ~ 1,
                                                         T ~ 0))) %>% 
    map(~ .x %>% mutate(positive_significant = case_when(str_detect(Estimate, "\\*") & str_detect(Estimate, "-", negate = T) ~ 1,
                                                         T ~ 0))) %>%
    map(~ .x %>% split(.$issue)) %>% 
    modify_depth(2, ~ .x %>% summarise_at(vars(contains("significant")), mean, na.rm = T)) %>% 
    map(~ .x %>% bind_rows(.id = "issue")) %>%
    bind_rows(.id = "horizon") %>% 
    gather("sign","share",negative_significant:positive_significant) %>% 
    mutate(sign = case_when(sign == "negative_significant" ~ "Optimistic",
                            T ~ "Pessimistic")) %>%
    split(.$issue) %>% 
    map(~ .x %>% 
          ggplot(aes(sign, share, fill = sign)) +
          geom_col(width = 0.4) +
          geom_text(aes(label = round(share,2)), size = 5, vjust = -0.5) +
          facet_wrap(~ horizon) +
          theme_minimal() +
          ylim(0,1)  +
          xlab("") +
          ylab("Share of countries (%)") +
          theme(legend.position = "bottom") +
          theme(strip.text.x = element_text(size = 16),
                axis.text.x = element_blank(),
                axis.text.y = element_text(size = 18),
                axis.title = element_text(size = 21),
                legend.text = element_text(size = 16)) +
          theme(panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank()) +
          labs(fill = ""))
    
    # Export:
    
    share_aggregate %>% 
      iwalk(~ ggsave(filename = paste0(output_path,.y,".pdf"),.x))
  }
}


  

