#' Analyse short-run forecasts bias
#' 
#' @param data df with forecasts at different horizons and actual values (named respectively variable1/2/3/4 
#' and targety_first)
#' @param regressions character vector. Different regressions to perform.
#' @param output_type character string. Can be "appendix_table", "share_plot","magnitude_table",
#' "share_plot_geo" or "magnitude_table_geo". 
#' @param export_path character string. Path to export table or graph.
#' @return Output depends on parameter output type.
#' #' The first produces a tex table with value of the intercept for each country and forecast horizon.
#' The second produces two plots with the share of countries with significant intercept
#' at each horizon (dividing between Spring and Fall.)
#' The third produces a table with the summary statistics of biases for each horizon and type
#' of bias i.e optimistic or pessimistic. For each horizon, Spring and Fall issues are pooled together.
#' The fourth produces the same plots as the second, but dividing by WEO geographical group.
#' The fifth produces the same table as the third, but dividing by WEO geographical group.
#' 

analyse_sr_bias <- function(data, regressions, output_type, export_path){

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
                out = export_path)
    
  }
  
  else if(output_type == "share_plot"){
    
    share_aggregate <- df_bias %>% 
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
          scale_fill_grey() +
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
      iwalk(~ ggsave(filename = paste0(export_path,.y,".pdf"),.x))
  }
  
  else if(output_type == "magnitude_table"){
    
    table_magnitude <- df_bias %>% 
      split(.$horizon) %>% 
      map(~ .x %>% filter(str_detect(Estimate,"\\*"))) %>%
      map(~ .x %>% mutate(negative = case_when(str_detect(Estimate,"-") ~ 1,
                                               T ~ 0))) %>% 
      map(~ .x %>% mutate(Estimate = as.numeric(str_remove(Estimate, "\\*+")))) %>% 
      map(~ .x %>% group_by(negative) %>% summarise(mean_bias = round(mean(Estimate, na.rm = T),2),
                                                    median_bias = round(median(Estimate, na.rm = T),2),
                                                    max_bias = round(max(Estimate),2),
                                                    min_bias = round(min(Estimate),2))) %>% 
      bind_rows(.id = "horizon") %>% 
      mutate(negative = case_when(negative == 1 ~ "Optimistic",
                                  T ~ "Pessimistic")) %>% 
      arrange(negative) %>% 
      setNames(c("Horizon","Type of bias","Mean","Median", "Min.", "Max."))
    
    
    table_magnitude %>% 
      stargazer(summary = F,
                rownames = F,
                out = export_path)
    
  }
  
  else if(output_type == "share_plot_geo"){
     
    share_aggregate_group <- df_bias %>% 
      merge(geo_group,by=c("country_code")) %>%
      split(.$horizon) %>% 
      map(~ .x %>% mutate(negative_significant = case_when(str_detect(Estimate, "\\*") & str_detect(Estimate, "-") ~ 1,
                                                           T ~ 0))) %>% 
      map(~ .x %>% mutate(positive_significant = case_when(str_detect(Estimate, "\\*") & str_detect(Estimate, "-", negate = T) ~ 1,
                                                           T ~ 0))) %>%
      map(~ .x %>% split(.$issue)) %>% 
      modify_depth(2, ~ .x %>% group_by(group)) %>% 
      modify_depth(2, ~ .x %>% summarise_at(vars(contains("significant")), mean, na.rm = T)) %>%
      map(~ .x %>% bind_rows(.id = "issue")) %>%
      bind_rows(.id = "horizon") %>% 
      gather("sign","share",negative_significant:positive_significant) %>% 
      mutate(sign = case_when(sign == "negative_significant" ~ "Optimistic",
                              T ~ "Pessimistic")) %>% 
      split(.$issue) %>% 
      map(~ .x %>% 
            ggplot(aes(group,share, fill = sign)) +
            geom_col(position = "dodge",width = 0.5) +
            coord_flip() +
            facet_wrap(~horizon) + 
            theme_minimal() +
            xlab("") +
            ylab("Share of countries (%)") +
            scale_fill_grey() +
            theme(legend.position = "bottom") +
            labs(fill="") +
            theme(strip.text.x = element_text(size = 16),
                  axis.text.x = element_text(size = 16),
                  axis.text.y = element_text(size = 18),
                  axis.title = element_text(size = 21),
                  legend.text = element_text(size = 16))
      )
    
    share_aggregate_group %>% 
      iwalk(~ ggsave(filename = paste0(export_path,.y,"_group.pdf"),.x))
  }
    
  else if(output_type == "magnitude_table_geo"){
      
    df_bias %>% 
      merge(geo_group,by=c("country_code")) %>%
      split(.$horizon) %>% 
      map(~ .x %>% filter(str_detect(Estimate,"\\*"))) %>% 
      map(~ .x %>% filter(str_detect(Estimate,"-"))) %>% 
      map(~ .x %>% group_by(group)) %>% 
      map(~ .x %>% mutate(Estimate = as.numeric(str_remove_all(Estimate,"\\*")))) %>% 
      map(~ .x %>% summarise(mean = round(mean(Estimate, na.rm = T),2),
                             median = round(median(Estimate, na.rm = T),2),
                             max = round(min(Estimate, na.rm = T),2))) %>% 
      bind_rows(.id = "horizon") %>%
      setNames(c("Horizon","Geo. group","Mean","Median","Max.")) %>% 
      stargazer(summary = F,
                rownames = F,
                out = export_path)
    
    }
  }



#' Analyse efficiency short-run forecasts
#' 
#' @param fe_df dataframe with forecast errors
#' @param centre_df list of dataframes with growth forecasts centre countries
#' @param list_regressions character vector. List of regressions to perform
#' @param condition character string. Conditions to filter the dataframe before regressions: it allows comparison
#' between different time periods and can be vectorized easily.
#' @return List of dataframes for each centre country. Each dataframe contains point estimate regression for each forecast horizon and geographical group as
#' well as upper and lower bounds CI.


analyze_sr_efficiency <- function(fe_df, centre_df, list_regressions, condition){
  
  regressions <- centre_countries %>% 
    map(~ merge(fe,.x, by=c("year"))) %>%
    map(~ .x %>% filter_(condition)) %>% 
    map(~ as.tibble(.x)) %>% 
    map(~ split(.x, .x$group)) %>% 
    modify_depth(2, ~ map(list_regressions, function(x){
      tryCatch(lm(x, .x), error = function(e){
        cat(crayon::red("Could not run the regression. Check data\n"))
      })})) 
  
  # Wrangle in table format: ----
  
  
  confint <- regressions %>% 
    modify_depth(3, ~ confint(.x)) %>% 
    modify_depth(3, ~ .x %>% as_tibble() %>%  slice(2)) %>% 
    modify_depth(2, ~ .x %>% bind_rows(.id = "horizon")) %>% 
    map(~ .x %>% bind_rows(.id = "group")) %>% 
    map(~ .x %>% mutate_at(vars(matches("\\.")), funs(round(., 2))))  %>% 
    map(~ .x %>% mutate(horizon = case_when(horizon == 1 ~ "H=0,F",
                                            horizon == 2 ~ "H=0,S",
                                            horizon == 3 ~ "H=1,F",
                                            T ~ "H=1,S"))) %>% 
    map(~ .x %>% setNames(c("group","horizon",paste0("lower",str_extract(condition,".{2}\\d{4}$")),paste0("upper",str_extract(condition,".{2}\\d{4}$")))))
  
  
  estimates <- regressions %>% 
    modify_depth(3, ~ summary(.x)) %>% 
    modify_depth(3, ~ .x[["coefficients"]]) %>% 
    modify_depth(3, ~ .x %>% as_tibble() %>%  slice(2)) %>% 
    modify_depth(2, ~ .x %>% bind_rows(.id = "horizon")) %>% 
    map(~ .x %>% bind_rows(.id = "group")) %>% 
    map(~ .x %>% mutate(Estimate = round(Estimate, 2)))  %>% 
    map(~ .x %>% mutate(horizon = case_when(horizon == 1 ~ "H=0,F",
                                            horizon == 2 ~ "H=0,S",
                                            horizon == 3 ~ "H=1,F",
                                            T ~ "H=1,S")))  %>% 
    map(~ .x %>% select(group, horizon, Estimate)) %>% 
    map(~ .x %>% setNames(c("group","horizon",paste0("estimate",str_extract(condition,".{2}\\d{4}$"))))) 
  
  
  estimates %>%
    map2(confint, ~ .x %>% merge(.y, by=c("group","horizon")))
  
}






