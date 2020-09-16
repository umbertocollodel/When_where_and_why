# 


get_list_comparison <- function(data,export_path){
  comparison_wb %>% 
    group_by(country) %>% 
    slice(1) %>% 
    select(country, group) %>% 
    arrange(group) %>%
    rename(Country = country, `Geo. group` = group) %>% 
    stargazer(summary = F,
              rownames = F,
              out = paste0("../IEO_forecasts_material/output/tables/comparison/",export_path))
}


get_scatterplot <- function(data, ylab, ylimits = c(-20,20), xlimits=c(-20,20), issues = c("currentJun","currentJan","aheadJun","aheadJan"), export_path){
  
  list <- list(data %>% select(matches("1|group")),
               data %>% select(matches("2|group")),
               data %>% select(matches("3|group")),
               data %>% select(matches("4|group")) %>% 
          map(~ .x %>% select(group,everything())))
  
  
  scatter_forecasts <- list %>% 
    map(~ .x %>% 
          ggplot(aes_string(names(.x)[[2]],names(.x)[[3]],col=names(.x)[[1]])) +
          geom_point(size=4) +
          geom_abline(intercept = 0,slope = 1, color = "red", size=1.2) +
          theme_minimal() +
          xlim(ylimits) +
          ylim(xlimits) +
          xlab("WEO Forecasts") +
          ylab(ylab) +
          labs(col="") +
          theme(legend.position = "bottom") +
          theme(axis.text = element_text(size = 18),
                axis.title = element_text(size = 21),
                legend.title = element_text(size = 18),
                legend.text = element_text(size = 16)))
  
  scatter_forecasts %>% 
    map2(issues, 
         ~ ggsave(paste0("../IEO_forecasts_material/output/figures/comparison/",exporth_path,.y,".pdf"),plot = .x))
  
}


get_accuracy_summary <- function(data, issues_abbr, other_var){
  
  percentage <- group %>% 
    merge(rmse_comparison, by=c("country_code")) %>% 
    mutate_at(vars(ratio1:ratio4), funs(case_when(. < 0 ~ 1,
                                                  T ~ 0))) %>% 
    group_by(group) %>% 
    summarise_at(vars(ratio1:ratio4), mean, na.rm = T) %>% 
    mutate_at(vars(contains("ratio")),funs(round(.,2))) %>% 
    setNames(c("Geo.group",issues_abbr)) %>% 
    mutate(Variable = "Percentage")
  
  significance <- data %>% 
    mutate_at(vars(matches(paste0("variable|",other_var))),funs(targety_first - .)) %>%
    split(.$group) %>%
    map(~ list(.x %>% select(matches("1")),
               .x %>% select(matches("2")),
               .x %>% select(matches("3")),
               .x %>% select(matches("4")))) %>%
    modify_depth(2, ~ .x %>% filter(complete.cases(.))) %>% 
    modify_depth(2, ~ dm.test(.x[[1]],.x[[2]])) %>% 
    modify_depth(2, ~.x[["statistic"]]) %>% 
    map(~ .x %>% bind_cols()) %>% 
    bind_rows(.id = "Geo. group") %>% 
    setNames(c("Geo.group",issues_abbr)) %>% 
    mutate_at(vars(contains("=")), funs(round(.,2))) %>% 
    mutate_at(vars(contains("=")),funs(case_when(. > 1.96 | . < -1.96 ~ str_replace(as.character(.), "$","**"),
                                                 (. > 1.68 & . < 1.96) | (. < -1.68 & . > -1.96) ~ str_replace(as.character(.), "$", "*"),
                                                 TRUE ~ as.character(.)))) %>% 
    mutate(Variable = "DM Test")
  
  rbind(percentage,significance) %>% 
    arrange(Geo.group) %>% 
    select(Geo.group, Variable, everything())
}





get_accuracy_summary(comparison_wb, c("H=0,Jul.", "H=0,Jan.","H=1,Jul.","H=1,Jan."), "wb")


