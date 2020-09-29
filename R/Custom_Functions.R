sum_weights <- function(var){

  formula <- as.formula(paste0("~",var))
  symbol <- as.symbol(var)

  summed <- survey::svytotal(formula,svy) %>%
    as_tibble(rownames = "variable") %>%
    rename(SE = !!{{symbol}} )

  return(summed)

}

sum_cat_var_weights_by_year <- function(var){

  formula <- as.formula(paste0("~",var))
  symbol <- as.symbol(var)

  total_year <- survey::svytotal(~year_factor,svy) %>%
    as_tibble(rownames = "year_factor") %>%
    rename(total_n=total) %>%
    mutate(year_factor = factor(stringr::str_remove(year_factor,"year_factor"),
                                levels = c("2012","2013","2014"))) %>%
    select(-SE)

  totals <- survey::svyby(formula,~year_factor,svy,svytotal) %>%
    pivot_longer(-year_factor,names_to = var,values_to = "n") %>%
    mutate(levels := stringr::str_remove({{symbol}},var)) %>%
    filter(!(stringr::str_detect(levels,"se"))) %>%
    left_join(total_year,by=c("year_factor")) %>%
    mutate(`(%)` = n/total_n*100) %>%
    pivot_wider(-total_n,names_from=year_factor,values_from = c("n","(%)")) %>%
    mutate(variable = var) %>%
    select(variable,levels,ends_with("2012"),ends_with("2013"),ends_with("2014"))



  return(totals)

}

sum_num_var_weights_by_year <- function(var){

  formula <- as.formula(paste0("~",var))
  symbol <- as.symbol(var)

  means <- survey::svyby(formula,~year_factor,svy,svymean) %>%
    select(-se) %>%
    rename(mean = !! {{symbol}})

  vars <- survey::svyby(formula,~year_factor,svy,svyvar) %>%
    mutate(sd = sqrt(!! {{symbol}})) %>%
    select(-se,-!! {{symbol}})


  means_sd <- left_join(means,vars,by="year_factor") %>%
    pivot_wider(names_from = year_factor,values_from = c("mean","sd")) %>%
    mutate(variable = var) %>%
    select(variable,ends_with("2012"),ends_with("2013"),ends_with("2014"))



  return(means_sd)

}
