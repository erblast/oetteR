f_diff_of_means_medians = function(df, group, variable){
  
  
  data = df %>%
    select( group = one_of(group), variable = one_of(variable) )%>%
    mutate ( variable = f_bring_to_pos_range(variable) ) %>%
    group_by( group ) %>%
    summarise( means     = mean(variable,   na.rm = T)
               , medians = median(variable, na.rm = T) ) %>%
    ungroup() %>%
    summarise( diff_of_means        = max(means) - min(means)
               , diff_of_means_perc = ( ( max(means) - min(means) ) /max(means) ) *100
               , diff_of_medians      = max(medians) - min(medians)
               , diff_of_medians_perc = ( ( max(medians) - min(medians) ) /max(means) ) *100    )
  
  
  return(data)
}

f_max_diff_of_freq = function(df, var1, var2){
  
  t = table( df[[var1]], df[[var2]] ) %>%
    as_tibble() %>%
    group_by( Var1 ) %>% 
    mutate ( diff_var1          = ( max(n)-min(n) ) 
             , diff_var1_perc = ( ( max(n)-min(n))/ max(n) *100) 
    ) %>%
    group_by( Var2 ) %>%
    mutate ( diff_var2          = ( max(n)-min(n) ) 
             , diff_var2_perc = ( ( max(n)-min(n))/ max(n) *100) 
    ) %>%
    ungroup()%>%
    summarise( max_diff_freq          = max( c(diff_var1, diff_var2) )
               , max_diff_freq_perc  = max( c(diff_var1_perc, diff_var2_perc) ) 
    )
}

f_anova_stats = function(df, group, variables) {
  # returns a dataframe with anova stats
  # df        : dataframe
  # group     : grouping variable as character vector, must indicate factor variable
  # variables : numerical variables to be analyzed as character vector, must indicate numerical variable
  
  data = df
  
  formula = stringr::str_c('value~',group) %>%
    as.formula()
  
  df_anova = data %>%
    as_tibble() %>%
    select( one_of( c(group, variables) ) ) %>%
    gather(key = 'variable', value = 'value', one_of( variables ) ) %>%
    group_by( variable ) %>%
    nest( one_of(group), value) %>%
    mutate( model_anova     = purrr::map( data, ~aov( formula, data = .)) 
            , summary_anova = purrr::map( model_anova, summary)
            , summary_anova = purrr::map( summary_anova
                                          , function(x) x[[1]])
            , anova_pval    = purrr::map(summary_anova, 'Pr(>F)')
            , anova_pval    = purrr::map_dbl(anova_pval
                                             , function(x) x[1])
            , model_kruskal = purrr::map( data, ~kruskal.test(formula, data = .) )
            , kruskal_pval  = purrr::map_dbl(model_kruskal, 'p.value')
            , model_shapiro = purrr::map(data
                                         , function(x) f_stat_shapiro( x$value ) )
            , shapiro_stat  = purrr::map_dbl(model_shapiro,'statistic')
            , shapiro_pval  = purrr::map_dbl(model_shapiro,'p.value')
            , diff_df       = purrr::map( data, f_diff_of_means_medians, group = group, variable = 'value')
            ) %>%
   unnest(diff_df) %>%
    select(variable
           , shapiro_stat
           , shapiro_pval
           , anova_pval
           , kruskal_pval
           , diff_of_means
           , diff_of_means_perc
           , diff_of_medians
           , diff_of_medians_perc
    )
  
  return(df_anova)
}

f_stat_shapiro = function(vec){
  
  vec = sample( vec, 5000, replace = F)

  if( sd(vec) == 0 ){
    
    m = list( statistic = NA, p.value = NA )
    
  }else{
    
    m = shapiro.test(vec)
  }
  
  return(m)
}

f_chi_square = function(df, group, variables) {
  # returns a dataframe with anova stats
  # df        : dataframe
  # group     : grouping variable as character vector, must indicate factor variable
  # variables : numerical variables to be analyzed as character vector, must indicate factor variable
  
  data = df
  
  variables = variables[!variables == group]
  
  if(purrr::is_empty(variables)) return()
  
  df_chi = data %>%
    as_tibble() %>%
    select( one_of( c(group, variables) ) ) %>%
    gather(key = 'variable', value = 'value', one_of( variables  ) ) %>%
    group_by( variable ) %>%
    nest( one_of( group ), value) %>%
    mutate(  model_chi = purrr::map( data, ~chisq.test(x = .[[group]]
                                                       , y = .[['value']]
    ) )
    ,chi_pval  = purrr::map_dbl(model_chi, 'p.value')
    ,diff_df   = purrr::map(data, f_max_diff_of_freq, group, 'value')  ) %>%
    unnest(diff_df) %>%
    select(variable, chi_pval, max_diff_freq, max_diff_freq_perc)
  
  return(df_chi)
}


f_bring_to_pos_range = function(x){
  
  
  if( min(x)< 0) x = x + abs(min(x))
  
  
  return(x)
}
