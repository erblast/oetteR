
f_clean_data = function(data
                        , max_number_of_levels_factors = 10
                        , min_number_of_levels_nums = 6
                        , exclude_missing = T
                        , replace_neg_values_with_zero = T
                        , allow_neg_values = c('null')
                        ){
  
  
  require(tidyverse)
  require(stringr)
  require(caret)
  require(forcats)
  
  data = data %>%
    as_tibble()
  
  numericals = names(data)[ map_lgl( data, is.numeric)]
  
  categoricals = names(data)[ ! map_lgl( data, is.numeric)]
  
  all_variables = names(data)
  
  # drop observations with missing Values
  
  if(exclude_missing == T) {
    
    print( paste('Number of excluded observations:', nrow(data[ ! complete.cases(data), ])) )
    
    data = data[stats::complete.cases(data),]
    
  }
  
  # add numericals with less than x distinc values to factors
  
  no_unique_vals = map_dbl(data[, numericals], function(x) length(unique(x)) )
  
  categoricals = c(categoricals, numericals[no_unique_vals <  min_number_of_levels_nums] )
  
  numericals = numericals[ !numericals %in% categoricals ]
  
  # replace negative values with zero
  
  if( replace_neg_values_with_zero == T){
    
    data = data %>%
      mutate_at( vars( numericals, - one_of(allow_neg_values)),
                 function(x) ifelse( x<0, 0, x ) )
  }
  
  # convert all non numericals and grouping variables to factors
  
  for (var in categoricals ){
    
    
    data[[var]] = as.factor(data[[var]])
    
  }
  
  # collapse smallest levels into one if factor levels exceed size x
  
  
  no_levels = map_dbl( data[, categoricals], function(x) length( levels(x) ) )  
  
  for (var in names( no_levels[no_levels > max_number_of_levels_factors] )) {
    
    data[[var]] = fct_lump(data[[var]], n = max_number_of_levels_factors)
  }
  
  
  # drop categoricals with only one level
  
  if( !is_empty(categoricals)){
    
    data = data %>%
      mutate_at( vars(one_of(categoricals) ) 
                 , fct_drop )
    
    no_lvl = data[,categoricals] %>%
      summarise_all( function(x) list(levels(x)) ) %>%
      summarise_all( function(x) length(x[[1]]) )
    
    only_one_lvl = categoricals[no_lvl == 1]
    
    if(length(only_one_lvl) > 0 ) {
      data = data %>%
        select( - one_of(only_one_lvl) )
      
      all_variables = all_variables[ !all_variables %in% only_one_lvl ]
      categoricals = categoricals[ no_lvl > 1]
      
    }
  } 
  
  # # remove none ascii characters from level names
  # 
  # for(categorical in categoricals){
  #   
  #   for( level in levels( data[[categorical]] ) ){
  #     
  #     non_ascii_name = stringi::stri_trans_general(level, "latin-ascii")
  #     
  #     data = data %>%
  #       mutate_( categorical = forcats::fct_recode( data[[categorical]], non_ascii_name = level) 
  #                )
  #   }
  #   
  # 
  # }
  
  
  # return statement
  
  return( list( data = data
                , numericals = numericals
                , categoricals = categoricals
                , all_variables = all_variables
                # , boxcox = names_transformed
  )
  )
}


f_boxcox = function(data_ls){
  
  require(tidyverse)
  require(stringr)
  require(caret)
  require(forcats)
  
  data          = data_ls$data
  numericals    = data_ls$numericals
  categoricals  = data_ls$categoricals
  all_variables = data_ls$all_variables
  
  data_clean_ls = data_ls

  # Boxcox transform numericals
  
  if(!is_empty(numericals)){
    
    data_box_cox = data[, numericals] %>%
      mutate_if( function(x) min(x) <= 0
                 , function(x) x + abs( min(x) ) + 0.00001 )
    
    
    
    trans = preProcess( as.data.frame(data_box_cox), c('BoxCox'), na.remove = T)
    
    pred = predict(trans, as.data.frame(data_box_cox) )
    
    names_transformed = names(pred) %>%
      str_c('_boxcox')
    
    names(pred) = names_transformed
    
    data = data %>%
      bind_cols(pred)
    
    if(!ncol(data[,numericals])
       == ncol(data[,names_transformed])) {
      stop( 'boxcox transformation unsucessfull')
    }
    
  } else {
    names_transformed = NULL
  }
  
  data_clean_ls$boxcox = pred
  
  return(data_clean_ls)
}

