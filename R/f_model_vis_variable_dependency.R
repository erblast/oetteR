

f_model_plot_variable_dependency_regression = function(data
                                                      , model
                                                      , ranked_variables
                                                      , numericals
                                                      , col_vector
                                                      , limit = 10){

  ranked_variables = ranked_variables %>%
    arrange(rank) %>%
    head(limit)

  seq_range = function(min_, max_, is_numeric, col_var, data){

    if(is_numeric){
      seq( min_, max_, 500)
    }else{
      levels( data[, col_var] )
    }

  }

  tib = tibble( variable = ranked_variables[[1]]
                , is_numeric = variable %in% numericals
                , min_   = min( data[, variable] )
                , max_   = max( data[, variable] )
                , range  = pmap( list( min_, max_, is_numeric, variable)
                                 , seq_range
                                 , data
                                 )
                , preds  = modelr::data_grid(data, range, model)
                )

}
