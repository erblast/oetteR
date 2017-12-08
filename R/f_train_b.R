f_train_lasso_faster = function(data
                               , formula
                               , p = c( 1, 1.25, 1.5, 1.75, 2 )
                               , k = 5
                               , family = "gaussian"
                                ){

  # wrapper for pipelearner ----------------------------------------------

  wr_tweedie = function(data, formula, p_fact, k ){

    response_var = f_manip_get_response_variable_from_formula(formula)

    y = data[[response_var]]
    x = model.matrix(formula, data)[,-1]

    m = HDtweedie::cv.HDtweedie( x, y
                                 , p = p_fact
                                 , alpha = 1
                                 , nfolds = k
                                 , pred.loss = 'mse' )

  }

  wr_glmnet = function(data, formula, family, k ){

    response_var = f_manip_get_response_variable_from_formula(formula)

    data = as.data.frame(data)

    y = data[[response_var]]
    x = model.matrix(formula, data)[,-1]

    m =glmnet::cv.glmnet( x, y
                          , alpha = 1
                          , family = family
                          , nfolds = k
                          , type.measure = 'mse')

  }

  # make call container for progress output-------------------------------

  total_iterations = ( length(p)+1 ) * k * length(grid)

  call_cont = make_container_for_function_calls()
  call_cont$set_total(total_iterations)

  # standardize data------------------------------------------------------

  response_var = f_manip_get_response_variable_from_formula(formula)
  vars = f_manip_get_variables_from_formula(formula)

  data_res = select(data, one_of(response_var) )

  data_exp = select(data, one_of(vars) )%>%
    mutate_if( is.numeric, scale, center = T) %>%
    select_if( function(x) ! any(is.na(x)) ) ## remove columns containing NA values

  data = data_res %>%
    bind_cols(data_exp)

  new_formula = paste( response_var, '~', paste( names(data_exp), collapse = ' + ' ) ) %>%
    as.formula()

  # learn lasso-----------------------------------------------------------

  pl = pipelearner::pipelearner(data) %>%
    pipelearner::learn_models(models = c(call_cont$make_call)
                              , formulas = c(new_formula)
                              , .f = c(wr_glmnet)
                              , function_name = c('glmnet')
                              , family = family
                              , k = k
    )

  if( ! is.null(p) ){
    pl = pl %>%
      pipelearner::learn_models(models = c(call_cont$make_call)
                                , formulas = c(new_formula)
                                , .f = c(wr_tweedie)
                                , function_name = c('tweedie')
                                , p_fact = p
                                , k = k
      )
  }

  pl = pl %>%
    pipelearner::learn() %>%
    mutate(  function_name = map_chr(params, 'function_name')
    )

  pl_glm = pl %>%
    filter( function_name == 'glmnet') %>%
    mutate( distribution = map_chr(params, 'family')
            , lambda = map(fit, 'lambda')
            , mse = map(fit, 'cvm')
    ) %>%
    unnest( lambda, mse, .drop = F) %>%
    mutate( coeff = map2(fit, lambda, coef )
            , coeff = map(coeff, as.matrix )
            , coeff = map(coeff, as.data.frame )
            , coeff = map(coeff, function(x) mutate(x, coef = row.names(x))  )
    )

  if( ! is.null(p) ){
    pl_tweedie = pl %>%
      filter( function_name == 'tweedie') %>%
      mutate( distribution = map_chr(params, 'p_fact')
              , lambda = map(fit, 'lambda')
              , mse = map(fit, 'cvm')
      ) %>%
      unnest( lambda, mse, .drop = F) %>%
      mutate( coeff = map2(fit, lambda, coef )
              , coeff = map(coeff, as.matrix )
              , coeff = map(coeff, as.data.frame )
              , coeff = map(coeff, function(x) mutate(x, coef = row.names(x))  )
      )
  }else{
    pl_tweedie = NULL
  }

  pl_all = pl_glm %>%
    bind_rows(pl_tweedie) %>%
    mutate( title = paste(function_name, lambda, p))

  # unpack coefficients -------------------------------------------------

  pl_coef = pl_all %>%
    unnest(coeff, .drop = F ) %>%
    group_by(coef, p, lambda) %>%
    summarize( s0 = mean(s0) )

  # construct formulas for minimum rtmse --------------------------------

  if( k == 1){
    pl_pred = pl_all %>%
      f_predict_pl_regression(formula = new_formula, newdata = 'train')
  }else{
    pl_pred = pl_all %>%
      f_predict_pl_regression(formula = new_formula, newdata = 'test')
  }

  pl_pred = pl_pred %>%
    unnest(preds, .drop = F)

  pl_lab = pl_pred %>%
    group_by(title, lambda, p, models.id) %>%
    summarize()

  pl_pred_plot = pl_pred %>%
    f_predict_pl_regression_summarize() %>%
    left_join( pl_lab )


  pl_fin_grid = pl_coef %>%
    group_by(p, lambda) %>%
    summarise

  pl_fin_coef = pl_coef %>%
    ungroup() %>%
    filter( coef != '(Intercept)', ! near(s0,0) ) %>%
    rename( coeff = coef ) %>%
    nest( coeff, s0, .key = 'formula' )

  pl_fin = pl_fin_grid %>%
    left_join(pl_fin_coef) %>%
    mutate( formula = map( formula, function(x) x[['coeff']] )
            , n_coeff_after_lasso = map_int( formula, length )
            , n_coeff_before_lasso = ncol( model.matrix(new_formula, data) ) - 1
            , formula = map( formula, stringr::str_replace_all, '\\.', '_')
            , formula = map( formula, stringr::str_replace_all, ' ', '_')
            , formula = map( formula, paste, collapse = ' + ')
            , formula = map( formula, function(x) ifelse( x == '', 1,x) )
            , formula = map( formula, function(x) paste( response_var, '~', x) )
            , formula_str = unlist(formula)
            , formula = map( formula, as.formula )
    ) %>%
    left_join( pl_pred_plot ) %>%
    left_join( pl_all ) %>%
    select( distribution = p
            , lambda
            , rtmse
            , coef
            , n_coeff_before_lasso
            , n_coeff_after_lasso
            , formula_str
            , formula
    )

  pl_min = pl_pred_plot   %>%
    group_by(p) %>%
    filter( rtmse == min(rtmse) ) %>%
    filter( lambda == max(lambda) ) %>%
    select( distribution = p
            , lambda) %>%
    left_join( pl_fin )


  # plot ----------------------------------------------------------------

  pl_min_plot = pl_min %>%
    ungroup() %>%
    mutate( lambda = log(lambda)
            , distribution = as.factor(distribution) )

  p_rtmse = pl_pred_plot %>%
    ungroup() %>%
    mutate( lambda = log(lambda)
            , distribution = as.factor(p) ) %>%
    ggplot( aes( lambda
                 , rtmse
                 , fill = distribution
                 , color = distribution ) )+
    geom_line() +
    geom_point(size = 1) +
    geom_vline( data = pl_min_plot
                , mapping = aes(xintercept = lambda
                                , color = distribution ) ) +
    scale_fill_manual( values = f_plot_col_vector74() )+
    scale_color_manual( values = f_plot_col_vector74()) +
    theme( legend.position = 'bottom') +
    labs( x = 'log lambda')


  p_coef = pl_coef %>%
    ungroup() %>%
    mutate( lambda = log(lambda)
            , coef = as.factor(coef)
            , distribution = as.factor(p) ) %>%
    ggplot( aes(lambda
                , s0
                , fill = coef
                , color = coef ) ) +
    geom_point(size = 1) +
    geom_line( ) +
    geom_vline( data = pl_min_plot
                , mapping = aes(xintercept = lambda )
    ) +
    facet_wrap(~distribution, scales = 'free' ) +
    scale_fill_manual( values = f_plot_col_vector74() )+
    scale_color_manual( values = f_plot_col_vector74()) +
    theme( legend.position = 'bottom') +
    labs( y = 'normalized coefficient values'
          ,x = 'log lambda')

  # return --------------------------------------------------------------

  ret = list( plot_rtmse = p_rtmse
              , plot_coef = p_coef
              , tib_all = pl_fin
              , tib_min = pl_min)

  return(ret)
}


