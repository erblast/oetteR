
#' @title container for function calls, can be used as a progress bar
#' @description creates a closure with a make_call() method that wraps
#' any function call. When the wrapper is used the function call is saved
#' and the calls are counted and the progress is being printed. Use the method
#' set_total() to input the total number of function calls. Based on the total
#' an ETA is estimated and a percentage calculated.

#' @return container
#' @details DETAILS
#' @examples
#'
#' .f = randomForest::randomForest
#' call_cont = make_container_for_function_calls()
#' call_cont$set_total(4)
#' m_wr = call_cont$make_call( .f = .f, formula = disp~., data = mtcars )
#'
#' #pipe version
#' call_cont = make_container_for_function_calls()
#' call_cont$set_total(5)
#'
#' pl = pipelearner::pipelearner(mtcars) %>%
#'   pipelearner::learn_models( models = c( call_cont$make_call )
#'                              , formulas = c(disp~.)
#'                              , .f = c( randomForest::randomForest )
#'                              , function_name = 'randomForest'
#'                              , print_call = c(T)
#'                            ) %>%
#'   pipelearner::learn_cvpairs( pipelearner::crossv_kfold, k = 5 ) %>%
#'   pipelearner::learn()
#'
#' @seealso
#'  \code{\link[lubridate]{now}},\code{\link[lubridate]{time_length}}
#' @rdname make_container_for_function_calls
#' @export
#' @importFrom lubridate now time_length
make_container_for_function_calls = function(){

  #attributes ----------------------------

  total      = NULL
  calls      = list()
  counter    = 1
  time_start = NULL
  time_end   = NULL
  function_names = vector()

  #methods -------------------------------

  set_total = function(total){

    total          <<- total
    calls          <<- list()
    counter        <<- 1
    time_start     <<- NULL
    time_end       <<- NULL
    function_names <<- vector()

  }

  get_call = function(call){

    call = as.character(call)
    call = call[ ! startsWith(call, 'list')]
    call = call[ ! startsWith(call, 'function')]
    call = paste( call, collapse = ' ; ')

  }

  make_call = function( .f
                        , function_name = ''
                        , print_progress = T
                        , print_call = F
                        , ... ){

    if( counter == 1) time_start <<- lubridate::now()

    if( is.null(total) ) stop('Call Container: Set expected total runs first!')

    x = .f(...)

    if( 'call' %in% names(x) ){
      call = paste( get_call( match.call() ), get_call(x$call), collapse = ' ; ' )
    }else{
      call = get_call( match.call() )
    }

    calls <<- f_manip_append_2_list( calls, call )
    function_names <<- c( function_names, function_name )

    if( print_call ) print( call )

    perc               = round( counter / total * 100, 1 )
    time_elapsed_s     = (lubridate::now() - time_start) %>% lubridate::time_length()
    time_predicted_s   = time_elapsed_s/counter * (total - counter)
    time_elapsed_min   = round(time_elapsed_s   / 60, 1)
    time_predicted_min = round(time_predicted_s / 60, 1)

    if(print_progress) print( paste( 'Progress:', counter, '/', total, ';'
                                     , round(perc,1), '%', ';'
                                     , 'ETA:', time_predicted_min, 'min'
    )
    )


    if( counter == total ) time_end <<- lubridate::now()

    counter <<- counter + 1

    return( x )

  }

  get_calls = function(){
    return(calls)
  }

  get_total = function(){
    return(total)
  }

  get_counter = function(){
    return(counter)
  }

  get_run_time = function(){
    return( time_start - time_end )
  }

  get_function_names = function(){
    return( function_names )
  }


  #returns--------------------------------

  return( list( set_total = set_total
                , make_call  = make_call
                , get_total = get_total
                , get_calls = get_calls
                , get_counter = get_counter
                , get_function_names = get_function_names
  )
  )


}


#' @title wrapper for glmnet and HDtweedie
#' @description performs lasso for different distributions, returns a list of
#'   formulas that result in the lowest rtmse for at least one of the
#'   distributions. Graphical output allows side-by-side comparison of lasso
#'   behaviour for all distributions.
#' @param data dataframe
#' @param formula formula
#' @param grid grid values for lambda, Default: 10^seq(4, -4, length = 100)
#' @param p p parameter for tweedie distributions, set p = NULL for not
#'   performing lasso for tweedie distributions, Default: c(1, 1.25, 1.5, 1.75,
#'   2)
#' @param k fold cross validation, set to 1 for testing against training data,
#'   Default: 5
#' @param family family parameter for glmnet, can be a vector, Default:
#'   'gaussian'
#' @return list()
#' @details Columns containing NA will be removed, formula cannot be constructed
#'   with '.'
#' @examples
#' data = MASS::quine
#' formula = Days~.
#'
#' lasso = f_train_lasso(data, formula, p = NULL, k = 1
#'                      , grid = 10^seq(3,-3,length= 25) )
#' lasso = f_train_lasso(data, formula, p = 1.5, k = 2
#'                      , grid = 10^seq(3,-3,length= 25) )
#'
#' lasso
#' @seealso \code{\link[HDtweedie]{HDtweedie}} \code{\link[glmnet]{glmnet}}
#'   \code{\link[pipelearner]{pipelearner}},\code{\link[pipelearner]{learn_models}},\code{\link[pipelearner]{learn_cvpairs}},\code{\link[pipelearner]{learn}}
#'
#'
#'
#'
#' @rdname f_train_lasso_manual_cv
#' @importFrom HDtweedie HDtweedie
#' @importFrom glmnet glmnet
#' @importFrom pipelearner pipelearner learn_models learn_cvpairs learn
f_train_lasso_manual_cv = function(data
                         , formula
                         , grid = 10^seq(4,-4,length= 100)
                         , p = c( 1, 1.25, 1.5, 1.75, 2 )
                         , k = 5
                         , family = "gaussian"
                         ){

  # wrapper for pipelearner ----------------------------------------------

  wr_tweedie = function(data, formula, lambda, p_fact ){

    response_var = f_manip_get_response_variable_from_formula(formula)

    y = data[[response_var]]
    x = model.matrix(formula, data)[,-1]

    m = HDtweedie::HDtweedie(x,y, lambda = lambda, p = p_fact, alpha = 1, standardize = F, ... )

  }

  wr_glmnet = function(data, formula, lambda, family ){

    response_var = f_manip_get_response_variable_from_formula(formula)

    data = as.data.frame(data)

    y = data[[response_var]]
    x = model.matrix(formula, data)[,-1]

    m =glmnet::glmnet(x,y, lambda = lambda, alpha = 1, family = family, standardize = F, ... )

  }

  # make call container for progress output-------------------------------

  total_iterations = ( length(p)+1 ) * k * length(grid)

  call_cont = make_container_for_function_calls()
  call_cont$set_total(total_iterations)

  # standardize data------------------------------------------------------

  # if binomial distribution is asked of glmnet Tweedie distribution
  # dont make sense.

  if (family == 'binomial'){
    p = NULL
  }

  response_var = f_manip_get_response_variable_from_formula(formula)
  vars = f_manip_get_variables_from_formula(formula)

  data_res = select(data, one_of(response_var) )

  data_exp = select(data, one_of(vars) )%>%
    # mutate_if( is.numeric, scale, center = T) %>%
    select_if( function(x) ! any(is.na(x)) ) ## remove columns containing NA values

  data = data_res %>%
    bind_cols(data_exp)

  new_formula = paste( response_var, '~', paste( names(data_exp), collapse = ' + ' ) ) %>%
    as.formula()

  # learn lasso-----------------------------------------------------------

  suppressWarnings({

    pl = pipelearner::pipelearner(data) %>%
      pipelearner::learn_models(models = c(call_cont$make_call)
                                , formulas = c(new_formula)
                                , .f = c(wr_glmnet)
                                , function_name = c('glmnet')
                                , lambda = grid
                                , family = family

      )

    if( ! is.null(p) ){
      pl = pl %>%
        pipelearner::learn_models(models = c(call_cont$make_call)
                                  , formulas = c(new_formula)
                                  , .f = c(wr_tweedie)
                                  , function_name = c('tweedie')
                                  , lambda = grid
                                  , p_fact = p

        )
    }

    if( k == 1){
      pl = pl %>%
        pipelearner::learn_cvpairs( pipelearner::crossv_mc, n = 1, test = 0.01)
    }else{
      pl = pl %>%
        pipelearner::learn_cvpairs( pipelearner::crossv_kfold, k = k )
    }

    pl = pl %>%
      pipelearner::learn() %>%
      mutate(  lambda = map_dbl(params, 'lambda')
               , function_name = map_chr(params, 'function_name')
      )

  })

  pl_glm = pl %>%
    filter( function_name == 'glmnet') %>%
    mutate( p = map_chr(params, 'family')
            , coef = map(fit, coef )
            , coef = map(coef, as.matrix )
            , coef = map(coef, as.data.frame )
            , coef = map(coef, function(x) mutate(x, coef = row.names(x))  )
    )

  if( ! is.null(p) ){
    pl_tweedie = pl %>%
      filter( function_name == 'tweedie') %>%
      mutate( p = map_dbl(params, 'p_fact')
              , p = paste('Tweedie, p:', round(p,1) )
              , coef = map(fit, coef )
              , coef = map(coef, as.matrix )
              , coef = map(coef, as.data.frame )
              , coef = map(coef, function(x) mutate(x, coef = row.names(x))  )
      )
  }else{
    pl_tweedie = NULL
  }

  pl_all = pl_glm %>%
    bind_rows(pl_tweedie) %>%
    mutate( title = paste(function_name, lambda, p))

  # unpack coefficients -------------------------------------------------

  pl_coef = pl_all %>%
    unnest(coef, .drop = F ) %>%
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

  p_mse = pl_pred_plot %>%
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

  ret = list( plot_rtmse = p_mse
              , plot_coef = p_coef
              , tib_all = pl_fin
              , tib_min = pl_min)

  return(ret)
}


#' @title wrapper for cv.glmnet and cv.HDtweedie
#' @description performs lasso for different distributions, returns a list of
#'   formulas that result in the lowest mse for at least one of the
#'   distributions. Graphical output allows side-by-side comparison of lasso
#'   behaviour for all distributions.
#' @param data dataframe
#' @param formula formula
#' @param p p parameter for tweedie distributions, set p = NULL for not
#'   performing lasso for tweedie distributions, Default: c(1, 1.25, 1.5, 1.75,
#'   2)
#' @param k fold cross validation, Default: 5
#' @param family family parameter for glmnet, can be a vector, Default:
#'   'gaussian'. For classification use 'binomial'. Performance metric MSE will be
#'   replaced with AUC.
#' @param ... arguments passed to cv.glmnet, cv.HDtweedie such as lambda or n_lambda
#' @return list()
#' @details Columns containing NA will be removed, formula cannot be constructed
#'   with '.', use family = 'binomial for classification'.
#'
#'   !!! Watchout the Data will not be scaled automatically.
#'
#' @examples
#'
#' #regular regression
#'
#' data = MASS::quine
#' formula = Days ~ Eth + Sex + Age + Lrn
#'
#' # here we scale, center and create sensibly named dummy variables
#' trans_ls = f_manip_data_2_model_matrix_format( data, formula )
#'
#'
#' lasso = f_train_lasso(trans_ls$data, trans_ls$formula, p = NULL, k = 3
#'                      , lambda = 10^seq(3,-3,length= 25) )
#' lasso = f_train_lasso(trans_ls$data, trans_ls$formula, p = 1.5, k = 3
#'                      , lambda = 10^seq(3,-3,length= 25) )
#'
#' lasso
#'
#'
#' #classification
#'
#' # here we transform double to factor
#' data_ls = mtcars %>%
#'   f_clean_data()
#'
#' formula = vs ~ cyl + mpg + disp + hp + drat + wt + qsec + am + gear + carb
#'
#' # here we scale, center and create sensibly named dummy variables
#' trans_ls = f_manip_data_2_model_matrix_format( data_ls$data, formula )
#'
#' lasso = f_train_lasso( trans_ls$data
#'                       , trans_ls$formula
#'                       , p = NULL
#'                       , family = 'binomial'
#'                       , k = 3
#'  )
#'
#' lasso
#'
#' @seealso
#',\code{\link[HDtweedie]{HDtweedie}}
#',\code{\link[glmnet]{glmnet}}
#',\code{\link[HDtweedie]{cv.HDtweedie}}
#',\code{\link[glmnet]{cv.glmnet}}
#',\code{\link[pipelearner]{pipelearner}}
#',\code{\link[pipelearner]{learn_models}}
#',\code{\link[pipelearner]{learn_cvpairs}}
#',\code{\link[pipelearner]{learn}}
#'
#'
#'
#'
#' @rdname f_train_lasso
#' @export
#' @importFrom HDtweedie HDtweedie cv.HDtweedie
#' @importFrom glmnet glmnet cv.glmnet
#' @importFrom pipelearner pipelearner learn_models learn_cvpairs learn

f_train_lasso = function(data
                         , formula
                         , p = c( 1, 1.25, 1.5, 1.75, 2 )
                         , k = 5
                         , family = "gaussian"
                         , ...
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
                                 , pred.loss = 'mse'
                                 , standardize = FALSE
                                 , ... )

  }

  wr_glmnet = function(data, formula, family, k ){

    response_var = f_manip_get_response_variable_from_formula(formula)

    data = as.data.frame(data)

    y = data[[response_var]]
    x = model.matrix(formula, data)[,-1]

    if( family == 'binomial'){
      type.measure = 'auc'
    }else{
      type.measure = 'deviance'
    }

    m =glmnet::cv.glmnet( x, y
                          , alpha = 1
                          , family = family
                          , nfolds = k
                          , type.measure = type.measure
                          , standardize = FALSE
                          , ... )

  }

  # make call container for progress output-------------------------------

  total_iterations = ( length(p)+1 )

  call_cont = make_container_for_function_calls()
  call_cont$set_total(total_iterations)

  # standardize data------------------------------------------------------

  response_var = f_manip_get_response_variable_from_formula(formula)
  vars = f_manip_get_variables_from_formula(formula)

  data_res = select(data, one_of(response_var) )

  data_exp = select(data, one_of(vars) )%>%
    # mutate_if( is.numeric, scale, center = T) %>%
    select_if( function(x) ! any(is.na(x)) ) ## remove columns containing NA values

  data = data_res %>%
    bind_cols(data_exp)

  new_formula = paste( response_var, '~', paste( names(data_exp), collapse = ' + ' ) ) %>%
    as.formula()

  # learn lasso-----------------------------------------------------------

  suppressWarnings({


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
      pipelearner::learn_cvpairs( pipelearner::crossv_mc, n = 1, test = 0.00000001) %>%
      pipelearner::learn() %>%
      mutate(  function_name = map_chr(params, 'function_name')
      )

  })

  pl_glm = pl %>%
    filter( function_name == 'glmnet') %>%
    mutate( distribution = map_chr(params, 'family') )

  if( ! is.null(p) ){
    pl_tweedie = pl %>%
      filter( function_name == 'tweedie') %>%
      mutate( distribution = map_dbl(params, 'p_fact')
              , distribution = map_chr(distribution
                                       , function(x) paste('Tweedie, p =', round(x,1) )
              )
      )
  }else{
    pl_tweedie = NULL
  }

  pl_all = pl_glm %>%
    bind_rows(pl_tweedie) %>%
    mutate( lambda = map(fit, 'lambda')
            , mse = map(fit, 'cvm')
            , lambda_min = map_dbl(fit, 'lambda.min')
            , lambda_1se = map_dbl(fit, 'lambda.1se')
    ) %>%
    unnest( lambda, mse, .drop = F) %>%
    mutate( coeff = map2(fit, lambda, coef )
            , coeff = map(coeff, as.matrix )
            , coeff = map(coeff, as.data.frame )
            , coeff = map(coeff, function(x) mutate(x, coef = row.names(x))  )
            , coeff = map(coeff, rename, value = '1' )
            , formula = coeff
            #, coeff = map(coeff, spread, key = coef, value = value )
    )


  # construct formulas ---------------------------------------------------


  pl_fin = pl_all %>%
    mutate( formula = map(formula, filter, coef != '(Intercept)' )
            , formula = map(formula, filter,  ! near( value, 0) )
            , formula = map(formula, 'coef')
            , n_coeff_after_lasso = map_int( formula, length )
            , n_coeff_before_lasso = ncol( model.matrix(new_formula, data) ) - 1
            , formula = map( formula, stringr::str_replace_all, '\\.', '_')
            , formula = map( formula, stringr::str_replace_all, ' ', '_')
            , formula = map( formula, paste, collapse = ' + ')
            , formula = map( formula, function(x) ifelse( x == '', 1,x) )
            , formula = map( formula, function(x) paste( response_var, '~', x) )
            , formula_str = unlist(formula)
            , formula = map( formula, as.formula )
    )%>%
    select( distribution
            , lambda
            , lambda_min
            , lambda_1se
            , mse
            , coeff
            , formula
            , formula_str
            , n_coeff_before_lasso
            , n_coeff_after_lasso
            )

  formula_str_lambda_min = pl_fin %>%
    filter( lambda == lambda_min ) %>%
    .$formula_str %>%
    unique()

  formula_str_lambda_1se = pl_fin %>%
    filter( lambda == lambda_1se ) %>%
    .$formula_str %>%
    unique()

  # plot ----------------------------------------------------------------

  pl_min_plot = pl_fin %>%
    group_by( distribution, lambda_min, lambda_1se) %>%
    summarise() %>%
    gather(key = 'lambda_type', value = 'lambda_value', lambda_min, lambda_1se ) %>%
    ungroup() %>%
    mutate( lambda_value = log(lambda_value)
            , lambda_type = as.factor(lambda_type)
            , distribution = as.factor(distribution)
    )

  #plotly fucks up the legend if we add it using only one dataframe

  pl_lambda_min = pl_min_plot %>%
    filter(lambda_type == 'lambda_min')

  pl_lambda_1se = pl_min_plot %>%
    filter(lambda_type == 'lambda_1se')

  # if the number of colors superseeds tha number of colors of the
  # manual scale ggplot raises an error.

  n_distributions = unique( pl_all$distribution ) %>% length
  n_coef          = unique( pl_all$coeff)         %>% length

  p_mse = pl_all %>%
    ungroup() %>%
    mutate( lambda = log(lambda)
            , distribution = as.factor(distribution) ) %>%
    # we have to limit the selection of columns because the ggplot obj
    # will save the entire dataframe and memory will explode for large datasets
    select( lambda, distribution, mse ) %>%
    ggplot( aes( lambda
                 , mse
                 , fill = distribution
                 , color = distribution ) )+
    geom_line() +
    geom_point(size = 1) +
    geom_vline( data = select(pl_min_plot, lambda_value, distribution, lambda_type)
                , mapping = aes(xintercept = lambda_value
                                , color = distribution
                                , linetype = lambda_type ) ) +
    scale_fill_manual( values = f_plot_adjust_col_vector_length(n_distributions) )+
    scale_color_manual( values = f_plot_adjust_col_vector_length(n_distributions) ) +
    theme( legend.position = 'bottom') +
    labs( x = 'log lambda')


  p_coef = pl_all %>%
    select(distribution, lambda, coeff) %>%
    unnest(coeff) %>%
    mutate( lambda = log(lambda)
            , coef = as.factor(coef)
            , distribution = as.factor(distribution) ) %>%
    ggplot( aes(lambda
                , value
                , fill = coef
                , color = coef ) ) +
    geom_point(size = 1) +
    geom_line( ) +
    geom_vline( data = pl_lambda_1se
                , mapping = aes(xintercept = lambda_value
                ) ) +
    geom_vline( data = pl_lambda_min
                , mapping = aes(xintercept = lambda_value )
                , linetype = 2 ) +
    facet_wrap(~distribution, scales = 'free', ncol = 1 ) +
    scale_fill_manual( values = f_plot_adjust_col_vector_length(n_coef) )+
    scale_color_manual( values = f_plot_adjust_col_vector_length(n_coef) ) +
    theme( legend.position = 'bottom') +
    labs( y = 'normalized coefficient values'
          ,x = 'log lambda')

  # correct performance label for classification

  if (family == 'binomial'){

    pl_fin = pl_fin %>%
      rename( auc = mse )

    p_mse = p_mse +
      labs( y = 'AUC' )

    if( max(pl_fin$auc) > 1 ){
      stop( 'maximum value of AUC > 1, possibly to few samples in test set
            , reduce k to reduce the number of cross validation sets' )
    }

  }


  # return --------------------------------------------------------------

  ret = list( plot_mse = p_mse
              , plot_coef = p_coef
              , tib_all = pl_fin
              , formula_str_lambda_1se = formula_str_lambda_1se
              , formula_str_lambda_min = formula_str_lambda_min
             )

  return(ret)
}


