
#'@import pipelearner

# randomForest--------------------------------------------------------


#' @title extract variable importance for randomForest model
#' @param m model of class randomForest
#' @return dataframe
#' @examples
#' #regression
#' m = twidlr::randomForest(mtcars, disp~.)
#' f_model_importance_randomForest(m)
#'
#' #classification
#' data_ls = f_clean_data(mtcars)
#' m = twidlr::randomForest(data_ls$data, cyl~.)
#' f_model_importance_randomForest(m)
#'
#' @rdname f_model_importance_randomForest
#' @export
f_model_importance_randomForest = function(m, ...){

  if( ! inherits(m, 'randomForest') ){
    warning('model is not of class randomForest returning NULL')
    return(NULL)
  }

  df = m$importance %>%

    f_manip_matrix_2_tibble()


  names(df) = c('row_names', 'value')

  df = df %>%
    arrange( desc(value) ) %>%
    mutate( rank = rank( desc(value) ) )

  return(df)

}

# svm---------------------------------------------------------------


#' @title extract variable importance for svm
#' @param m model of class svm
#' @param data original training dataframe
#' @return dataframe
#' @examples
#'
#'#regression
#' m = twidlr::svm(mtcars, disp~.)
#' f_model_importance_svm(m, mtcars)
#'
#' #classification
#' data = mtcars
#' data$cyl = factor(data$cyl, ordered = T)
#' m = twidlr::svm(data, cyl~.)
#' f_model_importance_svm(m, data)
#'
#'
#' @rdname f_model_importance_svm
#' @details uses 1D-SA 1 dimensional sensitivity analysis using
#'   rminer::Importance()
#' @export
#' @importFrom rminer Importance
f_model_importance_svm = function(m, data){

  if( ! inherits(m, 'svm') ){
    warning('model is not of class svm returning NULL')
    return(NULL)
  }

  # rminer does not like tibbles and resampling objects
  # as.data.frame(resample_obj) will return a tibble
  # thereofre we have to convert twice
  data = as.data.frame(data)
  data = as.data.frame(data)

  # terms to formula
  formula = m$terms %>%
    as.character()

  formula = paste0( formula[2], formula[1], formula[3] ) %>%
    as.formula()

  vars = f_manip_get_variables_from_formula(formula)
  response_var = f_manip_get_response_variable_from_formula(formula)
  response_var_sym = as.name(response_var)

  if( is.factor(data[[response_var]]) ){

    # rminer requires only numericals and does not like tibbles
    data = data %>%
      mutate_if( is.character, factor, ordered = T ) %>%
      mutate_all( as.numeric ) %>%
      mutate( !!response_var_sym := factor( !!response_var_sym, ordered = T ) )

    #classification
    m = rminer::fit( formula, data, model = 'svm' )

    imp = rminer::Importance( m, data )

  } else{
    #regression
    imp = rminer::Importance(m, data, PRED = twidlr::predict.svm )
  }

  df = tibble( row_names = names(data)
               , value   = imp$imp
               , rank    = rank( desc(value) )) %>%
    filter( row_names != response_var ) %>%
    arrange( desc(value) ) %>%
    head( length(vars) ) # make sure to return only variables that are in formula

  return(df)

}


# rpart--------------------------------------------------------------



#' @title extract variable importance for rpart
#' @param m model of class rpart
#' @return dataframe
#' @examples
#'
#'#regression
#' m = twidlr::rpart(mtcars, disp~.)
#' f_model_importance_rpart(m)
#'
#' #classification
#' data_ls = f_clean_data(mtcars)
#' m = twidlr::rpart(data_ls$data, cyl~.)
#' f_model_importance_rpart(m)
#'
#' @rdname f_model_importance_rpart
#' @export
f_model_importance_rpart = function(m, ...){

  if( ! inherits(m, 'rpart') ){
    warning('model is not of class rpart returning NULL')
    return(NULL)
  }

  df = tibble( row_names = names(m$variable.importance)
               , value   = m$variable.importance
               )

  # add variables with zero importance

  vars = m$terms %>%
    as.character() %>%
    .[3] %>%
    stringr::str_split(' \\+ ') %>%
    unlist() %>%
    .[ ! . %in% df[['row_names']] ]

  vars = tibble( row_names = vars
                 , value   = rep(0, length(vars) )
                )

  df = df %>%
    bind_rows( vars ) %>%
    arrange( desc(value) ) %>%
    mutate( rank = rank( desc(value) ) )

  return(df)

}

#' @title model importance
#' @description supports rpart, randomForest, svm, will return NULL for other models
#' @param m model
#' @param data training data
#' @return tibble
#' @examples
#' pl = pipelearner::pipelearner(mtcars) %>%
#'   pipelearner::learn_models( twidlr::rpart, disp~. ) %>%
#'   pipelearner::learn_models( twidlr::randomForest, disp~. ) %>%
#'   pipelearner::learn_models( twidlr::svm, disp~. ) %>%
#'   pipelearner::learn() %>%
#'   mutate( imp = map2(fit, train, f_model_importance) )
#'pl$imp
#' @rdname f_model_importance
#' @export

f_model_importance = function(m, data){

  if( inherits(m, 'rpart') ){
    return( f_model_importance_rpart(m) )
  }

  if( inherits(m, 'randomForest') ){
    return( f_model_importance_randomForest(m) )
  }

  if( inherits(m, 'svm') ){

    return(f_model_importance_svm(m, data))
  }

  return(NULL)
}

#' @title plot model importance
#' @description optimised for usage in pipelearner dataframe
#' @param importance dataframe importance created by f_model_importance()
#' @param model_name character vector (model column in pipelearner dataframe)
#'   will be pasted for plot title
#' @param models.id character vector will be pasted for plot title
#' @param cv_pairs.id character vector will be pasted for plot title
#' @param train_p character vector will be pasted for plot title
#' @param variable_color_code dataframe created by f_plot_color_code_variables()
#' @param ... additional character vectors to be pasted to plot title
#' @return plotly graph
#' @examples
#'
#' data_ls = f_clean_data(mtcars)
#' variable_color_code = f_plot_color_code_variables(data_ls)
#' m = twidlr::rpart(mtcars, disp~.)
#' imp = f_model_importance_rpart(m)
#' f_model_importance_plot(imp
#'                         , title = 'rpart'
#'                         , variable_color_code = variable_color_code
#'                         )
#' #pipelearner
#' pl = pipelearner::pipelearner(data_ls$data) %>%
#'   pipelearner::learn_models( twidlr::rpart, disp~. ) %>%
#'   pipelearner::learn_models( twidlr::randomForest, disp~. ) %>%
#'   pipelearner::learn_models( twidlr::svm, disp~. ) %>%
#'   pipelearner::learn() %>%
#'   mutate( imp = map2(fit, train, f_model_importance)
#'          , title = paste( model, models.id, cv_pairs.id, train_p )
#'          , plot = map2( imp
#'                        , title
#'                        , f_model_importance_plot
#'                        , variable_color_code = variable_color_code
#'                        )
#'          )
#' htmltools::tagList(pl$plot)
#' @rdname f_model_importance_plot
#' @export
#' @importFrom forcats fct_reorder
#' @importFrom plotly ggplotly
f_model_importance_plot = function( importance
                                    , title
                                    , variable_color_code = NULL
                                    ){

  importance = importance %>%
    mutate( row_names = forcats::fct_reorder(row_names, value) )%>%
    rename( variables = row_names)


  if( ! is.null(variable_color_code) ){

    importance = importance %>%
      left_join( variable_color_code  ) %>%
      mutate( variables = forcats::fct_reorder(variables, value) )%>%
      arrange( variables )

    col_vector = importance[['color']]

  } else{
    col_vector = f_plot_col_vector74()
  }

  p = ggplot(importance, aes( x = variables
                              , y = value
                              , fill = variables ) ) +
    geom_col( show.legend = F ) +
    coord_flip() +
    labs( title = title
          , x = 'variable'
          , y = 'importance') +
    theme( legend.position = 'none') +
    scale_fill_manual( values = col_vector)


  plotly::ggplotly(p)

}

#' @title tableplot of important variables
#' @description takes the most important variables of a model and plots a tabplot::tableplot
#' @param data dataframe
#' @param ranked_variables datafram as returned by f_model_importance()
#' @param response_var character vector denoting response variable
#' @param limit integer limit the number of variables , Default: 10
#' @param ... pass kwargs to tabplot::tableplot
#' @return tabplot::tableplot object
#' @examples
#'
#' data = f_clean_data(mtcars) %>%
#'   .$data
#' m = rpart::rpart( disp~., data)
#' ranked_variables  = f_model_importance(m, data)
#' response_var = 'disp'
#'
#' f_model_importance_plot_tableplot( data, ranked_variables, response_var, limit = 5 )
#'
#'#pipe
#'form = as.formula('disp~cyl+mpg+hp')
#'pl = pipelearner::pipelearner(mtcars) %>%
#'  pipelearner::learn_models( twidlr::rpart, form ) %>%
#'  pipelearner::learn_models( twidlr::randomForest, form ) %>%
#'  pipelearner::learn_models( twidlr::svm, form ) %>%
#'  pipelearner::learn() %>%
#'  mutate( imp = map2(fit, train, f_model_importance)
#'          , tabplot = pmap( list( data = train
#'                                  , ranked_variables = imp
#'                                  , response_var = target
#'                                  , title = model
#'          )
#'          , f_model_importance_plot_tableplot
#'          , limit = 5
#'          )
#'   )
#' @seealso
#'  \code{\link[tabplot]{tableplot}}
#' @rdname f_model_importance_plot_tableplot
#' @export
#' @importFrom tabplot tableplot
f_model_importance_plot_tableplot = function( data
                                              , ranked_variables
                                              , response_var
                                              , limit = 10
                                              , print = F
                                              ,  ... ){

  vars = ranked_variables %>%
    arrange_('rank') %>%
    head(limit) %>%
    .[['row_names']]

  p = as.data.frame(data) %>%
    select( one_of( response_var, vars) ) %>%
    tabplot::tableplot( plot = F, ...)

  if(print == T){
    tabplot:::plot.tabplot(p)
  }

  return(p)

}

#' @title add plots based on variable importance to pipelearner dataframe
#' @description adds a bar plot of the ranked variables, a tabplot sorted by the
#'   target variable and a dependency plot (response variable vs the sequential
#'   range of one of the predictor variables while all other predictors are kept
#'   constant at mean values).
#' @param pl a dataframe containing the columns for data, m, ranked_variables, response_var and title
#' @param data symbol (unquoted name) of data column in pl
#' @param m symbol (unquoted name) of data column in pl
#' @param ranked_variables symbol (unquoted name) of data column in pl
#' @param response_var symbol (unquoted name) of data column in pl
#' @param title symbol (unquoted name) of data column in pl
#' @param variable_color_code dataframe created by f_plot_color_code_variables()
#' @param formula fomula that was used to construct model
#' @param data_ls data_ls list object containing the whole of the original data
#' @param var_dep_limit number of variables to be plotted on dependency plot
#' @param var_dep_log_y should y axis of dependency plot be logarithmic
#' @param tabplot_limit number of variables to be plotted on tabplot
#' @return dataframe
#' @examples
#'
#' data_ls = f_clean_data(mtcars)
#' form = disp~cyl+mpg+hp
#' variable_color_code = f_plot_color_code_variables(data_ls)
#'
#' pl = pipelearner::pipelearner(data_ls$data) %>%
#'   pipelearner::learn_models( twidlr::rpart, form ) %>%
#'   pipelearner::learn_models( twidlr::randomForest, form ) %>%
#'   pipelearner::learn_models( twidlr::svm, form ) %>%
#'   pipelearner::learn() %>%
#'   mutate( imp = map2(fit, train, f_model_importance)
#'           , title = paste(model, models.id, train_p) ) %>%
#'   f_model_importance_pl_add_plots_regression(  data                  = train
#'                                                , m                   = fit
#'                                                , ranked_variables    = imp
#'                                                , title               = title
#'                                                , response_var        = target
#'                                                , variable_color_code = variable_color_code
#'                                                , formula             = form
#'                                                , data_ls             = data_ls
#'                                                , var_dep_limit       = 10
#'                                                , var_dep_log_y       = T
#'                                                , tabplot_limit       = 12 )
#'
#' @rdname f_model_importance_pl_add_plots_regression
#' @seealso \code{\link{f_model_importance_plot}}
#' \code{\link{f_model_importance_plot_tableplot}}
#' \code{\link{f_model_plot_variable_dependency_regression}}
#' @export
f_model_importance_pl_add_plots_regression = function( pl
                                                     , data
                                                     , m
                                                     , ranked_variables
                                                     , response_var
                                                     , title
                                                     , variable_color_code = f_plot_color_code_variables(data_ls)
                                                     , formula
                                                     , data_ls
                                                     , var_dep_limit = 10
                                                     , var_dep_log_y = F
                                                     , tabplot_limit = 12
                                                     ){

  data_enquo             = enquo(data)
  ranked_variables_enquo = enquo(ranked_variables)
  response_var_enquo     = enquo(response_var)
  title_enquo            = enquo(title)
  m_enquo                = enquo(m)

  # tabplot --------------------------------------------------------------
  pl = pl %>%
    mutate( imp_tabplot = pmap( list( data          = !! data_enquo
                                 , ranked_variables = !! ranked_variables_enquo
                                 , response_var     = !! response_var_enquo
                                 , title            = !! title_enquo
                                 )
                           , f_model_importance_plot_tableplot
                           , limit = tabplot_limit
    )
           )

  # plots---------------------------------------------------------------

  pl = pl %>%
    mutate( imp_plot = pmap( list(  importance = !! ranked_variables_enquo
                                 , title       = !! title_enquo
                                )
                           , f_model_importance_plot
                           , variable_color_code = variable_color_code
                          )
          )

  # variable dependency-------------------------------------------------

  pl = pl %>%
    mutate( imp_plot_dep = pmap( list(  m         = !! m_enquo
                               , ranked_variables = !! ranked_variables_enquo
                               , title            = !! title_enquo
                               , data             = !! data_enquo
                               )
                        , f_model_plot_variable_dependency_regression
                        , variable_color_code = variable_color_code
                        , formula             = formula
                        , data_ls             = data_ls
                        , limit               = var_dep_limit
                        , log_y               = var_dep_log_y
                      )
          )

}


#' @title print plots of variable importance in modelling dataframe to html
#' @description should execute f_model_importance_pl_add_plots_regression() on
#'   modelling dataframe first
#' @param pl modelling dataframe containing the following columns 'imp_plot', 'imp_plot_dep', 'imp_tabplot', 'title'
#' @param prefix character vector file name prefix for html files, Default: NULL
#' @return html files in working directory
#' @examples
#' \dontrun{
#'     data_ls = f_clean_data(mtcars)
#'form = disp~cyl+mpg+hp
#'variable_color_code = f_plot_color_code_variables(data_ls)
#'
#'pl = pipelearner::pipelearner(data_ls$data) %>%
#'  pipelearner::learn_models( twidlr::rpart, form ) %>%
#'  pipelearner::learn_models( twidlr::randomForest, form ) %>%
#'  pipelearner::learn_models( twidlr::svm, form ) %>%
#'  pipelearner::learn() %>%
#'  mutate( imp = map2(fit, train, f_model_importance)
#'          , title = paste(model, models.id, train_p) ) %>%
#'  f_model_importance_pl_add_plots_regression(  data                  = train
#'                                               , m                   = fit
#'                                               , ranked_variables    = imp
#'                                               , title               = title
#'                                               , response_var        = target
#'                                               , variable_color_code = variable_color_code
#'                                               , formula             = form
#'                                               , data_ls             = data_ls
#'                                               , var_dep_limit       = 10
#'                                               , var_dep_log_y       = T
#'                                               , tabplot_limit       = 12) %>%
#'  f_model_importance_pl_plots_as_html( prefix = 'test_oetteR_html_')
#'
#'files = dir() %>%
#'  .[ startsWith(., 'test_oetteR_html_') ]
#'
#'file.remove( files )
#'
#'
#' }
#' @seealso \code{\link[htmltools]{tagList}}
#' @rdname f_model_importance_pl_plots_as_html
#' @export
#' @importFrom htmltools tagList
f_model_importance_pl_plots_as_html = function(pl, prefix = NULL){

  bool = c('imp_plot', 'imp_plot_dep', 'imp_tabplot', 'title') %in% names(pl)

  if( ! all(bool)  ){
    stop(" one of the following columns was not found in pl: 'imp_plot', 'imp_plot_dep', 'imp_tabplot', 'title' ")
  }

  taglist = pl$imp_plot %>%
    htmltools::tagList()

  f_plot_obj_2_html( taglist
                     , type = 'taglist'
                     , output_file = paste0(prefix,'importance_plots')
                     , title = 'Variable Importance'
                     )

  f_plot_obj_2_html( pl$imp_plot_dep
                     , type = 'plots'
                     , output_file = paste0(prefix,'importance_variable_dependencies')
                     , title = 'Variable dependecies'
                    )

  f_plot_obj_2_html( pl$imp_tabplot
                     , type = 'tabplots'
                     , output_file = paste0(prefix,'importance_tabplots')
                     , titles = pl$title
                     , title = 'Tabplots'
                   )

}
