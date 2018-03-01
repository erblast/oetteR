


#' @title adds predictions to learned pipelearner dataframe
#' @param pl learned pipelearner dataframe
#' @param cols_id character vector naming id columns
#' @return dataframe
#' @examples
#' form = as.formula( 'disp~cyl+mpg')
#'
#' pl = mtcars %>%
#'   mutate(names = row.names(.)) %>%
#'   pipelearner::pipelearner() %>%
#'   pipelearner::learn_models( twidlr::rpart, form ) %>%
#'   pipelearner::learn_models( twidlr::randomForest, form ) %>%
#'   pipelearner::learn_models( twidlr::svm, form ) %>%
#'   pipelearner::learn_models( gamlss::gamlss, form ) %>%
#'   pipelearner::learn() %>%
#'   f_predict_pl_regression( cols_id = 'names' )
#' @rdname f_predict_pl_regression
#' @export
#' @seealso
#' \code{\link{f_predict_regression_add_predictions}}
f_predict_pl_regression = function( pl
                                    , cols_id = NULL
                                    , formula = NULL
                                    , col_model = 'fit'
                                    , col_target = 'target'
                                    , data_test = 'test'
                                    , data_train = 'train'){


  sym_data_test = as.name(data_test)
  sym_data_train = as.name(data_train)
  sym_target = as.name(col_target)
  sym_model = as.name(col_model)

  if( ! is.null(formula) ){
    pl = pl %>%
      mutate( preds = pmap( list( data_test = !! sym_data_test
                                  , m = !! sym_model
                                  , col_target = !! sym_target
                                  , data_train = !! sym_data_train )
                            , f_predict_regression_add_predictions
                            , cols_id
                            , formula ) )

  }else{

    if( ! 'formula' %in% names(pl) ){
      pl = pl %>%
        mutate( formula = map(params, 'formula') )
    }

    sym_formula = as.name('formula')

    pl = pl %>%
      mutate( preds = pmap( list( data_test = !! sym_data_test
                                  , m = !! sym_model
                                  , col_target = !! sym_target
                                  , data_train = !! sym_data_train
                                  , formula = !! sym_formula )
                            , f_predict_regression_add_predictions
                            , cols_id
                            )
              )
  }

  return(pl)

}


#' @title summarize prediction by f_predict_pl_regression()
#' @description use this function to get a quick summary of pipelearner
#'   dataframe with unnested predictions. Will group by title
#' @param pl pipelearner dataframe with nested predictions
#' @return dataframe with mape, mea, rtmse and median versions
#' @rdname f_predict_pl_regression_summarize
#' @seealso \code{\link{f_predict_pl_regression}}
#' @examples
#' form = as.formula( 'disp~cyl+mpg')
#'
#' pl = mtcars %>%
#'   mutate(names = row.names(.)) %>%
#'   pipelearner::pipelearner() %>%
#'   pipelearner::learn_models( twidlr::rpart, form ) %>%
#'   pipelearner::learn_models( twidlr::randomForest, form ) %>%
#'   pipelearner::learn_models( twidlr::svm, form ) %>%
#'   pipelearner::learn() %>%
#'   f_predict_pl_regression( 'names' ) %>%
#'   unnest( preds , .drop = FALSE ) %>%
#'   mutate( title = model ) %>%
#'   f_predict_pl_regression_summarize()
#'
#' pl
#'
#' @export
f_predict_pl_regression_summarize = function( pl ){

  if( ! all( c('title'
               , 'target1'
               , 'resid'
               , 'resid_abs'
               , 'resid_squ'
               , 'ape'
  )
  %in% names(pl) )
  ){
    stop('unnest predictions and add title first')
  }


  pl = pl %>%
    group_by( title )  %>%
    summarize( mean_target  = mean(target1)
               , mean_pred  = mean(pred)
               , mae        = mean(resid_abs)
               , rtmse      = sqrt( mean(resid_squ) )
               , mape       = mean(ape)
               , med_target = median(target1)
               , med_pred   = median(pred)
               , med_ae     = median(resid_abs)
               , rt_med_mse = sqrt( median(resid_squ) )
               , med_ape    = median(ape)
               )
  return(pl)

}

#' @title plot model performance
#' @description add predictions to modelling dataframe and unnest, create a title column and a bins column
#' @param data dataframe with the columns title, bins, resid_abs, resid_squ, ape
#' @return taglist \describe{
#'   \item{[1]}{Headline summary Plots }
#'   \item{[2]}{Residuals Pointplot}
#'   \item{[3]}{Residuals Boxplot}
#'   \item{[4]}{APE Pointplot}
#'   \item{[5]}{APE Boxplot}
#'   \item{[6]}{MAPE, MSE, MAE, Binning }
#'   \item{[7]}{Headline Performance Measures Summary}
#'   \item{[8]}{Summary MAPE, MSE, MAE with SE }
#'   \item{[9]}{Summary MAPE, MSE, MAE with CI95 }
#'   \item{[10]}{Headline Summary Tables}
#'   \item{[11]}{Summary Table}
#'   \item{[12]}{Table Binning}
#' }
#' @examples
#' \dontrun{
#'form = as.formula( 'displacement~cylinders+mpg')
#'
#'ISLR::Auto %>%
#'  pipelearner::pipelearner() %>%
#'  pipelearner::learn_models( twidlr::rpart, form ) %>%
#'  pipelearner::learn_models( twidlr::randomForest, form ) %>%
#'  pipelearner::learn_models( twidlr::svm, form ) %>%
#'  pipelearner::learn() %>%
#'  f_predict_pl_regression( 'name' ) %>%
#'  unnest(preds) %>%
#'  mutate( bins = cut(target1, breaks = 3 , dig.lab = 4)
#'          , title = paste(models.id, cv_pairs.id, train_p, target, model) ) %>%
#'  f_predict_plot_model_performance_regression() %>%
#'  f_plot_obj_2_html(type = 'taglist', 'test_me', title = 'Model Performance')
#'
#' file.remove('test_me.html')
#' }
#' @seealso
#'  \code{\link[htmltools]{tagList}}
#'  \code{\link[plotly]{ggplotly}}
#'  \code{\link[DT]{datatable}}
#'  \code{\link{f_predict_pl_regression} }
#' @rdname f_predict_plot_model_performance_regression
#' @export
#' @importFrom htmltools tagList
#' @importFrom plotly ggplotly
#' @importFrom DT datatable
f_predict_plot_model_performance_regression = function(data){


  if( ! all( c('title'
               , 'bins'
               , 'resid'
               , 'resid_abs'
               , 'resid_squ'
               , 'ape'
  )
  %in% names(data) )
  ){
    stop('unnest predictions and add bins and title first')
  }

  nrows = data %>%
    group_by( as.character(title) ) %>%
    count() %>%
    nrow()

  nrows = ceiling( nrows/2 )

  taglist = htmltools::tagList()


  # Residuals-------------------------------------------------------------------------

  p = f_plot_pretty_points(data
                           , 'target1'
                           , 'resid'
                           , col_facet = 'title'
                           , y_title = 'Residuals'
                           , x_title = 'Target Variable'
                           , ncol = 2
                           , size = 2) +
    geom_hline( yintercept = 0, size = 1, alpha = 0.5)

  p = plotly::ggplotly(p,  height = nrows * 200 , dynamicTicks = TRUE
                       , tooltip = c('x','y') )

  taglist[[1]] = f_html_padding(htmltools::h2('Summary Plots'), pad_before = 3)

  taglist[[2]] = f_html_padding(p, pad_before = 3, title = 'Residuals Scatterplot' )

  p = ggplot( data, aes(bins, resid)) +
    geom_boxplot( aes(fill=title)
                  , show.legend = F ) +
    facet_wrap(~title, ncol = 2 ) +
    geom_hline( yintercept = 0, size = 1, alpha = 0.5) +
    theme( axis.text.x = element_text(angle = 90)
           , legend.position = 'none')+
    scale_fill_manual( values =  f_plot_col_vector74() ) +
    labs(y = 'Residuals', x = 'Target Variable')


  p = plotly::ggplotly(p,  height = nrows * 200 )
  taglist[[3]] = f_html_padding(p, pad_before = 3, title = 'Residuals Boxplot')

  # APE-------------------------------------------------------------------------

  p = f_plot_pretty_points(data
                           , 'target1'
                           , 'ape'
                           , col_facet = 'title'
                           , y_title = 'APE of Predictions'
                           , x_title = 'Target Variable'
                           , ncol = 2
                           , size = 2) +
    coord_cartesian( ylim = c(500,0) )

  p = plotly::ggplotly(p,  height = nrows * 200)
  taglist[[4]] =  f_html_padding(p, pad_before = 3, title = 'APE Scatterplot')

  p = ggplot( data, aes(bins, ape)) +
    geom_boxplot( aes(fill=title) ) +
    facet_wrap(~title, ncol = 2 )+
    theme( axis.text.x = element_text(angle = 90)
           , legend.position = 'none') +
    scale_fill_manual(values = f_plot_col_vector74()) +
    labs(y = 'APE of Predictions', x = 'Target Variable') +
    coord_cartesian( ylim = c(200,0) )

  p = plotly::ggplotly(p,  height = nrows * 200, tooltip = c('x','y') )
  taglist[[5]] = f_html_padding(p, pad_before = 3, title = 'APE Boxplot')


  # Bins-------------------------------------------------------------------------

  data_sum1 = data %>%
    group_by( title, bins )  %>%
    rename( MAPE = ape, MAE = resid_abs, MSE = resid_squ) %>%
    gather( key = 'measure', value = 'value', MAE, MAPE, MSE) %>%
    group_by( title, bins, measure) %>%
    summarize( me = mean(value)
               , ci95 = qnorm(0.95, mean = mean(value), sd = sd(value) )
               , sem = sd(value) / sqrt( n() )
    ) %>%
    ungroup() %>%
    mutate( seq = as.integer(bins) )

  p = ggplot(data_sum1, aes( x = seq, y = me, color = title) ) +
    geom_pointrange( aes( ymin = me - sem, ymax = me + sem)  ) +
    geom_line() +
    facet_wrap(~measure, scales = 'free', ncol = 1) +
    scale_x_continuous( breaks = c(1:length( levels(data_sum1$bins) ) )
                        , labels = levels(data_sum1$bins) ) +
    # theme(axis.text.x = element_text(angle = 90)
    #       , legend.position = 'bottom') +
    labs( y = 'mean + SEM', x = 'Target Variable', color = '') +
    scale_color_manual(values = f_plot_col_vector74() )

  p = plotly::ggplotly( p, height = 600 )
  taglist[[6]] = f_html_padding(p, pad_before = 3, title = 'Performance Measures Binning')

  # Summary Plots-------------------------------------------------------------------------

  data_sum2 = data %>%
    group_by( title )  %>%
    rename( MAPE = ape, MAE = resid_abs, MSE = resid_squ) %>%
    gather( key = 'measure', value = 'value', MAE, MAPE, MSE) %>%
    group_by( title, measure) %>%
    summarize( me = mean(value)
               , ci95 = qnorm(0.95, mean = mean(value), sd = sd(value) )
               , sem = sd(value) / sqrt( n() )
    )

  p = ggplot(data_sum2, aes( x = title, y = me, color = title) ) +
    geom_pointrange( aes( ymin = me - sem, ymax = me + sem)  ) +
    facet_wrap(~measure, scales = 'free', ncol = 1) +
    theme(axis.text.x = element_blank()
          , axis.ticks.x = element_blank()
          , legend.position = 'bottom') +
    labs( y = 'mean + SEM', x = '', color = '') +
    scale_color_manual(values = f_plot_col_vector74() )


  taglist[[7]] = f_html_padding(htmltools::h3('Performance Measures Summary'), pad_before = 3)

  p = plotly::ggplotly( p, height = 600 )
  taglist[[8]] = f_html_padding(p, pad_before = 3, title = 'SEM')

  p = ggplot(data_sum2, aes( x = title, y = me, color = title) ) +
    geom_pointrange( aes( ymin = me - ci95, ymax = me + ci95)  ) +
    facet_wrap(~measure, scales = 'free', ncol = 1) +
    theme(axis.text.x = element_blank()
          , axis.ticks.x = element_blank()
          , legend.position = 'bottom') +
    labs( y = 'mean + CI95', x = '', color = '') +
    scale_color_manual(values = f_plot_col_vector74() )

  p = plotly::ggplotly(p, height = 600)
  taglist[[9]] = f_html_padding(p, pad_before = 3, title = 'CI95')

  # Summary Tables-------------------------------------------------------------------------

  taglist[[10]] = f_html_padding(htmltools::h2('Summary Tables'), pad_before = 3)

  t = f_datatable_universal(data_sum2, round_other_nums = 2)
  taglist[[11]] = f_html_padding(t, pad_before = 3, title = 'Performance Measures Summary')

  t = f_datatable_universal( data_sum1, round_other_nums = 2 )
  taglist[[12]] = f_html_padding(t, pad_before = 3, title = 'Performance Measures Binning')

  return(taglist)

}

#' @title adds predictions, residuals, abolute residuals, squared residuals and
#'   absolute percent error to a dataframe.
#' @description absolute percent error = (abs(resid/pred)*100 )
#' @param df dataframe containing data to be used as the basis for prediction.
#'   Can also be a modelR resample object
#' @param m regression model
#' @param col_target character vector naming target/response variable
#' @param cols_id character vector naming id columns, if specified non_id
#'   columns will be dropped from dataframe, in order to be more memory
#'   efficient.
#' @return dataframe
#' @examples
#' df = mtcars %>%
#' mutate(names = row.names(.))
#' m = rpart::rpart(disp~., df)
#' pred = f_predict_regression_add_predictions(df, m, 'disp', 'names')
#' pred
#' @details works with HDtweedie, randomForest, rpart, e1071::svm, glmnet, gamlss
#' @rdname f_predict_regression_add_predictions
#' @export
#' @importFrom modelr add_predictions add_residuals
f_predict_regression_add_predictions = function(data_test
                                                , m
                                                , col_target
                                                , data_train = NULL
                                                , cols_id = NULL
                                                , formula = NULL
                                                , ...){

  col_target_sym = as.name(col_target)

  data_test = as.tibble(data_test)
  data_train = as.tibble(data_train)

  if( inherits(m, what = 'HDtweedie') |
      inherits(m, what = 'glmnet') |
      inherits(m, what = 'cv.HDtweedie') |
      inherits(m, what = 'cv.glmnet')
      ){

    if( is.null(formula) ){
      stop('need formula to make predictions for glmnet/HDtweedie model')
    }

    x = select( data_test, f_manip_get_variables_from_formula(formula) ) %>%
      as.matrix

    if( inherits(m, what = 'HDtweedie') ){
        pred = HDtweedie::predict.HDtweedie( m, newx = x, ... )
    }
    if( inherits(m, what = 'cv.HDtweedie') ){
      pred = HDtweedie:::predict.cv.HDtweedie( m, newx = x, ... )
    }

    if( inherits(m, what = 'glmnet') ){
      pred = glmnet:::predict.glmnet( m, newx = x, type = 'response', ... )
    }
    if( inherits(m, what = 'cv.glmnet') ){
      pred = glmnet:::predict.cv.glmnet( m, newx = x, type = 'response', ... )
    }

    df = data_test %>%
      mutate( pred = pred
             , resid = (!! col_target_sym) - pred)

  } else if (inherits(m, what = 'gamlss') ) {
    if( is_empty(data_train) ){
      stop('need training dataframe to make predictions for gamlss model')
    }
      pred = gamlss:::predict.gamlss(m, data = data_train, newdata = data_test )
      df = data_test %>%
        mutate( pred = pred
                , resid = (!! col_target_sym) - pred)

  }else{

  df = data_test %>%
    modelr::add_predictions(m) %>%
    modelr::add_residuals(m)
  }

  df = df %>%
  mutate( target = !!col_target_sym ) %>%
  select( one_of( c( cols_id, 'target', 'pred', 'resid') ) ) %>%
  mutate( resid_abs   = abs(resid)
            , resid_squ = resid^2
            , ape       = abs(resid/pred) * 100
            , ape       = ifelse( is.na(ape), 0, ape )
  )

  return(df)
}


#' @title Plot distribution of model predictions vs observed
#' @description takes a dataframe with predictions and a title column and
#'   returns a list with one violin and one histogram plot to compare
#'   distributions.
#' @param data dataframE
#' @param col_title character vector denoting title column, Default: 'title'
#' @param col_pred character vector denoting column with predictions, Default:
#'   'preds'
#' @param col_obs character vecor denoting column with observed values, Default:
#'   'target1'
#' @param bins number of bins used for histograms, Default: 60
#' @param ... additional arguments passed to the facet_wrap function of the
#'   histogramss
#' @return list with two plots
#' @examples
#'
#'   form = as.formula( 'displacement~cylinders+mpg')
#'
#'   df = ISLR::Auto %>%
#'     pipelearner::pipelearner() %>%
#'     pipelearner::learn_models( rpart::rpart, form ) %>%
#'     pipelearner::learn_models( randomForest::randomForest, form ) %>%
#'     pipelearner::learn_models( e1071::svm, form ) %>%
#'     pipelearner::learn() %>%
#'     f_predict_pl_regression( 'name' ) %>%
#'     unnest(preds)
#'
#'   f_predict_plot_regression_distribution(df
#'                                          , col_title = 'model'
#'                                          , col_pred = 'pred'
#'                                          , col_obs = 'target1')
#'
#' @seealso \code{\link[rlang]{UQ}} \code{\link[RColorBrewer]{brewer.pal}}
#' @rdname f_predict_plot_regression_distribution
#' @export
#' @importFrom rlang UQ
#' @importFrom RColorBrewer brewer.pal
f_predict_plot_regression_distribution = function( data
                                                  , col_title = 'title'
                                                  , col_pred = 'pred'
                                                  , col_obs = 'target1'
                                                  , bins = 60
                                                  , ... ){


  if( ! all( c( col_title, col_pred, col_obs) %in% names(data) ) ){
    stop('col_title, col_pred or col_obs not found')
  }

  dist_pred = data %>%
    select( one_of( col_title, col_pred ) )

  titles = unique( dist_pred[[col_title]] )

  dist_obs = data %>%
    filter( rlang::UQ( as.name(col_title) ) == "randomForest" ) %>%
    select( one_of( col_title, col_obs ) ) %>%
    mutate( !! as.name(col_pred) := !! as.name(col_obs)
            , !! as.name(col_title) := 'observed' ) %>%
    select( one_of( col_title, col_pred ) )

  suppressWarnings({

    dist = dist_pred %>%
      bind_rows( dist_obs ) %>%
      mutate( !! as.name(col_title) := as.factor( !! as.name(col_title) ) )

  })

  col_vec =  f_plot_adjust_col_vector_length( n = length(titles) + 1
                                             ,  c( RColorBrewer::brewer.pal(n = 8,name = 'Dark2')
                                                , f_plot_col_vector74() ) )

  p_box = ggplot( dist, aes_string( col_title, col_pred, fill = col_title ) ) +
    geom_violin() +
    geom_boxplot( alpha = 0.4, fill = 'white', size = 1 )   +
    coord_flip() +
    theme( legend.position = 'none') +
    scale_fill_manual( values = col_vec ) +
    labs( y = '', x = '')


  p_hist = ggplot( dist, aes_string( col_pred, fill = col_title ) ) +
    geom_histogram( bins = bins ) +
    facet_wrap( as.formula( paste( '~', col_title) ) , scales = 'free', ...)  +
    scale_fill_manual( values = col_vec ) +
    scale_color_manual( values = col_vec ) +
    theme( legend.position = 'none') +
    labs( x = '' ) +
    geom_rug( aes_string(color = col_title ) )

  return( list(p_box = p_box, p_hist = p_hist) )

}

#' @title plot residuals of different models as aaluvials
#' @description Residuals will be  binned using boxplotstats with the
#'   modification, that the median will be set to zero. If quantile boundaries
#'   do not fall into sensible ranges they will be replaced by the standard
#'   error (SE). For example if we have the following boxplotstats -2, 1, 3, 5,
#'   8 with an SE of 0.5 we will modify the boundaries as following. -2, -0.5,
#'   0, 3, 5 . The reason is that we want to be able to follow observations with
#'   positive or negative residuals through the alluvial plot in order to judge
#'   emerging patterns. The models will be sorted by MSE and the alluvial plot
#'   will be flipped with the model with the lowest MSE on top.
#' @param data dataframe
#' @param col_id character vecotr dentoing id column
#' @param col_title character vector denoting model title column, Default:
#'   'title'
#' @param col_pred character vector denoting prediciont column, Default: 'pred'
#' @param col_obs character vector denoting column with observed values,
#'   Default: 'target1'
#' @param ... additional arguments passed to f_plot_alluvial_1v1
#' @return plot
#' @examples
#'
#'   form = as.formula( 'displacement~cylinders+mpg')
#'
#' df = ISLR::Auto %>%
#'   mutate( name = paste( name, row_number() ) ) %>%
#'   pipelearner::pipelearner() %>%
#'   pipelearner::learn_models( rpart::rpart, form ) %>%
#'   pipelearner::learn_models( randomForest::randomForest, form ) %>%
#'   pipelearner::learn_models( e1071::svm, form ) %>%
#'   pipelearner::learn() %>%
#'   f_predict_pl_regression( 'name' ) %>%
#'   unnest(preds)
#'
#' f_predict_plot_regression_alluvials(df
#'                                     , col_id = 'name'
#'                                     , col_title = 'model'
#'                                     , col_pred = 'pred'
#'                                     , col_obs = 'target1')
#'
#' @seealso \code{\link[RColorBrewer]{brewer.pal}}
#' @rdname f_predict_plot_regression_alluvials
#' @export
#' @importFrom RColorBrewer brewer.pal
f_predict_plot_regression_alluvials = function( data
                                                , col_id
                                                , col_title = 'title'
                                                , col_pred = 'pred'
                                                , col_obs = 'target1'
                                                , ... ){

  data$resid = data[[ col_pred ]] - data[[ col_obs ]]

  order_models = data %>%
    group_by( !! as.name( col_title) ) %>%
    summarise( rMSE = mean(resid^2) )  %>%
    arrange( desc(rMSE) ) %>%
    .[[ col_title ]]


  SEM = sd( data$resid )/ sqrt( nrow(data) )

  avg = mean(data$resid)

  med = median(data$resid)

  stats = boxplot.stats(data$resid)$stats

  #set median to zero

  stats[3] = 0

  #adjust other stat boundaries to new median

  if( stats[2] >= 0 ) stats[2] = 0 - SEM
  if( stats[1] >= stats[2] ) stats[1] = stats[2] - SEM

  if( stats[4] <= 0 ) stats[4] = 0 + SEM
  if( stats[5] <= stats[4] ) stats[5] = stats[4] + SEM


  min_val = ifelse( stats[[1]] <= min(data$resid) - 1, stats[[1]], min(data$resid) - 1 )
  max_val = ifelse( stats[[5]] >= max(data$resid) + 1, stats[[5]], max(data$resid) + 1 )

  breaks = c( min_val, stats, max_val )
  breaks = unique( breaks )

  data$bin = cut( data$resid, breaks = breaks )

  # cut will always return 6 levels even if some of them are empty

  level_names = c('<<<', '<<', '< 0', '> 0', '>>', '>>>')

  levels(data$bin) = level_names

  #check annotation if outlier levels are missing

  col_vec =  RColorBrewer::brewer.pal(n = 6,name = 'Dark2')

  suppressWarnings({

    p = f_plot_alluvial_1v1( data
                         , col_x = col_title
                         , col_y = 'bin'
                         , col_id = col_id
                         , col_vector_flow = col_vec
                         , fill_by = 'last_variable'
                         , order_levels_x = order_models
                         , ...  )  +
      theme( axis.text.x = element_text( angle = 0) ) +
      coord_flip()

  })

  return(p)
}
