


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
#'   pipelearner::learn() %>%
#'   f_predict_pl_regression( 'names' ) %>%
#'   f_predict_pl_regression_summarize()
#' @rdname f_predict_pl_regression
#' @export
#' @seealso
#' \code{\link{f_predict_regression_add_predictions}}
f_predict_pl_regression = function( pl, cols_id){

  if( ! all( c('models.id'
               , 'cv_pairs.id'
               , 'train_p'
               , 'target'
               , 'model')
             %in% names(pl) )
      ){
    stop('need learned pipelearner dataframe as input pl')
  }

  pl = pl %>%
    mutate( preds = pmap( list( test, fit, target)
                          , f_predict_regression_add_predictions
                          , cols_id) )
}


#' @title summarize prediction by f_predict_pl_regression()
#' @description use this function to get a quick summary of pipelearner dataframe with unnested predictions
#' @param pl pipelearner dataframe with nested predictions
#' @return dataframe with mape, mea, rtmse and median versions
#' @rdname f_predict_pl_regression_summarize
#' @seealso \code{\link{f_predict_pl_regression}}
#' @examples
#'   form = as.formula( 'disp~cyl+mpg')
#'
#' pl = mtcars %>%
#'   mutate(names = row.names(.)) %>%
#'   pipelearner::pipelearner() %>%
#'   pipelearner::learn_models( twidlr::rpart, form ) %>%
#'   pipelearner::learn_models( twidlr::randomForest, form ) %>%
#'   pipelearner::learn_models( twidlr::svm, form ) %>%
#'   pipelearner::learn() %>%
#'   f_predict_pl_regression( 'names' ) %>%
#'   f_predict_pl_regression_summarize()
#'
#' @export
f_predict_pl_regression_summarize = function( pl ){

  if( ! all( c('models.id'
               , 'cv_pairs.id'
               , 'train_p'
               , 'target'
               , 'model'
               , 'preds')
             %in% names(pl) )
  ){
    stop('run f_predict_pl_regression on learned pipelearner dataframe first')
  }


  pl = pl %>%
    select( - target ) %>%
    group_by( models.id, cv_pairs.id, train_p, model)  %>%
    unnest( preds ) %>%
    summarize( mean_target  = mean(target)
               , mean_pred  = mean(pred)
               , mae        = mean(resid_abs)
               , rtmse      = sqrt( mean(resid_squ) )
               , mape       = mean(ape)
               , med_target = median(target)
               , med_pred   = median(pred)
               , med_ae     = median(resid_abs)
               , rt_med_mse = sqrt( median(resid_squ) )
               , med_ape    = median(ape)
               )

}

#' @title plot model performance
#' @description add predictions to modelling dataframe and unnest, create a title column and a bins column
#' @param data dataframe with the columns title, bins, resid_abs, resid_squ, ape
#' @return taglist
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
#'  f_predict_plot_model_performance() %>%
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
               , 'resid_abs'
               , 'resid_squ'
               , 'ape'
               )
             %in% names(pl) )
  ){
    stop('unnest predictions and add bins first')
  }


  taglist = htmltools::tagList()

  p = f_plot_pretty_points(data, 'target1', 'resid', col_facet = 'title'
                       , y_title = 'Residuals', x_title = 'Target Variable') +
    geom_hline( yintercept = 0, size = 2)

  taglist[[1]] = plotly::ggplotly(p)

  p = ggplot( data, aes(bins, resid)) +
    geom_boxplot( aes(fill=title) ) +
    facet_wrap(~title) +
    geom_hline( yintercept = 0, size = 2) +
    theme( axis.text.x = element_text(angle = 90))+
    scale_fill_brewer(palette = 'Dark2') +
    labs(y = 'Residuals', x = 'Target Variable')

  taglist[[2]] = plotly::ggplotly(p)

  p = f_plot_pretty_points(data, 'target1', 'ape', col_facet = 'title'
                       , y_title = 'APE of Predictions', x_title = 'Target Variable')

  taglist[[3]] = plotly::ggplotly(p)

  p = ggplot( data, aes(bins, ape)) +
    geom_boxplot( aes(fill=title) ) +
    facet_wrap(~title)+
    theme( axis.text.x = element_text(angle = 90)) +
    scale_fill_brewer(palette = 'Dark2') +
    labs(y = 'APE of Predictions', x = 'Target Variable')

  taglist[[4]] = plotly::ggplotly(p)

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
    facet_wrap(~measure, scales = 'free') +
    scale_x_continuous( breaks = c(1:3), labels = levels(data_sum1$bins) ) +
    theme(axis.text.x = element_text(angle = 90)
          , legend.position = 'bottom') +
    labs( y = 'mean + SEM', x = 'Target Variable', color = '') +
    scale_color_brewer(palette = 'Dark2')

  taglist[[5]] = plotly::ggplotly(p)

  taglist[[6]] = DT::datatable( data_sum1 )

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
    facet_wrap(~measure, scales = 'free') +
    theme(axis.text.x = element_blank()
          , axis.ticks.x = element_blank()
          , legend.position = 'bottom') +
    labs( y = 'mean + SEM', x = '', color = '') +
    scale_color_brewer(palette = 'Dark2')

  taglist[[7]] = plotly::ggplotly(p)

  p = ggplot(data_sum2, aes( x = title, y = me, color = title) ) +
    geom_pointrange( aes( ymin = me - ci95, ymax = me + ci95)  ) +
    facet_wrap(~measure, scales = 'free') +
    theme(axis.text.x = element_blank()
          , axis.ticks.x = element_blank()
          , legend.position = 'bottom') +
    labs( y = 'mean + CI95', x = '', color = '') +
    scale_color_brewer(palette = 'Dark2')

  taglist[[8]] = plotly::ggplotly(p)
  taglist[[9]] = DT::datatable(data_sum2)

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
#' @seealso
#' \code{\link[modelr]{add_predictions}},\code{\link[modelr]{add_residuals}}
#' @rdname f_predict_regression_add_predictions
#' @export
#' @importFrom modelr add_predictions add_residuals
f_predict_regression_add_predictions = function(data, m, col_target, cols_id = NULL){

  col_target_sym = as.name(col_target)

  df = data %>%
    as.tibble() %>%
    modelr::add_predictions(m) %>%
    modelr::add_residuals(m) %>%
    mutate( target = !!col_target_sym )

    if ( ! is.null( cols_id ) ){

      df = df %>%
      select( one_of( c(cols_id, 'target', 'pred', 'resid') ) )

    }

    df = df %>%
      mutate( resid_abs   = abs(resid)
              , resid_squ = resid^2
              , ape       = abs(resid/pred) * 100
             )

  return(df)
}






