


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
#' @description FUNCTION_DESCRIPTION
#' @param pl PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @rdname f_predict_pl_regression_summarize
#' @seealso \code{\link{f_predict_pl_regression}}
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
    stop('run f_predict_pl_regression non learned pipelearner dataframe first')
  }


  pl = pl %>%
    select( - target ) %>%
    group_by( models.id, cv_pairs.id, train_p, model)  %>%
    unnest( preds ) %>%
    summarize( mean_target  = mean(target)
               , mean_pred  = mean(pred)
               , mea        = mean(resid_abs)
               , rtmse      = sqrt( mean(resid_squ) )
               , mape       = mean(ape)
               , med_target = median(target)
               , med_pred   = median(pred)
               , rt_med_mse = sqrt( median(resid_squ) )
               , med_ape    = median(ape)
               )
}

#' @title adds predictions, residuals, abolute residuals, squared residuals and
#'   absolute percent error to a dataframe
#' @description absolute percent error = (abs(resid/pred)*100 )
#' @param df dataframe
#' @param m regression model
#' @param col_target character vector naming targtet/response variable
#' @param cols_id character vector naming id columns
#' @return dataframe
#' @examples
#' df = mtcars %>%
#' mutate(names = row.names(.))
#' m = rpart::rpart(disp~., df)
#' f_predict_regression_add_predictions(df, m, 'disp', 'names')
#' @seealso
#' \code{\link[modelr]{add_predictions}},\code{\link[modelr]{add_residuals}}
#' @rdname f_predict_regression_add_predictions
#' @export
#' @importFrom modelr add_predictions add_residuals
f_predict_regression_add_predictions = function(df, m, col_target, cols_id){

  col_target_sym = as.name(col_target)

  df = df %>%
    as.tibble() %>%
    modelr::add_predictions(m) %>%
    modelr::add_residuals(m) %>%
    mutate( target = !!col_target_sym ) %>%
    select( one_of( c(cols_id, 'target', 'pred', 'resid') ) ) %>%
    mutate( resid_abs   = abs(resid)
            , resid_squ = resid^2
            , ape       = abs(resid/pred) * 100
            )
  return(df)
}



