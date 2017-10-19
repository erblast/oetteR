
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

  response_var = m$terms %>%
    as.character() %>%
    .[2]

  response_var_sym = as.name(response_var)

  formula = m$terms %>%
    as.character()

  formula = paste0( formula[2], formula[1], formula[3] ) %>%
    as.formula()

  if( is.factor(data[[response_var]]) ){

    # rminer requires only numericals and does not like tibbles
    data = data %>%
      mutate_if( is.character, factor, ordered = T ) %>%
      mutate_all( as.numeric ) %>%
      mutate( !!response_var_sym := factor( !!response_var_sym, ordered = T ) ) %>%
      as.data.frame()


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
    arrange( desc(value) )

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
               )  %>%
    arrange( desc(value) ) %>%
    mutate( rank = rank( desc(value) ) )

  return(df)

}


