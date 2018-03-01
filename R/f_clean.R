
#'@importFrom magrittr %>%
#'@importFrom stringr str_c
#'@import tibble
#'@import purrr
#'@import dplyr

#' @title f_clean_data
#' @description Performs a number of cleaning operations on a dataframe, detects
#'   numerical and categorical columns and returns a list containing the cleaned
#'   dataframe and vectors naming the columns with a specific data type.
#' @param data a dataframe
#' @param max_number_of_levels_factors If a factor variable contains more then
#'   the maximum number of levels the levels with the lowest frequency will be
#'   collapsed into 'others', Default: 10
#' @param min_number_of_levels_nums If a numeric number contains less that the
#'   minimum of distinct values it will be converted to a factor, Default: 6
#' @param exclude_missing exclude observations with missing values, Default: T
#' @param replace_neg_values_with_zero all negative values will be set to 0,
#'   Default: T
#' @param allow_neg_values specify columns for which negative values are
#'   allowed, Default: c("null")
#' @param id_cols specify columns containing ids.
#' @return returns a list
#'   \item{data}{the cleaned dataframe as tibble}
#'   \item{categoricals}{vector of column names containing categorical data}
#'   \item{categoricals_ordered}{vector of column names containing all ordered categorical data}
#'   \item{numericals}{vector of column names containing numerical data}
#'   \item{ids}{vector of column names containing ids}
#' @details The list this function returns can be a bit tedious to work with. If
#'   you want to engineer a new feature you have to manually update the
#'   categoricals or the numericals vector. I suggest that you do all the
#'   feature engineering before applying this function. The advantage of this
#'   column is that when you get to the modelling or visualisation steps you
#'   have full control over which columns are used for the formula or for the
#'   type of visualisation even if you might have bloated your dataframe with
#'   some junk columns.
#' @examples
#'  data_ls = f_clean_data( mtcars , id_cols = 'names')
#'  str(data_ls)
#' @seealso \code{\link{f_boxcox}}
#' @rdname f_clean_data
#' @export

f_clean_data = function(data
                        , max_number_of_levels_factors = 10
                        , min_number_of_levels_nums = 6
                        , exclude_missing = T
                        , replace_neg_values_with_zero = T
                        , allow_neg_values = c('null')
                        , id_cols = c('null')
                        ){


  data = data %>%
    as_tibble()

  # categorize cols

  ids           = id_cols

  none_ids      = names(data)[ ! names(data) %in% id_cols ]

  numericals    = none_ids[ map_lgl( data[, none_ids], is.numeric)]

  categoricals  = none_ids[ ! map_lgl( data[, none_ids], is.numeric)]

  all_variables = none_ids

  # drop observations with missing Values

  if(exclude_missing == T) {

    print( paste('Number of excluded observations:', nrow(data[ ! complete.cases(data), ])) )

    data = data[complete.cases(data),]

  }

  # add numericals with less than x distinc values to factors

  no_unique_vals = map_dbl(data[, numericals], function(x) length(unique(x)) )

  categoricals = c(categoricals, numericals[no_unique_vals <  min_number_of_levels_nums] )

  numericals = numericals[ !numericals %in% categoricals ]

  # replace negative values with zero

  if( replace_neg_values_with_zero == T){

    #throws warning if allow_neg_values == NULL
    suppressWarnings({

      data = data %>%
        dplyr::mutate_at( vars( numericals, - one_of(allow_neg_values)),
                   function(x) ifelse( x<0, 0, x ) )

    })
  }

  # convert all non numericals and grouping variables to factors
  # factors that could also be numeric become ordered and are listed
  # as categoricals ordered

  categoricals_ordered = c()

  for (var in categoricals ){

    if( is.numeric(data[[var]]) ){

      data[[var]] = factor(data[[var]], ordered = T)

      categoricals_ordered = c(categoricals_ordered, var)

    }else{

      data[[var]] = as.factor(data[[var]])

    }

  }

  # collapse smallest levels into one if factor levels exceed size x

  no_levels = map_dbl( data[, categoricals], function(x) length( levels(x) ) )

  for (var in names( no_levels[no_levels > max_number_of_levels_factors] )) {

    data[[var]] = forcats::fct_lump(data[[var]], n = max_number_of_levels_factors)
  }


  # drop categoricals with only one level

  if( !is_empty(categoricals)){

    data = data %>%
      mutate_at( vars(one_of(categoricals) )
                 , forcats::fct_drop )

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

  # return statement

  return( list( data                   = as.tibble(data)
                , numericals           = numericals
                , categoricals         = categoricals
                , categoricals_ordered = categoricals_ordered
                , all_variables        = all_variables
                , ids                  = id_cols
  )
  )
}

#' @title f_boxcox
#' @description Takes a data_ls object generated by f_clean_data() and adds
#'   boxcox transformations of all numeric variables.
#' @param data_ls data_ls object generated by f_clean_data(), or a named list
#'   list( data = <dataframe>, numericals = < vector with column names of
#'   numerical columns>)
#' @return returns a list
#'   \item{data}{the cleaned dataframe as tibble}
#'   \item{categoricals}{vector of column names containing categorical data}
#'   \item{numericals}{vector of column names containing numerical data}
#'   \item{ids}{vector of column names containing ids}
#'   \item{boxcox_names}{vector of column names containing boxcox transformed variables}
#'   \item{boxcox_data}{tibble containing boxcox transformed variables}
#' @details For a boxcox transformation all values mut be > 0. The function will
#'   automatically add the abs( min(x) ) + 0.0001 to all columns if they contain
#'   values <= 0
#' @examples
#' data_ls = f_clean_data(mtcars)
#' f_manip_get_most_common_level( data_ls$data$cyl)
#' @seealso \code{\link{f_clean_data}}
#' @rdname f_boxcox
#' @export

f_boxcox = function(data_ls){


  data          = data_ls$data
  numericals    = data_ls$numericals
  categoricals  = data_ls$categoricals
  all_variables = data_ls$all_variables

  data_ls = data_ls

  # Boxcox transform numericals

  if(!is_empty(numericals)){

    data_box_cox = data[, numericals] %>%
      mutate_if( function(x) min(x) <= 0
                 , function(x) x + abs( min(x) ) + 0.00001 )

    trans = caret::preProcess( as.data.frame(data_box_cox), c('BoxCox'), na.remove = T)

    pred = caret:::predict.preProcess(trans, as.data.frame(data_box_cox) )

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

  data_ls$boxcox_data  = pred
  data_ls$boxcox_names = names_transformed

  return(data_ls)
}

#' @title wrapper for f_clean_data without modifications to data
#' @param data a dataframe
#' @return returns a list
#'   \item{data}{the cleaned dataframe as tibble}
#'   \item{categoricals}{vector of column names containing categorical data}
#'   \item{categoricals_ordered}{vector of column names containing all ordered categorical data}
#'   \item{numericals}{vector of column names containing numerical data}
#'   \item{ids}{vector of column names containing ids}
#' @rdname f_clean_data_no_changes
#' @export
#' @seealso \code{\link{f_clean_data}}
f_clean_data_no_changes = function(data){

  f_clean_data( data
                , max_number_of_levels_factors = Inf
                , min_number_of_levels_nums = 1
                , exclude_missing = F
                , replace_neg_values_with_zero = F
  )

}
