
#'@import tibble
#'@import tidyr

#' @title converts matrices to tibble, preserving row.names
#' @description row.names are added as row_names column as the first column of
#'   the tibble. Function does not fail when object cannot be converted to
#'   tibble thus can be used to map over lists with various variable types such
#'   as modells and objects.
#' @param x any variable
#' @return a tibble or if the input variable is neither matrix dataframe or
#'   tibble the original input object.
#' @examples
#'
#' mat = as.matrix(mtcars)
#' head( mat, 10)
#' f_manip_matrix_2_tibble( mat )
#'
#' # convert all matrices from a list
#' pca = prcomp( mtcars ) %>%
#'  map( f_manip_matrix_2_tibble )
#' pca
#'
#' @rdname f_manip_matrix_2_tibble
#' @export
f_manip_matrix_2_tibble = function(x){

  if( ! ( is.matrix(x) | is.data.frame(x) ) ){
    warning('argument is not a matrix, returning input object')
    return(x)
  }

  tib = as_tibble(x)

  if( is.character( row.names(x) ) & ! is.tibble(x) ){

    row_names =  row.names(x)

    tib = tib %>%
      mutate( row_names = row_names) %>%
      select( row_names, everything() )
  }

  return(tib)
}

#'@title transpose a tibble
#'@description transpose a tibble, values in first column will become column
#'  titles. Row names will be converted to first columns
#'@param tib tibble
#'@return tibble
#' @examples
#'
#' tib = mtcars %>%
#'   as_tibble() %>%
#'   f_manip_transpose_tibble()
#'tib
#'@rdname f_manip_transpose_tibble
#'@export
f_manip_transpose_tibble = function(tib){

  if(  ! ( is.matrix(tib) | is.data.frame(tib) | is.tibble(tib) ) ){
    stop('input not transposable')
  }

  tib = tib %>%
    f_manip_matrix_2_tibble() %>%
    gather( key = 'key', value = 'value', 2:ncol(.) ) %>%
    spread( key = 1, value = value ) %>%
    rename( row_names = key ) %>%
    arrange( row_names )
}

#' @title
#' @description FUNCTION_DESCRIPTION
#' @param data_ls PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname f_manip_reduce_2_median_and_most_common_factor
#' @export
f_manip_summarize_2_median_and_most_common_factor = function(data_ls){

  df_numericals = data_ls$data %>%
    select( one_of(data_ls$numericals) ) %>%
    summarize_all( median )

  df_boxcox = data_ls$boxcox_data %>%
    summarize_all( median )

  df_categoricals = data_ls$data %>%
    select( one_of(data_ls$categoricals) ) %>%
    summarize_all( f_manip_get_most_common_level )

  data = df_numericals %>%
    bind_cols(df_categoricals) %>%
    select( one_of( names(data_ls$data) ) )

  return( list( data = data
                , boxcox_data = df_boxcox) )

}

f_manip_get_most_common_level = function(x){

  if( ! is.factor(x) ){
    stop( 'f_manip_get_most_common_level called on none factor vector' )
  }

  df = broom::tidy( summary(x) ) %>%
    arrange( desc(x) ) %>%
    .$names %>%
    head(1)

  return(df)

}

