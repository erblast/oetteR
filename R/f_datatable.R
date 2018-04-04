
#' @title convert dataframe to DT:datatable inlcuding most usefull extensions
#'   and options
#' @description inludes the features for excel and clipboard export, hide and
#'   unhide (column visibility), reorder columns per drag and drop, navigate
#'   table with arrow keys and prefix/suffix directed rounding of numerical
#'   values.
#'   \itemize{
  #'   \item count_/ _count will round to 0
  #'   \item p_val will round to Default: 2
  #'   \item perc_/ _perc will round to Default: 1
#'   }
#' @param df dataframe
#' @param round_perc digits for percentages, Default: 1
#' @param round_sign digits for p_values, Default: 2
#' @param count_cols_as_int detect count columns, Default: T
#' @param round_other_nums round other numerical(double) columns to that digit. Will not
#'   do anything if NULL, Default: NULL
#'@param page_length integer Default page length of table, Default: 10
#' @return DT:datatable
#' @examples
#' data_ls = f_clean_data(mtcars)
#' f_stat_group_counts_percentages(data_ls, 'cyl') %>%
#'   f_datatable_universal()
#'
#' f_stat_group_mean_medians(data_ls, 'cyl') %>%
#'   f_datatable_universal(round_other_nums = 2)
#' @seealso \code{\link[stringr]{str_detect}}
#'   \code{\link[DT]{datatable}},\code{\link[DT]{formatPercentage}},\code{\link[DT]{formatSignif}},\code{\link[DT]{formatRound}}
#'
#' @rdname f_datatable_universal
#' @export
#' @importFrom stringr str_detect
#' @importFrom DT datatable formatPercentage formatSignif formatRound
f_datatable_universal = function(df
                                 , round_perc = 1
                                 , round_sign = 2
                                 , count_cols_as_int = T
                                 , round_other_nums = NULL
                                 , page_length = 10){

  if( is.null(df) | is_empty(df) ){
    return()
  }

  df = f_manip_double_2_int( df )

  max_length = nrow(df)

  page_length_menu = c(10,25,50,100, max_length, page_length) %>%
    unique()

  page_length_menu = page_length_menu[ !page_length_menu > max_length]

  bool_perc = stringr::str_detect(names(df), '_perc' ) |
              stringr::str_detect(names(df), 'perc_' )

  bool_count = stringr::str_detect(names(df), '_count' ) |
               stringr::str_detect(names(df), 'count_' )

  bool_sign = stringr::str_detect(names(df), 'p_val' ) |
              stringr::str_detect(names(df), 'p.val' )

  bool_nums = map_lgl(df, is.double)


  pos_perc  = which(bool_perc)
  pos_count = which(bool_count)
  pos_sign  = which(bool_sign)
  pos_nums  = which(bool_nums)
  pos_nums  = pos_nums[ ! pos_nums %in% c(pos_perc, pos_sign, pos_count) ]

  # DT::formatPercentage expects 100% == 1 format
  if( any(bool_perc) ){
    df[, bool_perc] = df[, bool_perc] / 100
  }

  dt = DT::datatable(df
                     , extensions = c('Buttons', 'ColReorder', 'KeyTable')
                     , rownames = FALSE
                     , options = list(
                       dom = 'Bflrtip'
                       , buttons = I( c('colvis','copy', 'excel') )
                       , colReorder = TRUE
                       , keys = TRUE
                       , pageLength = page_length
                       , lengthMenu = page_length_menu
                     )
  )

  dt = dt %>%
    DT::formatPercentage( pos_perc,  round_perc ) %>%
    DT::formatSignif( pos_sign, round_sign ) %>%
    DT::formatRound( pos_count, 0 )

  if( is.numeric(round_other_nums) & ! is_empty(pos_nums) ){

    dt = dt %>%
      DT::formatRound( pos_nums, round_other_nums)

  }

  return(dt)
}

#' @title convert dataframe to DT:datatable with minimal features
#' @param df PARAM_DESCRIPTION
#' @param escape boolean, escape html content, Default:FALSE
#' @return DT::datatable
#' @examples
#' f_datatable_minimal(mtcars)
#' @seealso
#'  \code{\link[DT]{datatable}}
#' @rdname f_datatable_minimal
#' @export
#' @importFrom DT datatable
f_datatable_minimal = function(df, escape = F ){

  dt = DT::datatable( df , escape = escape, rownames = FALSE, options = list( dom = ''
                                                                              , pageLength = nrow(df) ) )

  return(dt)
}



