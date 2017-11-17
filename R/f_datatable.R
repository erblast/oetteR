
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
#' @param round_other_nums round other numerical columns to that digit. Will not
#'   do anything if NULL, Default: NULL
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
                                 , round_other_nums = NULL){

  if( is.null(df) | is_empty(df) ){
    return()
  }

  bool_perc = stringr::str_detect(names(df), '_perc' ) |
              stringr::str_detect(names(df), 'perc_' )

  bool_count = stringr::str_detect(names(df), '_count' ) |
               stringr::str_detect(names(df), 'count_' )

  bool_sign = stringr::str_detect(names(df), 'p_val' )

  bool_nums = map_lgl(df, is.numeric)


  pos_perc  = which(bool_perc)
  pos_count = which(bool_count)
  pos_sign  = which(bool_sign)
  pos_nums  = which(bool_nums)
  pos_nums  = pos_nums[ ! pos_nums %in% c(pos_perc, pos_sign, pos_count) ]

  df[, bool_perc] = df[, bool_perc] / 100

  dt = DT::datatable(df
                     , extensions = c('Buttons', 'ColReorder', 'KeyTable')
                     , options = list(
                       dom = 'Bflrtip'
                       , buttons = I( c('colvis','copy', 'excel') )
                       , colReorder = TRUE
                       , keys = TRUE
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

