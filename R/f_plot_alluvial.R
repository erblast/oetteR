#' @title plot alluvial
#' @description plots a dataframe as an alluvial plot. All numerical variables
#'   are scaled, centered and YeoJohnson transformed before binning.
#' @param data a dataframe
#' @param variables vector denoting names and order of the plotted variables,
#'   Default: names(data)
#' @param max_variables maximum number of variables, Default: 20
#' @param bins number of bins for numerical variables, Default: 5
#' @param bin_labels labels for the bins from low to high, Default: c("LL",
#'   "ML", "M", "MH", "HH")
#' @param order_levels character vector denoting levels to be reorderer from low to high
#' @param fill_by one_of(c('first_variable', 'last_variable', 'all_flows',
#'   'values')), Default: 'first_variable'
#' @param col_vector vector with HEX color codes, Default:
#'   RColorBrewer::brewer.pal(name = "Dark2", n = 8)
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#' data_ls = mtcars %>%
#'   f_clean_data()
#'
#' max_variables = 5
#' variables = c( data_ls$categoricals[1:3], data_ls$numericals[1:3] )
#'
#' f_plot_alluvial( data_ls$data, variables, max_variables, fill_by = 'first_variable' )
#' f_plot_alluvial( data_ls$data, variables, max_variables, fill_by = 'last_variable' )
#' f_plot_alluvial( data_ls$data, variables, max_variables, fill_by = 'all_flows' )
#' f_plot_alluvial( data_ls$data, variables, max_variables, fill_by = 'values' )
#'
#' # manually order variable values
#' f_plot_alluvial( data_ls$data
#'                  , variables
#'                  , max_variables
#'                  , fill_by = 'first_variable'
#'                  , order_levels = c('1', '0') )
#' }
#' @seealso \code{\link[RColorBrewer]{brewer.pal}}
#'   \code{\link[forcats]{fct_relevel}}
#'   \code{\link[ggalluvial]{geom_flow}},\code{\link[ggalluvial]{geom_stratum}}
#' @rdname f_plot_alluvial
#' @export
#' @importFrom RColorBrewer brewer.pal
#' @importFrom forcats fct_relevel
#' @importFrom ggalluvial geom_flow geom_stratum
f_plot_alluvial_1v1 = function( data
                            , col_x
                            , col_y
                            , col_id
                            , col_fill = NULL
                            , max_variables = 20
                            , bins = 5
                            , bin_labels = c('LL', 'ML', 'M', 'MH', 'HH')
                            , order_levels_y = NULL
                            , order_levels_x = NULL
                            , order_levels_fill = NULL
                            , complete = TRUE
                            , fill_by = 'first_variable'
                            , col_vector = RColorBrewer::brewer.pal(name = 'Dark2', n = 8)
){

  # package needs to be loaded entirely
  require(ggalluvial)

  # symbols

  sym_x = as.name( col_x )
  sym_y = as.name( col_y )
  sym_id = as.name( col_id )
  if( ! is.null(col_fill) ){
    sym_fill = as.name( col_fill )
  }else{
    sym_fill = NULL
  }

  # transform numerical variables for binning

  data_trans = data %>%
    select( one_of(col_x, col_y, col_fill, col_id) ) %>%
    mutate( !! sym_x := as.factor( !! sym_x )
            , !! sym_id := as.factor( !! sym_id )
            ) %>%
    f_manip_bin_numerics( bins, bin_labels) %>%
    mutate( !! sym_y := as.factor( !! sym_y ) )

  # add implicit missing data
  if(complete) data_trans = data_trans %>%
    complete( !! sym_x , !! sym_fill )

  # preserve order of categorical variables

  ordered_levels_x = c( order_levels_x, levels( data_trans[[col_x]] ) ) %>% unique()
  ordered_levels_y = c( order_levels_y, levels( data_trans[[col_y]] ) ) %>% unique()

  if( ! is.null(col_fill) ){
    ordered_levels_fill = c( order_levels_fill, levels( data_trans[[col_fill]] ) ) %>% unique()
  }else{
    ordered_levels_fill = NULL
  }


  # add alluvial ids
  data_new = data_trans %>%
    spread( key = !! sym_x, value = !! sym_y ) %>%
    select( - one_of(col_id) ) %>%
    group_by_all() %>%
    count() %>%
    ungroup() %>%
    mutate( alluvial_id = row_number() ) %>%
    gather( key = 'x', value = 'value',  - one_of( c('alluvial_id', 'n', col_fill) ) ) %>%
    mutate( x = as.factor(x)
            , x = forcats::fct_relevel(x, ordered_levels_x))

  # compose fill columns

  last_x = levels(data_new$x) %>%
    .[ length(.) ]

  first_x = levels(data_new$x)[1]

  if( ! is.null(col_fill) ){


    data_fill = data_new %>%
      filter( x == last_x ) %>%
      mutate( value = !! sym_fill
              , x = col_fill )

    suppressWarnings({
        data_new = data_new %>%
        bind_rows( data_fill )
    })

    data_new$fill = data_new[[col_fill]]

  }else if( fill_by %in% c( 'first_variable', 'last_variable') ) {

    data_fill = data_new

    if( fill_by == 'first_variable') data_fill = data_fill %>%
        filter( x == first_x )


    if(fill_by == 'last_variable') data_fill = data_fill %>%
        filter( x == last_x )

    data_fill = data_fill %>%
      select( alluvial_id, value ) %>%
      rename( fill = value )

    data_new = data_new %>%
      left_join( data_fill )

  } else if( fill_by == 'all_flows'){

    data_new$fill = data_new$alluvial_id %>%
      as.factor(.)

  }else{
    warning( 'no valid fill option selected')

    data_new$fill = 'a'

  }

  # reformat factors

  data_new = data_new %>%
    mutate_if( is.character, as.factor ) %>%
    mutate( x =  forcats::fct_relevel( x, ordered_levels_x )
            , value =  forcats::fct_relevel( value, ordered_levels_y )
            , fill =  forcats::fct_relevel( fill , ordered_levels_fill )
            )

  n_flows    = max( f_manip_factor_2_numeric( data_new$alluvial_id) )
  reduced_to = round( n_flows/nrow(data) * 100, 1 )
  max_weight = max( data_new$n )
  max_weight_perc = round( max_weight/nrow(data) * 100, 1 )

  print( paste('Number of flows:', n_flows) )
  print( paste('Original Dataframe reduced to', reduced_to, '%' ) )
  print( paste('Maximum weight of a singfle flow', max_weight_perc, '%') )


  #adjust col_vector length

  n_colors_needed = length( unique(data_new$fill) )

  if( length(col_vector) < n_colors_needed ) col_vector = c( col_vector, f_plot_col_vector74() )

  col_vector = f_plot_adjust_col_vector_length( n_colors_needed, col_vector )


  p <- ggplot(data_new,
              aes(x = x
                  , stratum = value
                  , alluvium = alluvial_id
                  , weight = n
                  , label = value)) +
    ggalluvial::geom_flow(stat = "alluvium"
                          , lode.guidance = "leftright"
                          , color = "darkgray"
                          , aes( fill = fill )
    ) +
    ggalluvial::geom_stratum( fill = 'darkgrey'
                              , color = 'black' ) +
    geom_label( stat = 'stratum') +
    theme(legend.position = "none" ) +
    scale_fill_manual( values = col_vector )

  return(p)
}

