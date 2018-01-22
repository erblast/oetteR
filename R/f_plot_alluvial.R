
#' @title plot alluvial
#' @description Plots two variables of a dataframe on an alluvial plot. A third
#'   variable can be added either two the left or the right of the alluvial plot
#'   to provide coloring of the flows. All numerical variables are scaled,
#'   centered and YeoJohnson transformed before binning.
#' @param data a dataframe
#' @param col_x character vector denoting column for the x axis variable
#' @param col_y character vector denoting column for the y axis variable
#' @param col_id character vector denoting id column
#' @param col_fill character vector denoting color fill variable for flows,
#'   Default: NULL
#' @param fill_right logical, TRUE fill variable is added to the right FALSE to
#'   the left, Default: T
#' @param bins number of bins for automatic binning of numerical variables,
#'   Default: 5
#' @param bin_labels labesl for bins, Default: c("LL", "ML", "M", "MH", "HH")
#' @param order_levels_y character vector denoting order of y levels from low to
#'   high, does not have to be complete can also just be used to bring levels to
#'   the front, Default: NULL
#' @param order_levels_x character vector denoting order of x levels from low to
#'   high, does not have to be complete can also just be used to bring levels to
#'   the front, Default: NULL
#' @param order_levels_fill character vector denoting order of color fill
#'   variable levels from low to high, does not have to be complete can also
#'   just be used to bring levels to the front, Default: NULL
#' @param complete add implicitly missing data, Default: TRUE
#' @param fill_by one_of(c('first_variable', 'last_variable', 'all_flows',
#'   'values')), Default: 'first_variable'
#' @param col_vector_flow HEX colors for flows, Default: f_plot_col_vector74(faint
#'   = F, greys = F)
#' @param col_vector_value Hex colors for y levels/values, Default:
#'   RColorBrewer::brewer.pal(9, "Greys")[c(3, 6, 4, 7, 5)]
#' @return plot
#' @seealso \code{\link[RColorBrewer]{brewer.pal}}
#'   \code{\link[forcats]{fct_relevel}},\code{\link[forcats]{fct_rev}}
#'   \code{\link[rlang]{UQ}}
#'   \code{\link[ggalluvial]{geom_flow}},\code{\link[ggalluvial]{geom_stratum}}
#' @examples
#' \dontrun{
#' if(interactive()){
#'# sample data
#'  monthly_flights = nycflights13::flights %>%
#'  group_by(month, tailnum, origin, dest, carrier) %>%
#'  summarise() %>%
#'  group_by( tailnum, origin, dest, carrier) %>%
#'  count() %>%
#'  filter( n == 12 ) %>%
#'  select( - n ) %>%
#'  left_join( nycflights13::flights ) %>%
#'  .[complete.cases(.), ] %>%
#'  ungroup() %>%
#'  mutate( tailnum = pmap_chr(list(tailnum, origin, dest, carrier), paste )
#'          , qu = cut(month, 4)) %>%
#'  group_by(tailnum, carrier, origin, dest, qu ) %>%
#'  summarise( mean_arr_delay = mean(arr_delay) ) %>%
#'  ungroup() %>%
#'  mutate( mean_arr_delay = ifelse( mean_arr_delay < 10, 'on_time', 'late' ) )
#'
#'levels(monthly_flights$qu) = c('Q1', 'Q2', 'Q3', 'Q4')
#'
#'data = monthly_flights
#'
#'col_x = 'qu'
#'col_y = 'mean_arr_delay'
#'col_fill = 'carrier'
#'col_id = 'tailnum'
#'
#'# flow coloring variants
#'f_plot_alluvial_1v1( data, col_x, col_y, col_id, col_fill )
#'f_plot_alluvial_1v1( data, col_x, col_y, col_id, fill_by = 'last_variable' )
#'f_plot_alluvial_1v1( data, col_x, col_y, col_id, fill_by = 'first_variable' )
#'f_plot_alluvial_1v1( data, col_x, col_y, col_id, fill_by = 'all_flows' )
#'f_plot_alluvial_1v1( data, col_x, col_y, col_id, fill_by = 'value' )
#'
#'# use same color coding for flows and y levels
#'f_plot_alluvial_1v1( data, col_x, col_y, col_id, fill_by = 'last_variable'
#'                     , col_vector_flow = f_plot_col_vector74()
#'                     , col_vector_value = f_plot_col_vector74() )
#'
#'# move fill variable to the left
#'f_plot_alluvial_1v1( data, col_x, col_y, col_id, col_fill, fill_right = F )
#'
#'# reorder levels
#'f_plot_alluvial_1v1( data, col_x, col_y, col_id, fill_by = 'first_variable'
#'                     , order_levels_y = c('on_time', 'late') )
#'
#'f_plot_alluvial_1v1( data, col_x, col_y, col_id, fill_by = 'first_variable'
#'                     , order_levels_x = c('Q4', 'Q3', 'Q2', 'Q1') )
#'
#'order_by_carrier_size = data %>%
#'  group_by(carrier) %>%
#'  count() %>%
#'  arrange( desc(n) ) %>%
#'  .[['carrier']]
#'
#'f_plot_alluvial_1v1( data, col_x, col_y, col_id, col_fill
#'                     , order_levels_fill = order_by_carrier_size )
#'
#' }
#' }
#' @rdname f_plot_alluvial_1v1
#' @export
#' @importFrom RColorBrewer brewer.pal
#' @importFrom forcats fct_relevel fct_rev
#' @importFrom rlang UQ
#' @importFrom ggalluvial geom_flow geom_stratum
f_plot_alluvial_1v1 = function( data
                            , col_x
                            , col_y
                            , col_id
                            , col_fill = NULL
                            , fill_right = T
                            , bins = 5
                            , bin_labels = c('LL', 'ML', 'M', 'MH', 'HH')
                            , order_levels_y = NULL
                            , order_levels_x = NULL
                            , order_levels_fill = NULL
                            , complete = TRUE
                            , fill_by = 'first_variable'
                            , col_vector_flow = f_plot_col_vector74( faint = F, greys = F )
                            , col_vector_value =  RColorBrewer::brewer.pal(9, 'Greys')[c(3,6,4,7,5)]
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
    ungroup() %>%
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
    ordered_levels_y = c( ordered_levels_y, ordered_levels_fill)

    if(fill_right){
      ordered_levels_x = c( ordered_levels_x, col_fill )
    }else{
      ordered_levels_x = c( col_fill, ordered_levels_x )
    }

  }else{
    ordered_levels_fill = NULL
  }

  suppressWarnings({

    # add alluvial ids
    data_spread = data_trans %>%
      spread( key = !! sym_x, value = !! sym_y )

    data_alluvial_id = data_spread %>%
      select( - one_of(col_id) ) %>%
      group_by_all() %>%
      count() %>%
      ungroup() %>%
      mutate( alluvial_id = row_number() )

    data_new = data_alluvial_id %>%
      gather( key = 'x', value = 'value',  - one_of( c('alluvial_id', 'n', col_fill) ) ) %>%
      mutate( x = as.factor(x)
              , x = forcats::fct_relevel(x, ordered_levels_x))
  })
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

    suppressMessages({
      data_new = data_new %>%
        left_join( data_fill )
    })

  } else if( fill_by == 'all_flows'){

    data_new$fill = data_new$alluvial_id %>%
      as.factor(.)

  }else if( fill_by == 'value'){

    data_new$fill = data_new$value

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
            ) %>%
    mutate( value =  forcats::fct_rev(value) )

  n_flows    = max( f_manip_factor_2_numeric( data_new$alluvial_id) )
  n_per_x    = nrow(data)/length( unique(data[[col_x]] ) )
  reduced_to = round( n_flows/ n_per_x * 100, 1 )
  max_weight = max( data_new$n )
  max_weight_perc = round( max_weight/n_per_x * 100, 1 )

  print( paste('Number of flows:', n_flows) )
  print( paste('Original Dataframe reduced to', reduced_to, '%' ) )
  print( paste('Maximum weight of a singfle flow', max_weight_perc, '%') )


  #adjust col_vector length fill flows

  n_colors_needed = length( unique(data_new$fill) )

  col_vector_flow = f_plot_adjust_col_vector_length( n_colors_needed, col_vector_flow )

  df_fill_flow = tibble( fill = unique(data_new$fill)
                         , fill_flow = col_vector_flow )

  suppressMessages({
    data_new = data_new %>%
      left_join( df_fill_flow )
  })

  # adjust col_vector length fill value

  n_colors_needed = length( unique(data_new$value) )

  col_vector_value = f_plot_adjust_col_vector_length( n_colors_needed, col_vector_value )

  d_fill_value = tibble( value = unique(data_new$value)
                         , fill_value = col_vector_value )

  suppressMessages({
    data_new = data_new %>%
      left_join( d_fill_value )
  })


  if( ! is.null(col_fill) ){

    data_new = data_new %>%
      mutate( fill_value = ifelse( as.character(value) == as.character(rlang::UQ(sym_fill))
                                   , fill_flow, fill_value ) )
  }

  p <- ggplot(data_new,
              aes(x = x
                  , stratum = value
                  , alluvium = alluvial_id
                  , weight = n
                  , label = value)) +
    ggalluvial::geom_flow(stat = "alluvium"
                          , lode.guidance = "leftright"
                          , aes( fill = fill_flow
                                 , color = fill_flow )
    ) +
    ggalluvial::geom_stratum(  aes(fill = fill_value
                                   , color = fill_value)
                               ) +
    geom_label( stat = 'stratum') +
    theme(legend.position = "none" ) +
    scale_fill_identity() +
    scale_color_identity()

  # attach alluvial_id to id keys

  suppressMessages({

    data_key = data_spread %>%
      left_join( data_alluvial_id )

    p$data_key = data_key

  })

  return(p)
}


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
#' }
#' @seealso \code{\link[RColorBrewer]{brewer.pal}}
#'   \code{\link[forcats]{fct_relevel}}
#'   \code{\link[ggalluvial]{geom_flow}},\code{\link[ggalluvial]{geom_stratum}}
#' @rdname f_plot_alluvial
#' @export
#' @importFrom RColorBrewer brewer.pal
#' @importFrom forcats fct_relevel
#' @importFrom ggalluvial geom_flow geom_stratum
f_plot_alluvial = function( data
                            , variables = names(data)
                            , max_variables = 20
                            , bins = 5
                            , bin_labels = c('LL', 'ML', 'M', 'MH', 'HH')
                            , order_levels = NULL
                            , fill_by = 'first_variable'
                            , col_vector_flow = f_plot_col_vector74( faint = F, greys = F )
                            , col_vector_value =  RColorBrewer::brewer.pal(9, 'Greys')[c(3,6,4,7,5)]
){

  # both packages need to be loaded entirely
  require(ggalluvial)

  # adjust variable length

  if( max_variables > length(variables) ) max_variables = length(variables)

  variables = variables[1:max_variables]

  fill_by =  switch (fill_by
                     , first_variable  = variables[1]
                     , last_variable   = variables[ length(variables) ]
                     , all_flows       = 'alluvial_id'
                     , values          = 'value'
  )



  # transform numerical variables for binning

  data_new = data %>%
    select( one_of(variables) ) %>%
    f_manip_bin_numerics( bins, bin_labels) %>%
    group_by_all() %>%
    count() %>%
    ungroup()

  # preserve order of categorical variables

  ordered_levels = data_new %>%
    select_if( is.factor ) %>%
    head(1) %>%
    mutate_all( function(x) list(levels(x)) ) %>%
    gather( key = 'key', value = 'levels') %>%
    unnest(levels) %>%
    .$levels %>%
    unique()

  if( ! is.null(order_levels) ){
    ordered_levels = ordered_levels[ ! ordered_levels %in% order_levels ]
    ordered_levels = c( order_levels, ordered_levels)

  }

  # Prepare dataframe

  # this code is a bit redundant but the different fill options require
  # different transformations. I have marked the sections where there are
  # differences with ## ***

  suppressWarnings(

    if( fill_by != 'value' ){

      data_new <- data_new %>%
        mutate( alluvial_id = row_number()
                , fill = !! as.name(fill_by) )  %>% ## ***
        gather( key = 'x', value = 'value', - alluvial_id, - n , - fill) %>% ## ***
        mutate( value = as.factor(value)
                , value = forcats::fct_relevel( value, ordered_levels )
                , value = forcats::fct_rev(value)
                , x = as.factor(x)
                , x = forcats::fct_relevel(x, variables)
                , fill = as.factor(fill) ## ***
                , alluvial_id = as.factor(alluvial_id)
        )
    }else{

      data_new <- data_new %>%
        mutate( alluvial_id = row_number() )  %>%
        gather( key = 'x', value = 'value', - alluvial_id, - n ) %>% ## ***
        mutate( value = as.factor(value)
                , value = forcats::fct_relevel( value, ordered_levels )
                , value = forcats::fct_rev(value)
                , x = as.factor(x)
                , x = forcats::fct_relevel(x, variables)
                , fill = as.factor(value) ## ***
                , alluvial_id = as.factor(alluvial_id)
        )
    }

  )

  n_flows    = max( f_manip_factor_2_numeric( data_new$alluvial_id) )
  reduced_to = round( n_flows/nrow(data) * 100, 1 )
  max_weight = max( data_new$n )
  max_weight_perc = round( max_weight/nrow(data) * 100, 1 )

  print( paste('Number of flows:', n_flows) )
  print( paste('Original Dataframe reduced to', reduced_to, '%' ) )
  print( paste('Maximum weight of a singfle flow', max_weight_perc, '%') )



  #adjust col_vector length fill flows

  n_colors_needed = length( unique(data_new$fill) )

  col_vector_flow = f_plot_adjust_col_vector_length( n_colors_needed, col_vector_flow )

  df_fill_flow = tibble( fill = unique(data_new$fill)
                         , fill_flow = col_vector_flow )

  suppressMessages({
    data_new = data_new %>%
      left_join( df_fill_flow )
  })

  # adjust col_vector length fill value

  n_colors_needed = length( unique(data_new$value) )

  col_vector_value = f_plot_adjust_col_vector_length( n_colors_needed, col_vector_value )

  d_fill_value = tibble( value = unique(data_new$value)
                         , fill_value = col_vector_value )

  suppressMessages({
    data_new = data_new %>%
      left_join( d_fill_value )
  })


  p <- ggplot(data_new,
              aes(x = x
                  , stratum = value
                  , alluvium = alluvial_id
                  , weight = n
                  , label = value)) +
    ggalluvial::geom_flow(stat = "alluvium"
                          , lode.guidance = "leftright"
                          , aes( fill = fill_flow
                                 , color = fill_flow )
    ) +
    ggalluvial::geom_stratum(  aes(fill = fill_value
                                   , color = fill_value)
    ) +
    geom_label( stat = 'stratum') +
    theme(legend.position = "none" ) +
    scale_fill_identity() +
    scale_color_identity()

  return(p)
}



