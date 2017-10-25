
#' @title plot revenues cost and profit development over time as an area chart.
#' @description the function can graphically devide the chart into two periods
#'   e.g. past and future.
#' @param data datafram
#' @param col_revenue character vector denoting revenue column
#' @param col_cost character vector denoting cost column
#' @param col_time character vector denoting time column
#' @param now integer denoting a time which should be regarded as the
#'   breakpoint, Default: max(data[, col_time])
#' @param unit_time character vector, will label y-axis, Default: 'years'
#' @param unit_value character vector, will label x-axis, Default: 'CHF'
#' @param title character vector, will be title label, Default: ''
#' @param alpha_past double between 0 and 1 will determine alpha value for fill
#'   under the curve before the breakpoint, Default: 1
#' @param alpha_future double between 0 and 1 will determine alpha value for fill
#'   under the curve after the breakpoint, Default: 0.5
#' @return plot (is not plotly compatibel)
#' @details not plotly compatibel
#' @examples
#' data = tibble( time     = c(0,1,2,3,4,5,6,7,8,9,10)
#'               , revenue = - time^2 + time * 12
#'               , cost    = revenue * 0.4 * -1
#'              )
#'
#' print( f_plot_profit_lines( data, 'revenue', 'cost', 'time') )
#' print( f_plot_profit_lines( data, 'revenue', 'cost', 'time', now = 5) )
#'
#' @rdname f_plot_profit_lines
#' @export
f_plot_profit_lines = function( data
                                , col_revenue
                                , col_cost
                                , col_time
                                , now = max(data[,col_time])
                                , unit_time = 'years'
                                , unit_value = 'CHF'
                                , title = ''
                                , alpha_past = 1
                                , alpha_future = 0.5
){


  #calculate profit
  data = tibble( time      = data[[col_time ]]
                 , revenue = data[[col_revenue]]
                 , cost    = data[[col_cost]]
                 , profit  = revenue + cost
  )

  data = data %>%
    gather(key = 'key', value = 'value', - time )

  revenues = data %>%
    filter( key == 'revenue')

  cost = data %>%
    filter( key == 'cost')

  profit = data %>%
    filter( key == 'profit')

  # plot init

  p = ggplot()

  #past-----------------------------------------

  p = p +
    geom_area( data = revenues
               , mapping = aes( x = ifelse(time <= now,time, min(data$time)-1 )
                                , y = value
               )
               , alpha = alpha_past
               , fill = 'springgreen3'
               , position = 'identity'
    ) +
    geom_area( data = cost
               , mapping = aes( x = ifelse(time <= now,time, min(data$time)-1 )
                                , y = value
               )
               , alpha = alpha_past
               , fill = 'firebrick2'
               , position = 'identity'
    ) +
    geom_area( data = profit
               , mapping = aes( x = ifelse(time <= now,time, min(data$time)-1 )
                                , y = value
               )
               , alpha = alpha_past
               , fill = 'royalblue2'
               , position = 'identity'
    )

  #future---------------------------------------

  p = p +
    geom_area( data = revenues
               , mapping = aes( x = ifelse(time >= now, time, now)
                                , y = value
               )
               , alpha = alpha_future
               , fill = 'springgreen3'
               , position = 'identity'
               , show.legend = T
    ) +
    geom_area( data = cost
               , mapping = aes( x = ifelse(time >= now, time, now)
                                , y = value
               )
               , alpha = alpha_future
               , fill = 'firebrick2'
               , position = 'identity'
    ) +
    geom_area( data = profit
               , mapping = aes( x = ifelse(time >= now, time, now)
                                , y = value
               )
               , alpha = alpha_future
               , fill = 'royalblue2'
               , position = 'identity'
    )

  # points---------------------------------------

  p = p +
    geom_point( data = data
                , mapping = aes( x = time
                                 , y = value
                                 , color = key
                )
                , size  = 4
    )

  # lines---------------------------------------

  p = p +
    geom_line( data = revenues
               , mapping = aes( x = time
                                , y = value
               )
               , color = 'springgreen4'
               , size  = 2
               #, linetype = 3
    ) +
    geom_line( data = cost
               , mapping = aes( x = time
                                , y = value
               )
               , color = 'firebrick3'
               , size  = 2
               #, linetype = 2
    ) +
    geom_line( data = profit
               , mapping = aes( x = time
                                , y = value
               )
               , color = 'royalblue3'
               , size  = 2
               #, linetype = 2
    )

  # theme--------------------------------------

  p = p +
    theme_minimal() +
    theme( legend.position = 'bottom') +
    xlim( c( min(data$time) , max(data$time) ) )

  # scale--------------------------------------

  p = p +
    scale_color_manual( values = c('firebrick3', 'royalblue3', 'springgreen4')
                        , breaks = c('revenue', 'profit', 'cost') ) +
    #fill = NA removes filled boxes around the points
    guides( color = guide_legend(override.aes = list(size = 5, fill = NA) ) )

  #labels--------------------------------------

  p = p +
    labs( title = title
          , x   = unit_time
          , y   = c( unit_value )
          , color = ''
    )

}


#' @title plot revenues cost and profit development over time with bars for
#'   revenue and costs and an area chart for profit.
#' @description the function can graphically devide the chart into two periods
#'   e.g. past and future.
#' @param data datafram
#' @param col_revenue character vector denoting revenue column
#' @param col_cost character vector denoting cost column
#' @param col_time character vector denoting time column
#' @param now integer denoting a time which should be regarded as the
#'   breakpoint, Default: max(data[, col_time])
#' @param unit_time character vector, will label y-axis, Default: 'years'
#' @param unit_value character vector, will label x-axis, Default: 'CHF'
#' @param title character vector, will be title label, Default: ''
#' @param alpha_past double between 0 and 1 will determine alpha value for fill
#'   under the curve before the breakpoint, Default: 1
#' @param alpha_future double between 0 and 1 will determine alpha value for
#'   fill under the curve after the breakpoint, Default: 0.5
#' @param alpha_past_area as alpha_past but for area only, Default: 0.9
#' @param alpha_future_area as alpha_future but for area only, Default: 0.7#'
#' @return plot (to some extent plotly compatible)
#' @details to some extent plotly compatible
#' @examples
#' data = tibble( time     = c(0,1,2,3,4,5,6,7,8,9,10,11,12)
#'               , revenue = - time^2 + time * 12
#'               , cost    = revenue * 0.4 * -1
#'              )
#' data[1,'cost'] = -10
#' data
#'
#' print( f_plot_profit_bars_plus_area( data, 'revenue', 'cost', 'time') )
#' print( f_plot_profit_bars_plus_area( data, 'revenue', 'cost', 'time', now = 5) )
#'
#' #clv figure for presenation
#' p = f_plot_profit_bars_plus_area( data, 'revenue', 'cost', 'time', now = 5, alpha_past_area = 0) +
#'   theme( panel.grid.major  = element_blank()
#'         , panel.grid.minor = element_blank()
#'         , axis.text        = element_blank()
#'         )+
#'   labs(x = '', y = '')
#' print(p)
#'
#' @rdname f_plot_profit_bars_plus_area
#' @importFrom modelr seq_range add_predictions
#' @export
f_plot_profit_bars_plus_area = function( data
                                         , col_revenue
                                         , col_cost
                                         , col_time
                                         , now = max(data[,col_time])
                                         , unit_time = 'years'
                                         , unit_value = 'CHF'
                                         , title = ''
                                         , alpha_past = 1
                                         , alpha_future = 0.5
                                         , alpha_past_area = 0.9
                                         , alpha_future_area = 0.7
){


  #calculate profit
  data = tibble( time      = data[[col_time ]]
                 , revenue = data[[col_revenue]]
                 , cost    = data[[col_cost]]
                 , profit  = revenue + cost
  )

  data = data %>%
    gather(key = 'key', value = 'value', - time )

  revenues = data %>%
    filter( key == 'revenue')

  cost = data %>%
    filter( key == 'cost')

  profit = data %>%
    filter( key == 'profit')

  # model the profit curve

  m = lm( value~I(time^2)+time, profit)

  grid_profit = tibble( time = modelr::seq_range(0:max(data[,col_time]), n = 200 ) ) %>%
    modelr::add_predictions( m, var = 'value')

  # we need to generate one profit dataset for the area and one for the line

  grid_profit_area = grid_profit %>%
    filter(value >= 0 )

  grid_profit_line = grid_profit_area %>%
    bind_rows( filter(profit, time == max(time) )) %>%
    bind_rows( filter(profit, time == min(time) ) )

  grid_profit_area_future = grid_profit_area %>%
    filter(time >= now + 0.5)

  grid_profit_area_past = grid_profit_area %>%
    filter(time < now + 0.5)

  # plot init

  p = ggplot()



  #past-----------------------------------------

  p = p +
    geom_col( data = revenues
              , mapping = aes( x = ifelse(time <= now,time, min(data$time) )
                               , y = ifelse(time <= now, value, 0)
              )
              , alpha = alpha_past
              , fill = 'springgreen3'
              , position = 'identity'
    ) +
    geom_col( data = cost
              , mapping = aes( x = ifelse(time <= now,time, min(data$time) )
                               , y = ifelse(time <= now, value, 0)
              )
              , alpha = alpha_past
              , fill = 'firebrick2'
              , position = 'identity'
    )+
    geom_area( data = grid_profit_area_past
               , mapping = aes( x = time
                                , y = ifelse(time < now + 0.5,value, 0 )
               )
               , alpha = alpha_past_area
               , fill = 'royalblue2'
               , position = 'identity'
    )

  #future---------------------------------------

  p = p +
    geom_col( data = revenues
              , mapping = aes( x = ifelse(time > now, time, now)
                               , y = ifelse(time > now, value, 0)
              )
              , alpha = alpha_future
              , fill = 'springgreen3'
              , position = 'identity'
              , show.legend = T
    ) +
    geom_col( data = cost
              , mapping = aes( x = ifelse(time > now, time, now)
                               , y = ifelse(time > now, value, 0)
              )
              , alpha = alpha_future
              , fill = 'firebrick2'
              , position = 'identity'
    ) +
    geom_area( data = grid_profit_area_future
               , mapping = aes( x = time
                                , y = ifelse(time > now + 0.5, value, 0)
               )
               , alpha = alpha_future_area
               , fill = 'royalblue2'
               , position = 'identity'
    )


  # lines---------------------------------------

  p= p +
    geom_line( data = grid_profit_line
               , mapping = aes( x = time
                                , y = value
               )
               , color = 'royalblue3'
               , size  = 2
               , linetype = 2
    )

  # theme--------------------------------------

  p = p +
    theme_minimal()

  #labels--------------------------------------

  p = p +
    labs( title = title
          , x   = unit_time
          , y   = c( unit_value )
          , color = ''
    )

}


