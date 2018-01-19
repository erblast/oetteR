
context('Plot alluvial functions')

test_that('f_plot_alluvial'
  ,{

  data_ls = mtcars %>%
    f_clean_data()

  max_variables = 5
  variables = c( data_ls$categoricals[1:3], data_ls$numericals[1:3] )

  suppressMessages({

    p = f_plot_alluvial( data_ls$data, variables, max_variables, fill_by = 'first_variable' )
    p = f_plot_alluvial( data_ls$data, variables, max_variables, fill_by = 'last_variable' )
    p = f_plot_alluvial( data_ls$data, variables, max_variables, fill_by = 'all_flows' )
    p = f_plot_alluvial( data_ls$data, variables, max_variables, fill_by = 'values' )

    # manually order variable values
    p = f_plot_alluvial( data_ls$data
                     , variables
                     , max_variables
                     , fill_by = 'first_variable'
                     , order_levels = c('1', '0') )

    # #reserve tests, they take too long
    # data_ls = diamonds %>%
    #   sample_n(500) %>%
    #   f_clean_data()
    #
    # variables =  c('price', 'cut', 'carat', 'depth', 'color')
    #
    #
    # f_plot_alluvial( data_ls$data, variables, fill_by = 'first_variable' )
    # f_plot_alluvial( data_ls$data, variables, fill_by = 'last_variable' )
    # f_plot_alluvial( data_ls$data, variables, fill_by = 'all_flows' )
    # f_plot_alluvial( data_ls$data, variables, fill_by = 'values' )

  })

})


test_that('f_plot_alluvial_1v1'
          ,{
  # sample data
  monthly_flights = nycflights13::flights %>%
    group_by(month, tailnum, origin, dest, carrier) %>%
    summarise() %>%
    group_by( tailnum, origin, dest, carrier) %>%
    count() %>%
    filter( n == 12 ) %>%
    select( - n ) %>%
    left_join( nycflights13::flights ) %>%
    .[complete.cases(.), ] %>%
    ungroup() %>%
    mutate( tailnum = pmap_chr(list(tailnum, origin, dest, carrier), paste )
            , qu = cut(month, 4)) %>%
    group_by(tailnum, carrier, origin, dest, qu ) %>%
    summarise( mean_arr_delay = mean(arr_delay) ) %>%
    ungroup() %>%
    mutate( mean_arr_delay = ifelse( mean_arr_delay < 10, 'on_time', 'late' ) )

  levels(monthly_flights$qu) = c('Q1', 'Q2', 'Q3', 'Q4')

  data = monthly_flights

  col_x = 'qu'
  col_y = 'mean_arr_delay'
  col_fill = 'carrier'
  col_id = 'tailnum'

  suppressMessages({

  # flow coloring variants
  p = f_plot_alluvial_1v1( data, col_x, col_y, col_id, col_fill )
  p = f_plot_alluvial_1v1( data, col_x, col_y, col_id, fill_by = 'last_variable' )
  p = f_plot_alluvial_1v1( data, col_x, col_y, col_id, fill_by = 'first_variable' )
  p = f_plot_alluvial_1v1( data, col_x, col_y, col_id, fill_by = 'all_flows' )
  p = f_plot_alluvial_1v1( data, col_x, col_y, col_id, fill_by = 'value' )

  # use same color coding for flows and y levels
  p = f_plot_alluvial_1v1( data, col_x, col_y, col_id, fill_by = 'last_variable'
                       , col_vector_flow = f_plot_col_vector74()
                       , col_vector_value = f_plot_col_vector74() )

  # move fill variable to the left
  p = f_plot_alluvial_1v1( data, col_x, col_y, col_id, col_fill, fill_right = F )

  # reorder levels
  p = f_plot_alluvial_1v1( data, col_x, col_y, col_id, fill_by = 'first_variable'
                       , order_levels_y = c('on_time', 'late') )

  p = f_plot_alluvial_1v1( data, col_x, col_y, col_id, fill_by = 'first_variable'
                       , order_levels_x = c('Q4', 'Q3', 'Q2', 'Q1') )

  order_by_carrier_size = data %>%
    group_by(carrier) %>%
    count() %>%
    arrange( desc(n) ) %>%
    .[['carrier']]

  p = f_plot_alluvial_1v1( data, col_x, col_y, col_id, col_fill
                       , order_levels_fill = order_by_carrier_size )

  })

})


