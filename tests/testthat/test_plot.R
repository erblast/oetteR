
context('Plot functions')

test_that('plot histogram'
          ,{

  data_ls = f_clean_data(mtcars)

  expect_is( f_plot_hist('disp', data_ls                                      ), 'ggplot' )
  expect_is( f_plot_hist('disp', data_ls, graph_type = 'violin', group = 'cyl'), 'ggplot' )
  expect_is( f_plot_hist('cyl' , data_ls, graph_type = 'violin', group = 'cyl'), 'ggplot' )
  expect_is( f_plot_hist('cyl' , data_ls, graph_type = 'bar',    group = 'cyl'), 'ggplot' )

})

test_that('plot time'
          ,{

    set.seed(1)
    data       = dplyr::sample_n( nycflights13::flights, 1000 )
    data$is_ua = ifelse( data$carrier == 'UA', 'UA', 'other')
    data$date  = data$year * 10000 + data$month * 100 + data$day
    data$date  = lubridate::as_date( data$date )
    data_ls    = f_clean_data( data, replace_neg_values_with_zero = F)

    expect_is( f_plot_time( 'arr_delay'
                            , 'month'
                            , data_ls
                            , group = 'is_ua'
                            , time_unit = 'month'
                            , time_variable_as_factor = T
                            ), 'ggplot' )

    expect_is( f_plot_time( 'arr_delay'
                            , 'month'
                            , data_ls
                            , time_unit = 'month'
                            , time_variable_as_factor = F
                            ), 'ggplot' )

})

test_that('color scale'
          ,{

            expect_true( length( f_get_col_vector60() ) == 74 )

          })
