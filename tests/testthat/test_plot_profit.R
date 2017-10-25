
context('Test profit plots')


test_that('f_profit_plot_lines'
          ,{

  data = tibble( time     = c(0,1,2,3,4,5,6,7,8,9,10)
                , revenue = - time^2 + time * 12
                , cost    = revenue * 0.4 * -1
               )

  print( f_plot_profit_lines( data, 'revenue', 'cost', 'time') )
  print( f_plot_profit_lines( data, 'revenue', 'cost', 'time', now = 5) )

})


test_that('f_profit_plot_bars_plus_area'
          ,{

  data = tibble( time     = c(0,1,2,3,4,5,6,7,8,9,10,11,12)
                , revenue = - time^2 + time * 12
                , cost    = revenue * 0.4 * -1
               )
  data[1,'cost'] = -10
  data

  print( f_plot_profit_bars_plus_area( data, 'revenue', 'cost', 'time') )
  print( f_plot_profit_bars_plus_area( data, 'revenue', 'cost', 'time', now = 5) )

})
