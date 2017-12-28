

context('pre processing')

test_that('data_ls output format'
          ,{

  data_ls = f_clean_data( mtcars ) %>%
    f_boxcox()

  expect_true( 'data'         %in% names(data_ls) )
  expect_true( 'numericals'   %in% names(data_ls) )
  expect_true( 'categoricals' %in% names(data_ls) )
  expect_true( 'categoricals_ordered' %in% names(data_ls) )
  expect_true( 'ids'          %in% names(data_ls) )
  expect_true( 'boxcox_data'  %in% names(data_ls) )
  expect_true( 'boxcox_names' %in% names(data_ls) )

  expect_true( is.ordered(data_ls$data$cyl) )


})


test_that( 'f_clean replace_neg_values_with_zero option '
  ,{

  data = mtcars %>%
    mutate( mpg = - mpg
            , disp = - disp)

  data_ls = f_clean_data( data,allow_neg_values = 'disp' )

  expect_true( all( data$mpg < 0 ) )
  expect_true( all( data$disp < 0 ) )
  expect_true( all( data_ls$data$mpg >= 0 ) )
  expect_true( all( data_ls$data$disp < 0 ) )

})
