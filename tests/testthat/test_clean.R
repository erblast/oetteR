


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
