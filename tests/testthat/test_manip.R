

context('Test manipulation functions')

test_that( 'Test matrix conversion and transposition'
           , {
  t_tib = mtcars %>%
    f_manip_transpose_tibble()

  expect_equivalent( sort( row.names(mtcars) ), names(t_tib)[2:ncol(t_tib) ] )
  expect_equivalent( sort( names(mtcars) ), t_tib$row_names )

})

test_that( 'f_manip_summarize_2_median_and_most_common_factor'
  ,{

    data_ls = f_clean_data(mtcars) %>%
      f_boxcox()

    summarized_ls = f_manip_summarize_2_median_and_most_common_factor(data_ls)

    expect_equal( median(summarized_ls$data$disp), median(mtcars$disp) )
    expect_equal( median(summarized_ls$boxcox_data$disp_boxcox), median(data_ls$boxcox_data$disp_boxcox))
    expect_true( summarized_ls$data$cyl[1] == '8' )

  })

test_that( 'f_manip_summarize_2_median_and_most_common_factor without boxcox'
  ,{

   data_ls = f_clean_data(mtcars)

   summarized_ls = f_manip_summarize_2_median_and_most_common_factor(data_ls)

   expect_equal( median(summarized_ls$data$disp), median(mtcars$disp) )
   expect_true( summarized_ls$data$cyl[1] == '8' )

 })


test_that( 'f_manip_get_most_common_level'
  ,{
    data_ls = f_clean_data(mtcars)
    x = f_manip_get_most_common_level(data_ls$data$cyl)
    expect_true('8' == x)
})

test_that('extract info from formula'
  ,{
    f = foo~bar1 + bar2

    vars = f_manip_get_variables_from_formula(f)
    expect_true( 'bar1' %in% vars)
    expect_true( 'bar2' %in% vars)

    response_var = f_manip_get_response_variable_from_formula(f)
    expect_identical( response_var, 'foo')

    f = foo~.
    expect_error( f_manip_get_variables_from_formula(f) )
})

test_that( 'f_manip_factor_2_numeric'
  ,{

    fac_num = factor( c(1,3,8) )
    fac_chr = factor( c('foo','bar') )
    fac_chr_ordered = factor( c('a','b','c'), ordered = T )

    expect_identical( f_manip_factor_2_numeric( fac_num ), c(1,3,8) )
    expect_identical( f_manip_factor_2_numeric( fac_chr ), c(2,1) )
    expect_identical( f_manip_factor_2_numeric( fac_chr_ordered ), c(1,2,3) )
  })
