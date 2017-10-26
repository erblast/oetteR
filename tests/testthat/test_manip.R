

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
      f_boxcox

    summarized_ls = f_manip_reduce_2_median_and_most_common_factor(data_ls)


    expect_equal( median(summarized_ls$data$disp), median(mtcars$disp) )
    expect_equal( median(summarized_ls$boxcox_data$disp_boxcox), median(data_ls$boxcox_data$disp_boxcox))
    expect_identical( summarized$data$cyl[1],  '8' )

  })


test_that( 'f_manip_get_most_common_level'
  ,{
    data_ls = f_clean_data(mtcars)
    expect_identical( f_manip_get_most_common_level( data_ls$data$cyl), '8')
})

