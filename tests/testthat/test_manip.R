

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


test_that('f_stat_bring_to_pos_range'
  ,{
    vec = c( -2,0,2,4,6)
    vec = f_manip_bring_to_pos_range( vec )

    expect_true( min(vec) >= 0 )
})

test_that('f_manip_append_2_list'
         ,{
   l = list('a', 'b')
   l = f_manip_append_2_list(l, 'c')
   str(l)

   expect_true( length(l) == 3 )
})


test_that('f_manip_data_2_model_matrix_format'
  ,{

   data = mtcars
   formula = cyl ~ disp + am
   data_trans = f_manip_data_2_model_matrix_format( data, formula)

   data_ls = f_clean_data(mtcars)
   data = data_ls$data
   formula = cyl ~ disp + am + gear
   data_trans = f_manip_data_2_model_matrix_format( data, formula)

   data = mtcars
   data$names = row.names(data)
   formula = cyl ~ names
   data_trans = f_manip_data_2_model_matrix_format( data, formula)

})

test_that('f_manip_bin_numerics'
  ,{

  data = f_clean_data(mtcars) %>%
    .$data

  data_new = f_manip_bin_numerics(data)

  numerics = data_new %>%
    select_if( is.numeric ) %>%
    names()

  expect_true( is_empty(numerics) )
  expect_true( ! is_empty(data_new) )
  expect_identical( names(data_new) , names(data) )

})


test_that('f_manip_bin_numerics no numerics in data'
          ,{

  data = mtcars %>%
    mutate_all( as.factor )

  data_new = f_manip_bin_numerics(data)

  expect_identical(data, data_new)

})

test_that('f_manip_bin_numerics zero variance columns'
          ,{

  data = mtcars %>%
    as_tibble() %>%
    mutate( zero_var = 1
            , zero = 0
            , near_zero_var = c( rep(1,nrow(.)-1), 0.9 ) )

  data_new = f_manip_bin_numerics(data)

  expect_identical( select(data, zero_var, zero)
                    , select(data_new, zero_var, zero) )

  expect_true( is.factor(data_new$near_zero_var) )

})

