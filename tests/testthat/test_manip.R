

context('Test manipulation functions')

test_that( 'Test matrix conversion and transposition'
           , {
  t_tib = mtcars %>%
    f_manip_transpose_tibble()

  expect_equivalent( sort( row.names(mtcars) ), names(t_tib)[2:ncol(t_tib) ] )
  expect_equivalent( sort( names(mtcars) ), t_tib$row_names )

})
