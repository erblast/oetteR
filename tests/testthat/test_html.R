
context('html functions')

test_that('padding and breaks'
  ,{

  f_html_breaks(5)
  f_html_padding(DT::datatable(mtcars),5,'mtcars Data','subtitle', 'caption', 8 )

  expect_true( is.null( f_html_breaks(0) ) )

})
