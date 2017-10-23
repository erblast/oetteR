
context('html widgets')

test_that('render html_widgets'
          ,{

  path = file.path( getwd(), 'tests/testthat')
  rmarkdown::render( file.path( path,'run_html_widget_test.Rmd' )
                    , output_file = file.path( path,'Htmlwidgets.html' ))

})


