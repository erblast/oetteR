
context('html widgets')

test_that('render html_widgets'
          ,{

            rmarkdown::render('./R/tests/testthat/run_html_widget_test.Rmd'
                              , output_file = Htmlwidgets.html)

})
