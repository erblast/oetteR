
context('html widgets')

test_that('render html_widgets'
          ,{

            rmarkdown::render('./run_html_widget_test.Rmd'
                              , output_file = './Htmlwidgets.html')

})
