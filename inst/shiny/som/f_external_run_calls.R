



f_call_multiview = function(data = ISLR::OJ){

  rmarkdown::run(file = 'multiview_v07.Rmd'
                 #, dir = "C:/Users/erbla/OneDrive/R/multiview shiny"
                 , auto_reload = F
                 , default_file = 'multiview_v07.Rmd'
                 , shiny_args  = list( launch.browser = T)
                 , render_args = list( params = list( data = data
                                                     , data_input = FALSE
                                                     )
                                       )
                 )

}

f_call_tree = function(data = ISLR::OJ){

  rmarkdown::run(file = 'mod_tree.Rmd'
                 #, dir = "C:/Users/erbla/OneDrive/R/multiview shiny"
                 , auto_reload = F
                 , default_file = 'mod_tree.Rmd'
                 , shiny_args  = list( launch.browser = T)
                 , render_args = list( params = list( data = data
                                                    )
                                      )
  )

}

f_call_som = function(data = ISLR::OJ){
  
  rmarkdown::run(file = 'multiview_som_v01.Rmd'
                 #, dir = "C:/Users/erbla/OneDrive/R/multiview shiny"
                 , auto_reload = F
                 , default_file = 'multiview_som_v01.Rmd'
                 , shiny_args  = list( launch.browser = T)
                 , render_args = list( params = list( data = data
                                                      , data_input = FALSE
                                                      )
                                      )
  )
  
}