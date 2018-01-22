## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
require(oetteR)
require(tidyverse)

## ------------------------------------------------------------------------

data_ls = f_clean_data(ISLR::Auto)
form = displacement~horsepower+cylinders+weight+acceleration
variable_color_code = f_plot_color_code_variables(data_ls)
limit            = 10

 pl = pipelearner::pipelearner( data_ls$data ) %>%
  pipelearner::learn_models( rpart::rpart, form ) %>%
  pipelearner::learn_models( randomForest::randomForest, form ) %>%
  pipelearner::learn_models( e1071::svm, form ) %>%
  pipelearner::learn() 
 
 
 
 

## ------------------------------------------------------------------------

pl = pl %>%
  mutate( imp   = map2(fit, train, f_model_importance) )

pl$imp  


## ------------------------------------------------------------------------
pl = pl %>%
  mutate( plot = pmap( list( m = fit, ranked_variables = imp, title = model, data = train)
                        , .f = f_model_plot_variable_dependency_regression
                        , formula = form
                        , data_ls = data_ls
                        , variable_color_code = variable_color_code
                       , limit = limit )
  )

pl$plot  


## ----fig.height = 30-----------------------------------------------------
pl = pl %>%
  mutate( range_var = map_chr(imp, function(x) head(x,1)$row_names )
          , grid = pmap( list( m = fit
                            , title = model
                            , variables = imp
                            , range_variable = range_var
                            , data = test
                     )
                     , f_model_plot_var_dep_over_spec_var_range
                     , formula = form
                     , data_ls = data_ls
                     , variable_color_code = variable_color_code
                     , log_y = F
                     , limit = 12
                     )
  )

pl$grid %>%
  walk( gridExtra::grid.arrange )

