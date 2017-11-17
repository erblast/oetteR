

context('visualize model dependencies')

test_that('f_model_plot_variable_dependency_regression'
  ,{

    make_plot = function(.f, log_y = F){
      data_ls             = f_clean_data(mtcars)
      data                = data_ls$data
      formula             = disp~hp+mpg+cyl
      m                   = .f(formula, data)
      ranked_variables    = f_model_importance( m, data)
      variable_color_code = f_plot_color_code_variables(data_ls)
      limit               = 10

      p = f_model_plot_variable_dependency_regression( m
                                                      , ranked_variables
                                                      , title = unlist( stringr::str_split( class(m)[1], '\\.') )[1]
                                                      , data
                                                      , formula
                                                      , data_ls
                                                      , variable_color_code
                                                      , limit
                                                      , log_y = log_y
                                                      )
    }

    print( make_plot(randomForest::randomForest) )
    print( make_plot(rpart::rpart) )
    print( make_plot(e1071::svm) )

    print( make_plot(randomForest::randomForest, log_y = T) )


    #pipelearner
    data_ls = f_clean_data(mtcars)
    form = as.formula('disp~hp+cyl+wt')
    variable_color_code = f_plot_color_code_variables(data_ls)
    limit            = 10

    pl = pipelearner::pipelearner( data_ls$data ) %>%
      pipelearner::learn_models( rpart::rpart, form ) %>%
      pipelearner::learn_models( randomForest::randomForest, form ) %>%
      pipelearner::learn_models( e1071::svm, form ) %>%
      pipelearner::learn() %>%
      mutate( imp   = map2(fit, train, f_model_importance)
              ,plot = pmap( list( m = fit, ranked_variables = imp, title = model, data = train)
                            , .f = f_model_plot_variable_dependency_regression
                            , formula = form
                            , data_ls = data_ls
                            , variable_color_code = variable_color_code
                            , limit = limit
                            )
              )

})

test_that('f_model_data_grid - more variables in formula than important variables'
  ,{

})

test_that( 'f_model_data_grid'
  ,{

  data_ls = f_clean_data(mtcars)
  formula = disp~cyl+mpg+hp
  col_var = 'mpg'
  n = 10

  tib = f_model_data_grid( col_var, data_ls, formula,  n )

  expect_true( all(duplicated(tib$cyl)[-1L]) )
  expect_equal( sd(tib$hp), 0 )
  expect_equal( length(f_manip_get_variables_from_formula(formula))
                , ncol(tib) )
  expect_true( sd(tib$mpg) > 0 )
  expect_equal( nrow(tib), n )

})

test_that('f_model_add_predictions_2_grid_regression'
  ,{

  make_grid = function( formula, .f, var){

    data_ls = f_clean_data(mtcars)
    m = .f(formula, data_ls$data)

    grid = f_model_data_grid('hp', data_ls, formula,  10) %>%
      f_model_add_predictions_2_grid_regression( m, 'disp')

  }

  # for numerical variable

  #response: numeric, predictors: numeric, range variable: numeric

  grid = make_grid( disp~hp+mpg, lm, 'hp')
  grid = make_grid( disp~hp+mpg, rpart::rpart, 'hp')
  grid = make_grid( disp~hp+mpg, randomForest::randomForest, 'hp')
  grid = make_grid( disp~hp+mpg, e1071::svm, 'hp')

  expect_true('disp' %in% names(grid) )

  #response: numeric, predictors: mixed, range variable: numeric

  grid = make_grid( disp~hp+mpg+cyl, lm, 'hp')
  grid = make_grid( disp~hp+mpg+cyl, rpart::rpart, 'hp')
  grid = make_grid( disp~hp+mpg+cyl, randomForest::randomForest, 'hp')
  grid = make_grid( disp~hp+mpg+cyl, e1071::svm, 'hp')

  expect_true('disp' %in% names(grid) )

  #response: numeric, predictors: mixed, range variable: categoric

  grid = make_grid( disp~hp+mpg+cyl, lm, 'cyl')
  grid = make_grid( disp~hp+mpg+cyl, rpart::rpart, 'cyl')
  grid = make_grid( disp~hp+mpg+cyl, randomForest::randomForest, 'cyl')
  grid = make_grid( disp~hp+mpg+cyl, e1071::svm, 'cyl')

  expect_true('disp' %in% names(grid) )

})

test_that( 'f_model_seq_range'

,{

   data_ls = f_clean_data(mtcars)
   col_var = 'disp'
   n = 10

   range = f_model_seq_range( data_ls, col_var, n )
   expect_equal( min(range), min( data_ls$data[, col_var] ) )
   expect_equal( max(range), max( data_ls$data[, col_var] ) )
   expect_equal( length(range), n )

   col_var = 'cyl'
   range = f_model_seq_range( data_ls, col_var )
   expect_equal( levels(data_ls$data[[col_var]]), levels(range) )
})
