

context('visualize model dependencies')

test_that('f_model_plot_variable_dependency_regression'
  ,{

    make_plot = function(.f){
      data_ls          = f_clean_data(mtcars)
      data             = data_ls$data
      formula          = disp~cyl+mpg+hp
      m                = .f(formula, data_ls$data)
      ranked_variables = f_model_importance( m, data_ls$data)
      col_vector       = f_plot_color_code_variables(data_ls)
      limit            = 10

      p = f_model_plot_variable_dependency_regression( m
                                                      , ranked_variables
                                                      , data
                                                      , formula
                                                      , data_ls
                                                      , col_vector
                                                      , limit
                                                      )
    }

    print( make_plot(randomForest::randomForest) )
    print( make_plot(rpart::rpart) )
    print( make_plot(e1071::svm) )

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

  make_grid = function( formula, .f){

    data_ls = f_clean_data(mtcars)
    m = .f(formula, data_ls$data)

    grid = f_model_data_grid('hp', data_ls, formula,  10) %>%
      f_model_add_predictions_2_grid_regression( m, 'disp')

  }
  #only numericals

  grid = make_grid( disp~hp+mpg, lm)
  grid = make_grid( disp~hp+mpg, rpart::rpart)
  grid = make_grid( disp~hp+mpg, randomForest::randomForest)
  grid = make_grid( disp~hp+mpg, e1071::svm)

  expect_true('disp' %in% names(grid) )

  #predict numeric with mixed numeric and categoric

  grid = make_grid( disp~hp+mpg+cyl, lm)
  grid = make_grid( disp~hp+mpg+cyl, rpart::rpart)
  grid = make_grid( disp~hp+mpg+cyl, randomForest::randomForest)
  grid = make_grid( disp~hp+mpg+cyl, e1071::svm)

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
   expect_equal( levels(data_ls$data[[col_var]]), range )
})
