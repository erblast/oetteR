

context('model features')


test_that('model importance regression'
          ,{

  m = twidlr::randomForest(mtcars, disp~.)
  f_model_importance_randomForest(m)

  m = twidlr::svm(mtcars, disp~.)
  f_model_importance_svm(m, mtcars)

  m = twidlr::rpart(mtcars, disp~.)
  f_model_importance_rpart(m)

  pl = pipelearner::pipelearner(mtcars) %>%
    pipelearner::learn_models( twidlr::rpart, disp~. ) %>%
    pipelearner::learn_models( twidlr::randomForest, disp~. ) %>%
    pipelearner::learn_models( twidlr::svm, disp~. ) %>%
    pipelearner::learn() %>%
    mutate( imp = map2(fit, train, f_model_importance) )

})


test_that('model importance classification'
          ,{

  data_ls = f_clean_data(mtcars)

  m = twidlr::randomForest(data_ls$data, cyl~.)
  f_model_importance_randomForest(m)

  m = twidlr::svm(data_ls$data, cyl~.)
  f_model_importance_svm(m, data_ls$data )

  m = twidlr::rpart(data_ls$data, cyl~.)
  f_model_importance_rpart(m)

  #classification with non numerics
  data_ls = f_clean_data(mtcars)
  data = data_ls$data
  m = twidlr::svm(data, cyl~.)
  f_model_importance_svm(m, data)

  #classification with character variable
  data = f_manip_matrix_2_tibble(mtcars)
  data$cyl = factor(data$cyl, ordered = T)
  m = twidlr::svm(data, cyl~.)
  f_model_importance_svm(m, data)

  pl = pipelearner::pipelearner(data_ls$data) %>%
    pipelearner::learn_models( twidlr::rpart, cyl~. ) %>%
    pipelearner::learn_models( twidlr::randomForest, cyl~. ) %>%
    pipelearner::learn_models( twidlr::svm, cyl~. ) %>%
    pipelearner::learn() %>%
    mutate( imp = map2(fit, train, f_model_importance) )

})

test_that('plot importance'
          ,{

  data_ls = f_clean_data(mtcars)
  variable_color_code = f_plot_color_code_variables(data_ls)
  m = twidlr::rpart(mtcars, disp~.)
  imp = f_model_importance_rpart(m)
  f_model_importance_plot(imp
                          , model_name = 'rpart'
                          , variable_color_code = variable_color_code
                          , 'example')

})



