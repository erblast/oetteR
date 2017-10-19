

context('model features')


test_that('model importance regression'
          ,{

  m = twidlr::randomForest(mtcars, disp~.)
  f_model_importance_randomForest(m)

  m = twidlr::svm(mtcars, disp~.)
  f_model_importance_svm(m, mtcars)

  m = twidlr::rpart(mtcars, disp~.)
  f_model_importance_rpart(m)

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

})
