

context('model features')


test_that('model features regression'
          ,{

  m = twidlr::randomForest(mtcars, disp~.)
  f_model_importance_randomForest(m)

  m = twidlr::svm(mtcars, disp~.)
  f_model_importance_svm(m)

  m = twidlr::rpart(mtcars, disp~.)
  f_model_importance_rpart(m)

  hotzenplotz
})
