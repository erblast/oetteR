

context('test predict functions')

test_that('add regression predictions to dataframe'
          ,{

  f = mtcars %>%
  mutate(names = row.names(.))
  m = rpart::rpart(disp~., df)
  f_predict_regression_add_predictions(df, m, 'disp', 'names')

})


test_that('add regression predictions to dataframe'
          ,{

  form = as.formula( 'disp~cyl+mpg')

  pl = mtcars %>%
  mutate(names = row.names(.)) %>%
  pipelearner::pipelearner() %>%
  pipelearner::learn_models( twidlr::rpart, form ) %>%
  pipelearner::learn_models( twidlr::randomForest, form ) %>%
  pipelearner::learn_models( twidlr::svm, form ) %>%
  pipelearner::learn() %>%
  f_predict_pl_regression( 'names' ) %>%
  f_predict_pl_regression_summarize()

})
