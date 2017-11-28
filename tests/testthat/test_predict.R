

context('test predict functions')

test_that('add regression predictions to dataframe'
          ,{

  df = mtcars %>%
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
    unnest( preds , .drop = FALSE ) %>%
    mutate( title = model ) %>%
    f_predict_pl_regression_summarize()

})


test_that('plot model performance'
          ,{


  form = as.formula( 'displacement~cylinders+mpg')

  df = ISLR::Auto %>%
    pipelearner::pipelearner() %>%
    pipelearner::learn_models( twidlr::rpart, form ) %>%
    pipelearner::learn_models( twidlr::randomForest, form ) %>%
    pipelearner::learn_models( twidlr::svm, form ) %>%
    pipelearner::learn() %>%
    f_predict_pl_regression( 'name' ) %>%
    unnest(preds)

  df %>%
    mutate( bins = cut(target1, breaks = 3 , dig.lab = 4)
            , title = paste(models.id, cv_pairs.id, train_p, target, model) ) %>%
    f_predict_plot_model_performance_regression() %>%
    f_plot_obj_2_html(type = 'taglist', 'test_me', title = 'Model Performance')

  file.remove('test_me.html')

  # more than three bins

  taglist = df %>%
    mutate( bins = cut(target1, breaks = 6 , dig.lab = 4)
            , title = paste(models.id, cv_pairs.id, train_p, target, model) ) %>%
    f_predict_plot_model_performance_regression()

})


