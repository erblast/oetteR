

context('test predict functions')

test_that('add regression predictions to dataframe rpart'
          ,{

  df = mtcars %>%
  mutate(names = row.names(.))
  m = rpart::rpart(disp~., df)
  f_predict_regression_add_predictions(df, m, 'disp', 'names')

})

test_that('add regression predictions to dataframe glmnet, HDtweedie'
          ,{

  formula = disp ~ hp + cyl

  df = mtcars %>%
    mutate(names = row.names(.)) %>%
    f_manip_data_2_model_matrix_format( formula )

  new_formula = df$formula
  new_df = df$data

  x = select(new_df, f_manip_get_variables_from_formula( new_formula) )
  y = new_df$disp

  m    = glmnet::glmnet( x = as.matrix(x), y = y)
  m_cv = glmnet::cv.glmnet( x = as.matrix(x), y = y, nfolds = 3)

  f_predict_regression_add_predictions( m = m
                                       , data_test = new_df
                                       , col_target = 'disp'
                                       , formula = new_formula
                                       , s = 1 )

  f_predict_regression_add_predictions( m = m_cv
                                        , data_test = new_df
                                        , col_target = 'disp'
                                        , formula = new_formula
                                        , s = 'lambda.1se' )

  m    = HDtweedie::HDtweedie( x = as.matrix(x), y = y)
  m_cv = HDtweedie::cv.HDtweedie( x = as.matrix(x), y = y, nfolds = 3)

  f_predict_regression_add_predictions( m = m
                                        , data_test = new_df
                                        , col_target = 'disp'
                                        , formula = new_formula
                                        , s = 1 )

  f_predict_regression_add_predictions( m = m_cv
                                        , data_test = new_df
                                        , col_target = 'disp'
                                        , formula = new_formula
                                        , s = 'lambda.1se' )
})

test_that('add regression predictions to dataframe gamlss'
          ,{

  formula = disp ~ hp + cyl

  df = mtcars %>%
    mutate(names = row.names(.)) %>%
    f_manip_data_2_model_matrix_format( formula )

  new_formula = df$formula
  new_df = df$data

  suppressWarnings({

    pl = pipelearner::pipelearner(new_df) %>%
      pipelearner::learn_models( gamlss::gamlss
                                 , formula = new_formula) %>%
      pipelearner::learn_cvpairs( pipelearner::crossv_kfold, k = 3) %>%
      pipelearner::learn() %>%
      mutate( preds = pmap( list(data_test = test
                                 , m = fit
                                 , data_train = train
                                 , col_target = target )
                            , f_predict_regression_add_predictions
                            , cols_id = 'names'
                            )
      ) %>%
      unnest(preds)

  })

})





test_that('add regression predictions to dataframe rpart'
          ,{

  df = mtcars %>%
    mutate(names = row.names(.))
  m = rpart::rpart(disp~., df)
  f_predict_regression_add_predictions(df, m, 'disp', 'names')

})


test_that('add regression predictions to dataframe'
          ,{

  form = as.formula( 'disp~cyl+mpg')

  suppressWarnings({

    pl = mtcars %>%
      mutate(names = row.names(.)) %>%
       pipelearner::pipelearner() %>%
      pipelearner::learn_models( rpart::rpart, form ) %>%
      pipelearner::learn_models( randomForest::randomForest, form ) %>%
      pipelearner::learn_models( e1071::svm, form ) %>%
      pipelearner::learn() %>%
      f_predict_pl_regression( 'names' ) %>%
      unnest( preds , .drop = FALSE ) %>%
      mutate( title = model ) %>%
      f_predict_pl_regression_summarize()

  })

})

test_that('add regression predictions for training data to df'
          ,{

  form = as.formula( 'disp~cyl+mpg')

  suppressWarnings({

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

})


test_that('plot model performance'
          ,{


  form = as.formula( 'displacement~cylinders+mpg')

  suppressWarnings({

  df = ISLR::Auto %>%
    pipelearner::pipelearner() %>%
    pipelearner::learn_models( rpart::rpart, form ) %>%
    pipelearner::learn_models( randomForest::randomForest, form ) %>%
    pipelearner::learn_models( e1071::svm, form ) %>%
    pipelearner::learn() %>%
    f_predict_pl_regression( 'name' ) %>%
    unnest(preds)

  })

  df %>%
    mutate( bins = cut(target1, breaks = 3 , dig.lab = 4)
            , title = paste(models.id, cv_pairs.id, train_p, target, model) ) %>%
    f_predict_plot_model_performance_regression() %>%
    f_plot_obj_2_html(type = 'taglist'
                      , 'test_me'
                      , title = 'Model Performance'
                      , quiet = TRUE )

  file.remove('test_me.html')

  # more than three bins

  taglist = df %>%
    mutate( bins = cut(target1, breaks = 6 , dig.lab = 4)
            , title = paste(models.id, cv_pairs.id, train_p, target, model) ) %>%
    f_predict_plot_model_performance_regression()

})

test_that( 'plot model predictions distributions'
  ,{

    form = as.formula( 'displacement~cylinders+mpg')

    suppressWarnings({

    df = ISLR::Auto %>%
      pipelearner::pipelearner() %>%
      pipelearner::learn_models( rpart::rpart, form ) %>%
      pipelearner::learn_models( randomForest::randomForest, form ) %>%
      pipelearner::learn_models( e1071::svm, form ) %>%
      pipelearner::learn() %>%
      f_predict_pl_regression( 'name' ) %>%
      unnest(preds)

    })

    f_predict_plot_regression_distribution(df
                                          , col_title = 'model'
                                          , col_pred = 'pred'
                                          , col_obs = 'target1')
})


test_that( 'plot model predictions as alluvial'
  ,{

  form = as.formula( 'displacement~cylinders+mpg')

  suppressWarnings({

    df = ISLR::Auto %>%
      mutate( name = paste( name, row_number() ) ) %>%
      pipelearner::pipelearner() %>%
      pipelearner::learn_models( rpart::rpart, form ) %>%
      pipelearner::learn_models( randomForest::randomForest, form ) %>%
      pipelearner::learn_models( e1071::svm, form ) %>%
      pipelearner::learn() %>%
      f_predict_pl_regression( 'name' ) %>%
      unnest(preds)

  })

  f_predict_plot_regression_alluvials(df
                                      , col_id = 'name'
                                      , col_title = 'model'
                                      , col_pred = 'pred'
                                      , col_obs = 'target1')

})
