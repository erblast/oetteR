

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
  f_predict_pl_regression_summarize()

})


test_that('plot model performance'
          ,{


  form = as.formula( 'displacement~cylinders+mpg')

  ISLR::Auto %>%
    pipelearner::pipelearner() %>%
    pipelearner::learn_models( twidlr::rpart, form ) %>%
    pipelearner::learn_models( twidlr::randomForest, form ) %>%
    pipelearner::learn_models( twidlr::svm, form ) %>%
    pipelearner::learn() %>%
    f_predict_pl_regression( 'name' ) %>%
    unnest(preds) %>%
    mutate( bins = cut(target1, breaks = 3, dig.lab = 4 )
            , title = paste(models.id, cv_pairs.id, train_p, target, model) ) %>%
    ggplot( aes( bins, ape, fill = title ) ) +
    geom_boxplot()


  form = as.formula( 'displacement~cylinders+mpg')

  # f_plot_time can be used in a pipe but assumes that the variables are changing
  # and the data remains constant. We have to create a wrapper to change the order
  # of the variables



  data = ISLR::Auto %>%
    pipelearner::pipelearner() %>%
    pipelearner::learn_models( twidlr::rpart, form ) %>%
    pipelearner::learn_models( twidlr::randomForest, form ) %>%
    pipelearner::learn_models( twidlr::svm, form ) %>%
    pipelearner::learn() %>%
    f_predict_pl_regression( 'name' ) %>%
    unnest(preds) %>%
    mutate( bins = cut(target1, breaks = 3 , dig.lab = 4)
            , title = paste(models.id, cv_pairs.id, train_p, target, model) )



    ggplot( data, aes(target1, resid)) +
      geom_point() +
      facet_wrap(~title)

    ggplot( data, aes(bins, resid)) +
      geom_boxplot() +
      facet_wrap(~title)

    ggplot( data, aes(target1, ape)) +
      geom_point() +
      facet_wrap(~title)

    ggplot( data, aes(bins, ape)) +
      geom_boxplot() +
      facet_wrap(~title)

    data = ISLR::Auto %>%
      pipelearner::pipelearner() %>%
      pipelearner::learn_models( twidlr::rpart, form ) %>%
      pipelearner::learn_models( twidlr::randomForest, form ) %>%
      pipelearner::learn_models( twidlr::svm, form ) %>%
      pipelearner::learn() %>%
      f_predict_pl_regression( 'name' ) %>%
      f_predict_pl_regression_summarize()

    ggplot(data) +
      geom_pointrange( mapping = aes( x = model, ymin = (mea-mea_sem), ymax = (mea+mea_sem), y = mea ) )

})


