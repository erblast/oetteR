

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


test_that('check if importance df contains variables not in formula'
  ,{

    f = disp ~ cyl + mpg
    m = randomForest::randomForest( f, mtcars)
    imp = f_model_importance_randomForest(m)
    expect_true( all( imp$row_names %in% f_manip_get_variables_from_formula(f) ) )

    f = disp ~ cyl + mpg
    m = e1071::svm( f, mtcars)
    imp = f_model_importance_svm(m, data = mtcars)
    expect_true( all( imp$row_names %in% f_manip_get_variables_from_formula(f) ) )

    f = disp ~ cyl + mpg
    m = rpart::rpart( f, mtcars)
    imp = f_model_importance_rpart(m)
    expect_true( all( imp$row_names %in% f_manip_get_variables_from_formula(f) ) )

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
                          , title = 'rpart'
                          , variable_color_code = variable_color_code
                          )

})

test_that('importance: training on only a fraction of the variables'
          ,{
  # calculating importance depending on the model needs to use model specific
  # predict functions that require the original training dataframe. Sometimes
  # the dataframe must contain only the data described in the formula.

  #classification
  data_ls = f_clean_data(mtcars)

  form = as.formula('cyl~hp+disp')

  pl = pipelearner::pipelearner(data_ls$data) %>%
    pipelearner::learn_models( twidlr::rpart, form ) %>%
    pipelearner::learn_models( twidlr::randomForest, form ) %>%
    pipelearner::learn_models( twidlr::svm, form ) %>%
    pipelearner::learn() %>%
    mutate( imp = map2(fit, train, f_model_importance) )

  #regression

  pl = pipelearner::pipelearner(mtcars) %>%
    pipelearner::learn_models( twidlr::rpart, form ) %>%
    pipelearner::learn_models( twidlr::randomForest, form ) %>%
    pipelearner::learn_models( twidlr::svm, form ) %>%
    pipelearner::learn() %>%
    mutate( imp = map2(fit, train, f_model_importance) )
})

test_that('importance: return a value for each variable'
          ,{

  data_ls = f_clean_data(mtcars)

  m_randomForest = twidlr::randomForest(data_ls$data, cyl~.)
  m_svm = twidlr::svm(data_ls$data, cyl~.)
  m_rpart = twidlr::rpart(data_ls$data, cyl~.)


  expect_equal( nrow(f_model_importance_randomForest(m_randomForest))
                , nrow(f_model_importance_svm(m_svm, data_ls$data))
                )

  expect_equal( nrow(f_model_importance_rpart(m_rpart))
                , nrow(f_model_importance_svm(m_svm, data_ls$data))
               )

})

test_that('tabplot::tableplot important variables'
          ,{

  data = f_clean_data(mtcars) %>%
    .$data
  m = rpart::rpart( disp~., data)
  ranked_variables  = f_model_importance(m, data)
  response_var = 'disp'

  f_model_importance_plot_tableplot( data, ranked_variables, response_var, limit = 5, title = 'test')

})

test_that('f_model_pl_add_plots_regression, f_model_importance_pl_plots_as_html '
  ,{

    data_ls = f_clean_data(mtcars)
    form = disp~cyl+mpg+hp
    variable_color_code = f_plot_color_code_variables(data_ls)

    pl = pipelearner::pipelearner(data_ls$data) %>%
      pipelearner::learn_models( twidlr::rpart, form ) %>%
      pipelearner::learn_models( twidlr::randomForest, form ) %>%
      pipelearner::learn_models( twidlr::svm, form ) %>%
      pipelearner::learn() %>%
      mutate( imp = map2(fit, train, f_model_importance)
              , title = paste(model, models.id, train_p) ) %>%
      f_model_importance_pl_add_plots_regression(  data                  = train
                                                   , m                   = fit
                                                   , ranked_variables    = imp
                                                   , title               = title
                                                   , response_var        = target
                                                   , variable_color_code = variable_color_code
                                                   , formula             = form
                                                   , data_ls             = data_ls
                                                   , var_dep_limit       = 10
                                                   , var_dep_log_y       = T
                                                   , tabplot_limit       = 12) %>%
      f_model_importance_pl_plots_as_html( prefix = 'test_oetteR_html_')

      files = dir() %>%
        .[ startsWith(., 'test_oetteR_html_') ]

      file.remove( files )

  })


test_that('f_model_pl_add_plots_regression, formula in pl'
  ,{

    data_ls = f_clean_data(mtcars)
    form = disp~cyl+mpg+hp
    variable_color_code = f_plot_color_code_variables(data_ls)

    wr_lean_model = function( data, formula, .f ){

      pipelearner::learn_models( data, .f, formula )

    }

    tib = tibble( data = list( data_ls$data )
                  , formula = list(form) ) %>%
      mutate( pipe = map(data, pipelearner::pipelearner)
              , pipe = map2( pipe, formula, wr_lean_model, randomForest::randomForest )
              , pipe = map2( pipe, formula, wr_lean_model, e1071::svm )
              , pipe = map2( pipe, formula, wr_lean_model, rpart::rpart )
              , pipe = map( pipe, pipelearner::learn )
      ) %>%
      unnest( pipe, .drop = F ) %>%
      mutate( title = model
              , imp = map2(fit, train, f_model_importance) ) %>%
      f_model_importance_pl_add_plots_regression(  data                  = train
                                                   , m                   = fit
                                                   , ranked_variables    = imp
                                                   , title               = title
                                                   , formula             = formula
                                                   , response_var        = target
                                                   , variable_color_code = variable_color_code
                                                   , data_ls             = data_ls
                                                   , var_dep_limit       = 12
                                                   , var_dep_log_y       = T
                                                   , tabplot_limit       = 12
                                                   , formula_in_pl       = T
                                                   )
  })





