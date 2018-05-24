context('wrappers for modelling functions')

test_that('call container'
  ,{

  .f = randomForest::randomForest
  call_cont = make_container_for_function_calls()
  call_cont$set_total(4)

  expect_equal( call_cont$get_total(), 4)

  set.seed(1)
  m_wr = call_cont$make_call( .f = .f, formula = disp~., data = mtcars )

  set.seed(1)
  m_wr2 = call_cont$make_call( .f = .f, formula = mpg~., data = mtcars )

  set.seed(1)
  m_wr3 = call_cont$make_call( .f = .f, print_call = T, formula = mpg~., data = mtcars )

  call_cont$get_calls()

  set.seed(1)
  m_or = .f( disp~., mtcars )

  m_wr$call = NULL
  m_or$call = NULL

  expect_identical(m_wr, m_or)

})


test_that('test call container with pipelearner'
  ,{

  call_cont = make_container_for_function_calls()
  call_cont$set_total(10)

  suppressWarnings({

    pl = pipelearner::pipelearner(mtcars) %>%
      pipelearner::learn_models( models = c( call_cont$make_call )
                                 , formulas = c(disp~.)
                                 , .f = c( randomForest::randomForest )
                                 , function_name = 'randomForest'
                                  ) %>%
      pipelearner::learn_models( models = c( call_cont$make_call )
                                 , formulas = c(disp~.)
                                 , .f = c( rpart::rpart )
                                 , function_name = 'rpart'
      ) %>%
      pipelearner::learn_cvpairs( pipelearner::crossv_kfold, k = 5 ) %>%
      pipelearner::learn()

  })

  function_names = pl %>%
    mutate( function_names = map_chr(params, 'function_name') ) %>%
    .$function_names

  expect_identical( sort(function_names), sort(call_cont$get_function_names() ) )

})

test_that('lasso',{

  data = MASS::quine
  data$zerovar = 1

  formula = Days~Eth+Sex+Age+Lrn+zerovar

  lasso = f_train_lasso(data, formula, p = NULL, k = 3
                        , lambda = 10^seq(3,-3,length= 25) )

  lasso = f_train_lasso(data, formula, p = 1.5, k = 3
                        , lambda = 10^seq(3,-3,length= 25) )

  lasso = f_train_lasso(data, formula, p = NULL, k = 3
                        , lambda = 10^seq(3,-3,length= 25) )

  expect_true( ncol(lasso$plot_mse$data) == 3 )
  expect_true( ncol(lasso$plot_coef$data) == 4 )


  lasso
})

test_that( 'lasso classification',{

  data_ls = mtcars %>%
    f_clean_data()

  formula = vs ~ cyl + mpg + disp + hp + drat + wt + qsec + am + gear + carb

  trans_ls = f_manip_data_2_model_matrix_format( data_ls$data, formula )

  set.seed(1)

  lasso = f_train_lasso( trans_ls$data
                         , trans_ls$formula
                         , p = NULL
                         , family = 'binomial'
                         , k = 3
                         )

  lasso

  # if the test set is too small auc could get heigher then 1 for some reason
  # the f_train_lasso catches that

  expect_error({

    lasso = f_train_lasso( trans_ls$data
                           , trans_ls$formula
                           , p = NULL
                           , family = 'binomial'
                           , k = 10
    )

  })


  expect_true( max(lasso$tib_all$auc) <= 1 )


})




