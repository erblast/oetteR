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

  set.seed(1)
  m_wr4 = call_cont$make_call( .f = .f, print_model_call = T, formula = mpg~., data = mtcars )

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
  call_cont$set_total(5)


  pl = pipelearner::pipelearner(mtcars) %>%
    pipelearner::learn_models( models = c( call_cont$make_call )
                               , formulas = c(disp~.)
                               , .f = c( randomForest::randomForest )
                               , print_call = c(T)
                                ) %>%
    pipelearner::learn_cvpairs( pipelearner::crossv_kfold, k = 5 ) %>%
    pipelearner::learn()

})
