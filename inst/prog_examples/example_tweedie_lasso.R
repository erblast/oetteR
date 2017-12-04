
require(oetteR)
require(pipelearner)

data = MASS::quine

formula = Days~.

grid = 10^seq(10,-2,length= 100)

call_cont = make_container_for_function_calls()
call_cont$set_total(1500)

wr_tweedie = function(data, formula, lambda, p_fact ){

  response_var = f_manip_get_response_variable_from_formula(formula)

  y = data[[response_var]]
  x = model.matrix(formula, data)[,-1]

  m = HDtweedie::HDtweedie(x,y, lambda = lambda, p = p_fact )

}


pl = pipelearner(data) %>%
  learn_models(models = c(call_cont$make_call)
               , formulas = c(formula)
               , .f = c(wr_tweedie)
               , function_name = c('tweedie')
               , lambda = grid
               , p_fact = c(0,1.5,2)
               ) %>%
  learn_cvpairs( crossv_kfold, 5 ) %>%
  learn()

pl = pl %>%
  f_predict_pl_regression()


