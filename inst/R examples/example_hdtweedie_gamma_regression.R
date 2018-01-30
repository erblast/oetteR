


library('HDtweedie')
library('MASS')

data = quine

x = model.matrix(Days~., data )[,-1] #-1 to remove intercept
y = data$Days

cv_fit <- cv.HDtweedie(x,y, standardize = T, p = 1.5)

cv_fit$lambda.min

coef(cv_fit, s = "lambda.min")

plot(cv_fit)


# pipe ---------------------------------------------------------

formula = Days~.

grid = 10^seq(10,-2, length = 100)

wr_HDtweedie = function(data, formula, lambda, p ){

  response_var = f_manip_get_response_variable_from_formula(formula)

  y = data[[response_var]]
  x = model.matrix( formula, data )

  m = HDtweedie::HDtweedie( x, y, lambda = lambda, p = p )

}


pl = pipelearner::pipelearner(quine) %>%
  pipelearner::learn_models( models = c(wr_HDtweedie)
                             , formulas = c(formula)
                             , lambda = grid
                             #, p = c( 2 ,0, 1.5 )
                             ) %>%
  pipelearner::lea

