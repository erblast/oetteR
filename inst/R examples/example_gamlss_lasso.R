
# mtcars --------------------------------------------------
data = mtcars

data_ls = f_clean_data(data) %>%
  f_boxcox()

data_resp_var = data_ls$data %>%
  dplyr::select(disp)


data_expl_var = data_ls$boxcox_data %>%
  dplyr::select( - disp_boxcox ) %>%
  scale() %>%
  as_tibble()

data_new = data_resp_var %>%
  bind_cols(data_expl_var)


m = gamlss(disp~ri(data_expl_var, Lp = 1), data = data_new )

zapsmall(coef(getSmo(m)), digits=3)

#Hitters --------------------------------------------------
data = Hitters[complete.cases(Hitters),]

x = model.matrix(Salary~., data = d)[,-1] # remove the intercept value
y = d$Salary

x = model.matrix(Salary~., data = d)[,-1] # remove the intercept value
y = d$Salary

grid = 10^seq(10,-2,length= 100)

data_new = cbind(y,x) %>%
  as_tibble() %>%
  rename( Salary = y)

#lambda gets estimated
m = gamlss( Salary~ri( dplyr::select(data_new, - Salary), Lp = 1), data = data_new )
zapsmall(coef(getSmo(m)), digits=3)




#verify by cv

wr_learn = function(formula, data, ...){

  data = as.data.frame(data)



  m = gamlss::gamlss( Salary~ri(dplyr::select(data, - Salary), Lp = 1, ... ), data = data  )

}


pl = pipelearner::pipelearner(data_new) %>%
  pipelearner::learn_models( models = c(wr_learn)
                             , formulas = Salary~.
                             , lambda = grid
                             ) %>%
  pipelearner::learn_cvpairs( pipelearner::crossv_kfold, 5 ) %>%
  pipelearner::learn()

pl = pl %>%
  mutate( preds = pmap( list(test, fit, target), f_predict_regression_add_predictions ) )

test = pl$test[[1]] %>%
  as.data.frame()

train = pl$train[[1]] %>%
  as.data.frame()

fit  = pl$fit[[1]]
target = 'Salary'

f_predict_regression_add_predictions(data, m, target)

predict(m)

