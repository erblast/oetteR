
require(oetteR)
require(pipelearner)
require(tidyverse)


data = ggplot2::diamonds %>%
  select(-color, -clarity, -cut, -x,-y,-z) %>%
  mutate_if( is.double, scale, center = T )

formula = price~.

grid = 10^seq(10,-4,length= 10)

call_cont = make_container_for_function_calls()
call_cont$set_total(600)

wr_tweedie = function(data, formula, lambda, p_fact ){

  response_var = f_manip_get_response_variable_from_formula(formula)

  y = data[[response_var]]
  x = model.matrix(formula, data)[,-1]

  m = HDtweedie::HDtweedie(x,y, lambda = lambda, p = p_fact )

}

wr_glmnet = function(data, formula, lambda ){

  response_var = f_manip_get_response_variable_from_formula(formula)

  y = data[[response_var]]
  x = model.matrix(formula, data)[,-1]

  m =glmnet::glmnet(x,y, lambda = lambda, alpha = 1 )

}


pl = pipelearner(data) %>%
  learn_models(models = c(call_cont$make_call)
               , formulas = c(formula)
               , .f = c(wr_tweedie)
               , function_name = c('tweedie')
               , lambda = grid
               , p_fact = seq(1,2,0.1)
               ) %>%
  learn_models(models = c(call_cont$make_call)
               , formulas = c(formula)
               , .f = c(wr_glmnet)
               , function_name = c('glmnet')
               , lambda = grid
  ) %>%
  learn_cvpairs( crossv_kfold, 5 ) %>%
  #learn_cvpairs( crossv_mc, n = 1, test = 0.01 ) %>%
  learn()

pl = pl %>%
  mutate(  lambda = map_dbl(params, 'lambda')
          , function_name = map_chr(params, 'function_name')
          )

pl_glm = pl %>%
  filter( function_name == 'glmnet') %>%
  mutate( p = -1
          , coef = map(fit, coef )
          , coef = map(coef, as.matrix )
          , coef = map(coef, as.data.frame )
          , coef = map(coef, function(x) mutate(x, coef = row.names(x))  )
  )

pl_tweedie = pl %>%
  filter( function_name == 'tweedie') %>%
  mutate( p = map_dbl(params, 'p_fact')
          , coef = map(fit, coef )
          , coef = map(coef, as.matrix )
          , coef = map(coef, as.data.frame )
          , coef = map(coef, function(x) mutate(x, coef = row.names(x))  )
  )

pl_all = pl_glm %>%
  bind_rows(pl_tweedie) %>%
  mutate( title = paste(function_name, lambda, p))

pl_pred = pl_all %>%
  f_predict_pl_regression( formula = formula)%>%
  unnest(preds, .drop = F)

pl_lab = pl_pred %>%
  group_by(title, lambda, p, models.id) %>%
  summarize()

pl_sum = pl_pred %>%
  f_predict_pl_regression_summarize() %>%
  left_join( pl_lab )

p = ggplot(pl_sum, aes( log(lambda)
                    , rtmse
                    , fill = as.factor(p)
                    , color = as.factor(p)
                    )
       )+
  geom_line() +
  geom_point() +
  scale_fill_manual( values = f_plot_col_vector74() )+
  scale_color_manual( values = f_plot_col_vector74())


plotly::ggplotly(p)




p_coef = pl_all %>%
  unnest(coef, .drop = F ) %>%
  group_by(coef, p, lambda) %>%
  summarize( s0 = mean(s0) )


p_coef %>%
  filter( p == -1 ) %>%
  ggplot( aes(x = log(lambda), y = s0, color = coef) ) +
  geom_line()+
  geom_point()

p_coef %>%
  filter( p == 1.1 ) %>%
  ggplot( aes(x = log(lambda), y = s0, color = coef) ) +
  geom_line()+
  geom_point()

p_coef %>%
  filter( p == 1.2 ) %>%
  ggplot( aes(x = log(lambda), y = s0, color = coef) ) +
  geom_line()+
  geom_point()

