

data = tibble( resp = rgamma(500, shape = 1)
               , pred1 = rnorm(500, 100, 1.8) * -resp
               , pred2 = rnorm(500, mean = 6, sd = 0.5) * resp
               , pred3 = rnorm(500, 2, 1)
)


x = model.matrix( resp~., data)[,-1]
x = scale( center = T)
y = data[['resp']]


m = glmnet::cv.glmnet(x,y,alpha = 1)
predict(m, type = 'coefficients', lambda = m$lambda.min)

m = HDtweedie::cv.HDtweedie(x,y, standardize = T, p = 1.9, alpha = 1)
plot(m)
coef(m, s = "lambda.min")

lambda_min = m$lambda.min

m = HDtweedie::HDtweedie(x,y, p = 2, alpha = 1)
plot(m)
