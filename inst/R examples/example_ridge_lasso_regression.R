
library(glmnet)
library(ISLR)

d = Hitters[complete.cases(Hitters),]

#the glmnet function doe snot use the normal formula syntax but instead
#requires a defined x and y vector

x = model.matrix(Salary~., data = d)[,-1] # remove the intercept value
y = d$Salary

grid = 10^seq(10,-2,length= 100)
m = glmnet(x,y, alpha = 0, lambda = grid, standardize = TRUE)
                                          #alpha = 0 --> ridge regression
                                          #alpha = 1 --> lasso

plot(m)

dim(coef(m)) # returns a set of coefficients for each lambda grid value

# the higher lambda gets the lower get the normalised coefficients l2
lambda = m$lambda[50] # 11498
l2 = sqrt( sum( coef(m)[-1,50]^2) ) # 6.3


lambda = m$lambda[60] # 705
l2 = sqrt( sum( coef(m)[-1,60]^2) ) # 57.1

# the predict function can be used to predict the coefficients for an
# arbitrary value lambda

predict(m, s = 50, type ='coefficients')

# perform ridge regression on training data
set.seed(1)
train = sample( 1:nrow(x), nrow(x)/2 )
test=(-train)
y_test = y[test]

m =glmnet(x[train,], y[train], alpha = 0, lambda = grid,
          thresh = 1e-12) # convergence threshold, default = 1e-7

#here the predict function produces predict values for the test data
pred = predict(m, s=4, newx =x[test,])
mean(( pred - y_test)^2) #101036

# mse for a model with just an intercept
mean( (mean(y[train])-y_test)^2 ) # 193253

# this can be reproduced by setting lambda to a high value
pred = predict(m, s=1e10, newx =x[test,])
mean(( pred - y_test)^2) #193253

# compared to least sqaure regression. Note least squares regression
# is simply ridge regression with lambda = 0

pred = predict(m, s = 0, newx = x[test,],
               exact = T) # exact value is needed to prevent predict
                          # from interpolating over all lambda
                          # values used in fitting

mean( (pred-y_test)^2 ) # 114783.1

#picking the best lambda value by cross validation

set.seed(1)
m_cv = cv.glmnet(x[train,], y[train], alpha = 0)
plot(m_cv)

best_lambda = m_cv$lambda.min # 211.74

pred = predict( m_cv, s = best_lambda, newx = x[test,])
mean( (pred-y_test)^2 ) # 9582.96
sqrt( mean( (pred-y_test)^2 ) ) # 309.81 $

# get coefficients for best ridge model

m_ridge = glmnet(x,y, alpha = 0)

predict(m_ridge, type = 'coefficients', s = best_lambda)


# The lasso ------------------------------------------

m = glmnet( x[train,], y[train], alpha=1, lambda = grid)
plot(m)

set.seed(1)

m_cv = cv.glmnet(x[train,], y[train], alpha =1, lambda = grid)
plot(m_cv)
best_lambda = m_cv$lambda.min
pred = predict(m_cv, s = best_lambda, newx = x[test,])
mean( (pred-y_test)^2 ) # 101996.8
sqrt(  mean( (pred-y_test)^2 ) ) #317.58$

m_lasso = glmnet(x,y,alpha=1 ,lambda=grid)
predict(m_lasso, type = 'coefficients', s = best_lambda )

# conclusion
# by using the lasso we increase the error by 8$ but we get an
# easier to interpret model with fewer variables
# compared to cross validation and best subsetselection both ridge regression
# and lasso perform better when comparing pessimistic errors

