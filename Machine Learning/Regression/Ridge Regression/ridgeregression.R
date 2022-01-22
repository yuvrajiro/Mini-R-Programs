#' ---
#' title: "Ridge Regression in R"
#' author: "Rahul Goswami"
#' ---


#' Ridge  Regression is a statistical method for estimating the parameters of a linear model, where the independent variables
#' are highly correlated, It is a way to reduce the variance of the estimates by imposing a penalty on the absolute value of the coefficients.
#' the penalty decides the amount of regularization, In ridge penalty is added to the sum of squares of the coefficients 

#' In genral we try to reduce 
#' $$RSS = \sum_{i=1}^n (y_i - \hat{y}_i)^2$$  
#' but in Ridge Regression we add a penalty to the sum of squares of the coefficients
#' $$\sum_{i=1}^n (y_i - \hat{y}_i)^2 + \lambda \sum_{j=1}^p b_j^2$$
#' where $\lambda$ is the penalty parameter, and the whole second term is known as Shrinkage.  



#' ### Ridge Regression
#' $$ b = (X^T X + \lambda I)^{-1} X^T y $$

#' Dataset
nobs <- 100 # number of observations
ntrain <- 0.8*nobs # number of training observations
x1 <- runif(nobs) # independent variable
x2 <- x1 + rnorm(nobs,0,0.2) # independent variable
y <- 4 + 3*x1 + 2*x2 + rnorm(nobs) # dependent variable
X = matrix(c(rep(1,nobs),x1,x2),ncol=3) # design matrix
train.X <- X[1:ntrain,] # training design matrix
train.y <- y[1:ntrain] # training dependent variable
test.X <- X[(ntrain+1):nobs,] # test design matrix
test.y <- y[(ntrain+1):nobs] # test dependent variable

for(lambda in seq(0,4,0.1)){
  #' Ridge Regression
  b = solve(t(train.X) %*% train.X + lambda*diag(1,3,3), t(train.X) %*% train.y)
  #' b using OLS
  b.ols <- solve(t(train.X) %*% train.X, t(train.X) %*% train.y)
  #' Prediction
  pred.y <- test.X %*% b
  #' Prediction using OLS
  pred.ols <- test.X %*% b.ols
  #' Residuals
  resid.y <- test.y - pred.y
  #' Residuals using OLS
  resid.ols <- test.y - pred.ols
  #' RSS
  RSS <- sum(resid.y^2)
  #' RSS using OLS
  RSS.ols <- sum(resid.ols^2)
  #' R-squared
  R2 <- 1 - RSS/sum((test.y - mean(test.y))^2)
  #' R-squared using OLS
  R2.ols <- 1 - RSS.ols/sum((test.y - mean(test.y))^2)
  cat("lambda = ",lambda,", RSS = ",RSS,", R2 = ",R2,"\n")
  cat("lambda = ",lambda,", RSS = ",RSS.ols,", R2 = ",R2.ols,"\n","\n")
} 









