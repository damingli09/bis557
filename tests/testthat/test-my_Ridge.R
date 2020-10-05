library(testthat)
library(MASS)

context("Test the output of ridge regression.")

test_that("You my_ridge() function handles collinearity well.", {

  n <- 500
  p <- 5
  beta <- c(1, 2,-1, 0,-2)
  X <- matrix(rnorm(n*p), nrow=n, ncol = p)
  X[,1] <- X[,1]*0.01 + X[,2]*0.99  # make 1st and 2nd columns collinear
  y <- X %*% beta + rnorm(n)

  fit_my_rdige <- my_Ridge(X, y, lambda=1.0)

  df <- data.frame(cbind(X,y))
  colnames(df)[1] = 'y'

  fit_lm_ridge <- lm.ridge(y~.-1,df,lambda=1.0)

  expect_equivalent(fit_lm_ridge$coef, fit_my_rdige$coefficients,
                    tolerance = 1e-2)
})

