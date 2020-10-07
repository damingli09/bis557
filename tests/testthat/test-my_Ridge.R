library(testthat)
library(MASS)

context("Test the output of ridge regression.")

test_that("You my_ridge() function handles collinearity well.", {

  n <- 500
  p <- 5
  beta <- c(1, 2,-1, 0,-2)
  X <- matrix(rnorm(n*p), nrow=n, ncol = p)
  X[,1] <- X[,1]*0.05 + X[,2]*0.95  # make 1st and 2nd columns collinear
  y <- X %*% beta + rnorm(n)

  fit_my_rdige <- my_Ridge(X, y, lambda=1.0)


  expect_equivalent(beta, fit_my_rdige$coefficients,
                    tolerance = 0.2)
})

