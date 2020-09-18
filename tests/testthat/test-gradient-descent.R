library(testthat)

context("Test the output of homework 1.")

test_that("Your gradient_descent() function works in an easy case.", {
  
  data(iris)
  
  fit_gradient_descent <- gradient_descent(Sepal.Length ~ Petal.Width, iris)
  
  fit_lm <- lm(Sepal.Length  ~ Petal.Width, iris)
  
  expect_equivalent(fit_lm$coefficients, fit_gradient_descent$coefficients,
                    tolerance = 1e-3)
})