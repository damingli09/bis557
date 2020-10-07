library(testthat)

context("Test the output of homework 2 question 2.")

test_that("Your gradient_descent_outofsample() function works in an easy case.", {

  data(iris)

  fit_gradient_descent_outofsample <- gradient_descent_outofsample(Sepal.Length ~ Petal.Width, iris)

  fit_lm <- lm(Sepal.Length  ~ Petal.Width, iris)

  expect_equivalent(fit_lm$coefficients, fit_gradient_descent_outofsample$coefficients,
                    tolerance = 1e-1)
})
