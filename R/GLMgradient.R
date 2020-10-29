#' @title GLM Gradient descent with constant step
#' @description Fits generalized linear model by gradient descent, maximizing log-likelihood
#' @param X the design matrix
#' @param y the response variable
#' @param mu_fun function from eta to the expected value
#' @param T_fun the sufficient statistic as a function of y
#' @param lrate the learning rate
#' @param maxiter the maximum iteration number
#' @param tol the numerical tolerance
#' @export

GLMgradient <- function(X, y, mu_fun, T_fun, lrate=0.01, maxiter=10000, tol=1e-5){
  coefs <- matrix(rep(0, ncol(X)))
  coefsOld <- coefs

  coefErr <- 1
  counter <- 0
  while(coefErr > tol && counter < maxiter)
  {
    tmp <- lrate*matrix(t(X) %*% (T_fun(y)-mu_fun(X%*%coefsOld)))/nrow(X)
    coefs <- coefsOld + tmp
    coefErr <- sqrt(t(tmp) %*% tmp)
    coefsOld <- coefs
    counter <- counter + 1
  }

  coefs
}
