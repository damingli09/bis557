
#' @title GLM Gradient descent with momentum
#' @description Fits generalized linear model by gradient descent, maximizing log-likelihood
#' @param X the design matrix
#' @param y the response variable
#' @param mu_fun function from eta to the expected value
#' @param T_fun the sufficient statistic as a function of y
#' @param lrate the learning rate
#' @param mom the momentum parameter
#' @param maxiter the maximum iteration number
#' @param tol the numerical tolerance
#' @export

GLMgradientMomentum <- function(X, y, mu_fun, T_fun, lrate=0.01, mom=0.9, maxiter=10000, tol=1e-5){
  coefs <- matrix(rep(0, ncol(X)))
  coefsOld <- coefs
  v <- matrix(rep(0, ncol(X)))
  vOld <- v

  coefErr <- 1
  counter <- 0
  while(coefErr > tol && counter < maxiter)
  {
    tmp <- lrate*matrix(t(X) %*% (T_fun(y)-mu_fun(X%*%coefsOld)))/nrow(X)
    v <- mom*vOld + tmp
    coefs <- coefsOld + v
    coefErr <- sqrt(t(v) %*% v)
    coefsOld <- coefs
    vOld <- v
    counter <- counter + 1
  }

  coefs
}
