#' @title Softmax regression with gradient descent
#' @description Fits softmax regression using gradient descent, by maximizing log-likelihood
#' @param X the design matrix
#' @param y the response variable
#' @param lrate the learning rate
#' @param maxiter the maximum iteration number
#' @param tol the numerical tolerance
#' @export

softmaxreg <- function(X, y, lrate=0.01, maxiter=10000, tol=1e-5){
  y <- onehot(y)
  p <- ncol(X)
  K <- ncol(y)
  coefs <- matrix(rep(0, p*K), ncol=K, byrow=TRUE)
  coefsOld <- coefs

  coefErr <- 1
  counter <- 0
  while(coefErr > tol && counter < maxiter)
  {
    tmp <- lrate*matrix(t(X) %*% (y-softmax(X%*%coefsOld)), ncol=K)/nrow(X)
    coefs <- coefsOld + tmp
    coefErr <- norm(coefs - coefsOld)
    coefsOld <- coefs
    counter <- counter + 1
  }

  coefs
}

softmax <- function(z) {
  tmp <- exp(z)
  tmp/colSums(tmp)
}

onehot <- function(Y){
  n_col <- length(unique(Y))
  n_row <- length(Y)
  Y_enc <- matrix(rep(0, n_col*n_row), ncol = n_col, byrow=TRUE)
  for (i in 1:n_row) {
    Y_enc[i, Y[i]] <- 1
  }
  Y_enc
}
