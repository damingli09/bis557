#' @title Fits Ridge regression model
#' @description Implements Ridge regression model by SVD
#' @param X the data design matrix
#' @param y the response vector
#' @param lambda the penalty strength
#' @example
#' fit <- my_Ridge(X, y, lambda=1.0)
#' print(fit)
#' @export

my_Ridge <- function(X, y, lambda){
  svd_obj <- svd(X)
  U <- svd_obj$u
  V <- svd_obj$v
  svals <- svd_obj$d

  D <- diag(svals/(svals^2+lambda))
  ridge_beta <- V %*% D %*% t(U) %*% y

  res <- list(coefficients = ridge_beta, lambda = lambda)
  res
}
