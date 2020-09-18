#' @title Gradient descent
#' @description Fits linear model by gradient descent
#' @param form the input formula object
#' @param dat the input dataframe
#' @param gamma the learning rate
#' @param tol the numerical tolerance
#' @param iter the maximum iterations
#' @example 
#' fit <- gradient_descent(Sepal.Length ~ ., iris)
#' print(fit)
#' @export

gradient_descent <- function(form, dat, gamma=0.1, tol=1e-5, iter=100000){
  X <- model.matrix(form,dat)
  y <- model.frame(form,dat)[,1]
  N <- length(y)
  betas <- as.matrix(rnorm(n=dim(X)[2]))

  for (i in 1:iter){
    betas_tmp <- betas - (gamma * (1/N) * (t(X) %*% (X %*% betas - y)))
    error <- sqrt(sum((betas_tmp-betas)^2))
    betas <- betas_tmp
    if (is.na(betas)){
      print("Gradient descent failed.")
      break
    }
    if (error<=tol){
      break
    }
  }
  
  betas_name <- rownames(betas)
  betas <- as.numeric(betas)
  names(betas) <- betas_name
  
  res <- list(coefficients = betas)
  res
}
