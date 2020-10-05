#' @title Gradient descent based on out of sample accuracy
#' @description Fits linear model by gradient descent optimizing out of sample accuracy
#' @param form the input formula object
#' @param dat the input dataframe
#' @param gamma the learning rate
#' @param tol the numerical tolerance
#' @param iter the maximum iterations
#' @example
#' fit <- gradient_descent_outofsample(Sepal.Length ~ ., iris)
#' print(fit)
#' @export

gradient_descent_outofsample <- function(form, dat, gamma=0.1, tol=1e-3, iter=100000){
  X <- model.matrix(form,dat)
  y <- model.frame(form,dat)[,1]

  train_smp_size <- floor(0.75 * nrow(X))
  set.seed(2020)
  train_ind <- sample(seq_len(nrow(X)), size = train_smp_size)

  Xtrain <- X[train_ind, ]
  Xtest <- X[-train_ind, ]
  ytrain <- y[train_ind]
  ytest <- y[-train_ind]

  N <- length(ytrain)
  betas <- as.matrix(rnorm(n=dim(X)[2]))

  for (i in 1:iter){
    betas_tmp <- betas - (gamma * (1/N) * (t(Xtrain) %*% (Xtrain %*% betas - ytrain)))
    error <- sqrt(sum((Xtest%*%betas_tmp - ytest)^2))/N
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
