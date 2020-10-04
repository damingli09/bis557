#' @title Selection of optimal penalty strength Ridge regression model
#' @description Selection of optimal penalty strength for Ridge regression model using n-fold cross validation
#' @param X the data design matrix
#' @param y the response vector
#' @param lambdas an array of penalty strengths
#' @param n number of folds in cross validation
#' @example
#' lambda_optim <- optimal_lambda(X, y, lambda=seq(0.1,10,by=0.5), n=5)
#' print(lambda_optim)
#' @export

optimal_lambda <- function(X, y, lambdas, n){
  folds <- rep(1:n,length.out = length(y))
  fold <- sample(folds)  # randomize

  errors <- matrix(0,length(lambdas),n)  # holds the validation errors for each lambda and fold

  l <- length(lambdas)
  for (k in 1:l){
    for (i in 1:n){
      X_train <- X[which(fold != i),]
      X_test <- X[which(fold == i),]
      y_train <- y[which(fold != i),]
      y_test <- y[which(fold == i),]

      beta_hat <- my_Ridge(X_train,y_train,lambdas[k])
      y_hat <- X_test %*% beta_hat$coefficients

      errors[k,i] <- apply((y_hat-y_test)^2, 2, mean)
    }
  }

  mse <- rowMeans(errors)
  lambdas[which.min(mse)]
}

