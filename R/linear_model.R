#' @title Fits linear model
#' @description Fits linear model by solving normal equation
#' @param form a formula object
#' @param dat a dataframe
#' @param constrasts list of variables as input
#' @example 
#' fit <- linear_model(Sepal.Length ~ ., iris, contrasts = list(Species = "contr.sum"))
#' print(fit)
#' @export

linear_model <- function(form, dat, contrasts=NULL){
    if (! is.null(contrasts)){
      contrasts(dat[,names(contrasts)]) <- contrasts[[names(contrasts)]]
    }
  
    X <- model.matrix(form,dat)
    y_name <- as.character(form)[2]
    y <- matrix(model.frame(form,dat)[,y_name], ncol=1)
    
    #betas <- solve(t(X)%*%X)%*%t(X)%*%y
    betas <- solve.qr(qr(X), y)
    betas[which(betas==0)] = NA
    betas_name <- rownames(betas)
    betas <- as.numeric(betas)
    names(betas) <- betas_name
    
    res <- list(coefficients = betas)
    #lass(res) <- "my_lm"
    res
}
