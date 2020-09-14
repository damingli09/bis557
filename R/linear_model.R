#' @title Fits linear model
#' @description Fits linear model using built-in lm function
#' @param f the formula
#' @param df the dataframe
#' @param constrasts list of variables as input
#' @example 
#' fit <- linear_model(Sepal.Length ~ ., iris, contrasts = list(Species = "contr.sum"))
#' @export

linear_model <- function(f, df, constrasts=NULL) {
    lm(f, df, contrasts)
}
