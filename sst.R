
#' SST
#'
#' @name sst
#'
#'  the sum-squared-total
#'
#' @usage sst (model)
#'
#' @param model a (usually lm) model
#'
#' @return the sum-squared total
#'

iaw$sst <- function (model) {
    y <- model$model[[1]]  # Get response variable
    sum((y - mean(y))^2)
}

## old one: iaw$sst <- function (model) sum(fitted(model)^2) + iaw$sse(model)
## better: iaw$sst <- function (model) sum((fitted(model) - mean(fitted(model)))^2) + iaw$sse(model)
