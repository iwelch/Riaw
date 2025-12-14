#' Extract Residuals
#'
#' @name residuals
#'
#' Extracts residuals from model.
#'
#' @param model Regression model.
#'
#' @return Residuals vector.
#'
#' @family regression
#' @export

iaw$residuals <- function(model) {
    residuals(model)
}
