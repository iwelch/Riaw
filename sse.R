
#' SSE
#'
#' @name sse
#'
#'   the sum-squared-error
#'
#' @usage sse (model)
#'
#' @param model a (usually lm) model
#'
#' @return
#'

iaw$sse <- function (model) sum(resid(model)^2)
