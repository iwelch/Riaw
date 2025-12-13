
#' COMPLETEOBS
#'
#' @name completeobs
#'
#' eliminate rows with an NA from a list
#'
#' @usage completeobs (object)
#'
#' @param an object
#'
#' @return object without NA rows
#'
#'

iaw$completeobs <- function (object) iaw$abort("please use na.omit or complete.cases instead") # (object[complete.cases(object), ])
