
#' CAT.STDERR
#'
#' @name cat.stderr
#'
#' mnemonic wrapper for cat(...,file=stderr())
#'
#' @usage cat.stderr("I am", x, "\n")
#'
#' @param ...
#'
#' @return
#'

iaw$cat.stderr <- function (...)
  cat(..., file = stderr())
