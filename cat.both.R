
#' CAT.BOTH
#'
#' @name cat.both
#'
#'  mnemonic wrapper for cat(...,file=stderr()); cat(...)
#'
#' @usage cat.both("I am", x, "\n")
#'
#' @param ... anything
#'
#'  @return
#'


iaw$cat.both <- function (...)
{
  cat(..., file = stderr())
  cat(...)
}
