
#' SPRINT.DATA.FRAME
#'
#' @name sprint.data.frame
#'
#'  convert a data frame into a printable object that message or cat can use
#'
#' @usage sprint.data.frame(d)
#'
#' @param d a data frame
#'
#' @return a very long strong
#'


iaw$sprint.data.frame <- function (d) paste0(capture.output(d, "\n"))
