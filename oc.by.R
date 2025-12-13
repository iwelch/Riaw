
#' OC.BY
#'
#' @name oc.by
#'
#' a by-hand faster implementation of by, using a single CPU core
#'
#' @usage oc.by (indata, INDICES, FUN, ...)
#'
#' @param dataframe the typical
#'
#' @return
#'
#' @seealso by
#' @seealso mc.by
#' @seealso rbind.oc.by
#'

iaw$oc.by <- function (indata, INDICES, FUN, ...)
    lapply(split(1:nrow(indata), INDICES),  FUN=function(.index) FUN(indata[.index, , drop = FALSE], ...))

