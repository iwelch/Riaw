#'
#' RBIND.OC.BY
#'
#' @name rbind.oc.by
#'
#' @usage rbind.oc.by (data, INDICES, FUN, ...)
#'
#' @param dataframe the input data frame
#'
#' @return a matrix that can be coerced into a data frame
#'
#' @seealso by
#' @seealso oc.by
#' @seealso mc.by
#' @seealso rbind.oc.by
#'

iaw$rbind.oc.by <- function(indataframe, INDICES, FUNIN, ..., FAST=FALSE) {
    result <- iaw$oc.by( indataframe, INDICES, FUNIN, ... )
    if (exists("DEBUG")) message("[rbind.oc.by: result of oc.by obtained]")

    if (FAST) t(simplify2array(result)) else as.data.frame( do.call("rbind", result) )
}
