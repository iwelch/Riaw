#' RBIND.MC.BY
#'
#' @name rbind.mc.by
#'
#' @usage rbind.mc.by (data, INDICES, FUN, ...)
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

iaw$rbind.mc.by <- function(indataframe, INDICES, FUNIN, ..., FAST=FALSE)  {
    result <- iaw$mc.by( indataframe, INDICES, FUNIN, ... )
    if (exists("DEBUG")) message("[rbind.mc.by: result of mc.by obtained]")

    if (FAST) t(simplify2array(result)) else as.data.frame( do.call("rbind", result) )
}
