
#' LAGDATAFRAME
#'
#' @name lagdataframe
#'
#'   lag all rows in a data frame
#'
#' @usage lagdataframe (dataframein, numlags = 1, panelid = NULL, timeid= NULL)
#'
#' @param dfin an input data frame
#'
#' @return an output data frame
#'
#' @seealso lagseries
#'


iaw$lagdataframe <- function (dataframein, numlags = 1, panelid = NULL, timeid = NULL) {
   stop("This needs to be checked / written better")

   for (i in 1:ncol(dataframein))
      dataframein[, i] <- iaw$lagseries(dataframein[, i], numlags, panelid, timeid)
    names(dataframein) <- paste0("L", names(dataframein))
    dataframein
}
