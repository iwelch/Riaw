
#' DONE
#'
#' @name done
#'
#' a pleasant abort function --- stops all sinks
#'
#' @usage done ()
#'
#' @return
#'


iaw$done <- function () {
  if (!interactive()) q()
  while (sink.number>=1) sink()
  .Internal(stop(as.logical(TRUE), "The program has ended nicely."))
  cat("Executed done!\n")
}
