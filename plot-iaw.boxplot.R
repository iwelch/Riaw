#' Enhanced Boxplot
#'
#' @name iaw.boxplot
#'
#' Boxplot with additional features.
#'
#' @param x Data for boxplot.
#' @param ... Boxplot arguments.
#'
#' @return Boxplot object.
#'
#' @family plotting
#' @export

iaw$iaw.boxplot <- function(x, ...) {
    boxplot(x, ...)
}
